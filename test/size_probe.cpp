/// MIT License
///
/// Copyright (c) 2026 koniarik
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy
/// of this software and associated documentation files (the "Software"), to deal
/// in the Software without restriction, including without limitation the rights
/// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
/// copies of the Software, and to permit persons to whom the Software is
/// furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in all
/// copies or substantial portions of the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
/// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
/// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
/// SOFTWARE.
/// Comprehensive code-size probe for ecor on 32-bit ARM Cortex-M.
///
/// Design:
///   - Every pattern lives in its own [[gnu::noinline]] function so nm can
///     attribute bytes precisely.  Without noinline the compiler merges
///     everything into ecor_size_probe() and you see only one symbol.
///   - All sources are global data (no construction cost in probe functions).
///   - All receivers and coroutines must be at file scope per ecor guidelines.
///   - A global volatile int g_sink prevents the optimizer from removing
///     computations whose results are otherwise unused.
///   - Use only embedded-friendly types: no std::string, no heap via ::new.
///
/// Sections:
///   A - Sources          bare connect/start/fire for each source type/sig combo
///   B - Adapters         then, operator||, as_variant, err_to_val, sink_err
///   C - Tasks            trivial coroutines through chained coroutines
///   D - task_holder      restart loop + cooperative stop
///   E - Stop machinery   inplace_stop_source / token / callback
///   F - Memory           circular_buffer_memory alloc/dealloc cycle

#include "ecor/ecor.hpp"

#include <cstdint>
#include <optional>
#include <variant>

// ── volatile result sink ─────────────────────────────────────────────────────
int volatile g_sink = 0;

// ── shared task memory ───────────────────────────────────────────────────────
static uint8_t                                                                      g_raw[2048];
static ecor::circular_buffer_memory< ecor::smallest_index_type< sizeof( g_raw ) > > g_mem{ g_raw };

// ── domain types ─────────────────────────────────────────────────────────────
// Realistic embedded error codes — kept as plain enums to avoid heap allocation.
enum class uart_err : uint8_t
{
        framing = 1,
        overrun = 2,
        parity  = 3
};
enum class spi_err : uint8_t
{
        timeout   = 1,
        collision = 2
};

// ── custom task configuration ─────────────────────────────────────────────────
// Models a driver task that may yield uart_err in addition to the default
// task_error.  The extra error signature is propagated to the receiver.
struct uart_task_cfg
{
        using extra_error_signatures = ecor::completion_signatures< ecor::set_error_t( uart_err ) >;
        using trace_type             = ecor::task_default_trace;
};

// ─────────────────────────────────────────────────────────────────────────────
// Section A global sources
// ─────────────────────────────────────────────────────────────────────────────

// broadcast variants
static ecor::broadcast_source< ecor::set_value_t() >                        g_bcast_void;
static ecor::broadcast_source< ecor::set_value_t( uint8_t ) >               g_bcast_u8;
static ecor::broadcast_source< ecor::set_value_t( int ) >                   g_bcast_int;
static ecor::broadcast_source< ecor::set_value_t(), ecor::set_stopped_t() > g_bcast_stoppable;
static ecor::broadcast_source< ecor::set_error_t( uart_err ) >              g_bcast_err;
// second void broadcast for operator|| racing
static ecor::broadcast_source< ecor::set_value_t() > g_bcast_void2;

// FIFO variants
static ecor::fifo_source< ecor::set_value_t() >                                g_fifo_void;
static ecor::fifo_source< ecor::set_value_t( uint8_t ) >                       g_fifo_u8;
static ecor::fifo_source< ecor::set_value_t(), ecor::set_error_t( uart_err ) > g_fifo_err;

// Sequenced (skew-heap) variants — two key widths to quantify that cost
static ecor::seq_source< uint32_t, ecor::set_value_t() > g_seq_u32;
static ecor::seq_source< uint16_t, ecor::set_value_t() > g_seq_u16;

// ─────────────────────────────────────────────────────────────────────────────
// Receivers  (must be at file scope — templates not allowed in local classes)
// ─────────────────────────────────────────────────────────────────────────────

// Generic void receiver: handles set_value(), set_error(*), set_stopped().
// Suitable for direct connection to all void-valued senders.
struct R_void
{
        using receiver_concept = ecor::receiver_t;
        void set_value() noexcept
        {
                g_sink = 1;
        }
        void set_error( uart_err e ) noexcept
        {
                g_sink = (int) e;
        }
        void set_error( spi_err e ) noexcept
        {
                g_sink = (int) e;
        }
        void set_error( ecor::task_error e ) noexcept
        {
                g_sink = (int) e;
        }
        void set_stopped() noexcept
        {
                g_sink = -1;
        }
        [[nodiscard]] ecor::empty_env get_env() const noexcept
        {
                return {};
        }
};

// Receiver for set_value(uint8_t)
struct R_u8
{
        using receiver_concept = ecor::receiver_t;
        void set_value( uint8_t v ) noexcept
        {
                g_sink = v;
        }
        void set_error( uart_err e ) noexcept
        {
                g_sink = (int) e;
        }
        void set_stopped() noexcept
        {
                g_sink = -1;
        }
        [[nodiscard]] ecor::empty_env get_env() const noexcept
        {
                return {};
        }
};

// Receiver for set_value(int)
struct R_int
{
        using receiver_concept = ecor::receiver_t;
        void set_value( int v ) noexcept
        {
                g_sink = v;
        }
        void set_error( ecor::task_error e ) noexcept
        {
                g_sink = (int) e;
        }
        void set_stopped() noexcept
        {
                g_sink = -1;
        }
        [[nodiscard]] ecor::empty_env get_env() const noexcept
        {
                return {};
        }
};

// Receiver for task<void, uart_task_cfg>: handles the extra uart_err signal
struct R_task_uart
{
        using receiver_concept = ecor::receiver_t;
        void set_value() noexcept
        {
        }
        void set_error( ecor::task_error e ) noexcept
        {
                g_sink = (int) e;
        }
        void set_error( uart_err e ) noexcept
        {
                g_sink = (int) e;
        }
        void set_stopped() noexcept
        {
        }
        [[nodiscard]] ecor::empty_env get_env() const noexcept
        {
                return {};
        }
};

// Receiver for err_to_val: g_bcast_err | err_to_val  →  set_value(uart_err)
struct R_err_to_val
{
        using receiver_concept = ecor::receiver_t;
        void set_value( uart_err e ) noexcept
        {
                g_sink = (int) e;
        }
        [[nodiscard]] ecor::empty_env get_env() const noexcept
        {
                return {};
        }
};

// Receiver for as_variant: (g_bcast_int.schedule() || g_bcast_u8.schedule()) | as_variant
//   →  set_value(std::variant<int, uint8_t>)
struct R_as_variant
{
        using receiver_concept = ecor::receiver_t;
        void set_value( std::variant< int, uint8_t > v ) noexcept
        {
                std::visit(
                    []( auto x ) {
                            g_sink = (int) x;
                    },
                    v );
        }
        [[nodiscard]] ecor::empty_env get_env() const noexcept
        {
                return {};
        }
};

// ─────────────────────────────────────────────────────────────────────────────
// Coroutine functions  (free functions — lambdas cannot return task<T>)
// ─────────────────────────────────────────────────────────────────────────────

// C-1: void task, no co_await — measures pure coroutine-frame overhead
static ecor::task< void > coro_trivial_void( ecor::task_ctx& )
{
        co_return;
}

// C-2: int task, immediate co_return — value-returning coroutine frame
static ecor::task< int > coro_trivial_int( ecor::task_ctx& )
{
        co_return 42;
}

// C-3: await a broadcast_source — minimal scheduler round-trip
static ecor::task< void > coro_await_bcast( ecor::task_ctx& )
{
        co_await g_bcast_void.schedule();
}

// C-4: await a seq_source — adds skew-heap key cost
static ecor::task< void > coro_await_seq( ecor::task_ctx& )
{
        co_await g_seq_u32.schedule( 0u );
}

// C-5: inner task returned by value — measures task<int> promise overhead
static ecor::task< int > coro_chain_inner( ecor::task_ctx& )
{
        co_return 7;
}

// C-6: outer task that co_awaits inner task<int> — task-to-task chaining cost
static ecor::task< void > coro_chain_outer( ecor::task_ctx& ctx )
{
        g_sink = co_await coro_chain_inner( ctx );
}

// C-7: task using then adapter inside a coroutine
static ecor::task< void > coro_then_inside( ecor::task_ctx& )
{
        int v  = co_await ( g_bcast_int.schedule() | ecor::then( []( int x ) noexcept {
                                   return x * 2;
                           } ) );
        g_sink = v;
}

// C-8: task using operator|| to race two sources
static ecor::task< void > coro_or_race( ecor::task_ctx& )
{
        co_await ( g_bcast_void.schedule() || g_bcast_void2.schedule() );
}

// C-9: task using as_variant on two different-typed sources
static ecor::task< void > coro_as_variant( ecor::task_ctx& )
{
        auto v = co_await ecor::as_variant( g_bcast_int.schedule() || g_bcast_u8.schedule() );
        std::visit(
            []( auto x ) noexcept {
                    g_sink = (int) x;
            },
            v );
}

// C-10: task using sink_err to absorb errors as optional<variant<uart_err>>
static ecor::task< void > coro_sink_err( ecor::task_ctx& )
{
        auto r = co_await ( g_fifo_err.schedule() | ecor::sink_err );
        g_sink = r.has_value() ? 1 : 0;
}

// C-11: task with custom config that yields just_error(uart_err)
static ecor::task< void, uart_task_cfg > coro_custom_err( ecor::task_ctx& )
{
        co_await ecor::just_error( uart_err::framing );
}

// C-12: task that co_awaits wait_until_stopped (driven via task_holder stop())
static ecor::task< void > coro_wait_stopped( ecor::task_ctx& )
{
        co_await ecor::wait_until_stopped;
}

// D: factory for task_holder restart probe (awaits seq to be realistic)
static ecor::task< void > coro_holder_body( ecor::task_ctx& )
{
        co_await g_seq_u32.schedule( 0u );
}

// ─────────────────────────────────────────────────────────────────────────────
// Helper: flush task core without looping forever
// ─────────────────────────────────────────────────────────────────────────────
[[gnu::noinline]] static void run( ecor::task_core& c, int n = 4 )
{
        c.run_n( n );
}

// ─────────────────────────────────────────────────────────────────────────────
// Section A — source instantiation probes
// ─────────────────────────────────────────────────────────────────────────────

[[gnu::noinline]] static void probe_src_broadcast_void()
{
        R_void r;
        auto   op = g_bcast_void.schedule().connect( r );
        op.start();
        g_bcast_void.set_value();
}

[[gnu::noinline]] static void probe_src_broadcast_u8()
{
        R_u8 r;
        auto op = g_bcast_u8.schedule().connect( r );
        op.start();
        g_bcast_u8.set_value( uint8_t{ 0xAB } );
}

[[gnu::noinline]] static void probe_src_broadcast_stoppable()
{
        R_void r;
        auto   op = g_bcast_stoppable.schedule().connect( r );
        op.start();
        g_bcast_stoppable.set_stopped();
}

[[gnu::noinline]] static void probe_src_fifo_void()
{
        R_void r;
        auto   op = g_fifo_void.schedule().connect( r );
        op.start();
        g_fifo_void.set_value();
}

[[gnu::noinline]] static void probe_src_fifo_u8()
{
        R_u8 r;
        auto op = g_fifo_u8.schedule().connect( r );
        op.start();
        g_fifo_u8.set_value( uint8_t{ 0xCD } );
}

[[gnu::noinline]] static void probe_src_fifo_err()
{
        R_void r;
        auto   op = g_fifo_err.schedule().connect( r );
        op.start();
        g_fifo_err.set_error( uart_err::framing );
}

[[gnu::noinline]] static void probe_src_seq_u32()
{
        R_void r;
        auto   op = g_seq_u32.schedule( 100u ).connect( r );
        op.start();
        g_seq_u32.set_value();
}

[[gnu::noinline]] static void probe_src_seq_u16()
{
        R_void r;
        auto   op = g_seq_u16.schedule( uint16_t{ 50 } ).connect( r );
        op.start();
        g_seq_u16.set_value();
}

// ─────────────────────────────────────────────────────────────────────────────
// Section B — adapter probes (direct connection, no coroutine overhead)
// ─────────────────────────────────────────────────────────────────────────────

[[gnu::noinline]] static void probe_adapter_then_int_to_int()
{
        R_int r;
        auto  op = ( g_bcast_int.schedule() | ecor::then( []( int x ) noexcept {
                            return x * 2;
                    } ) )
                      .connect( r );
        op.start();
        g_bcast_int.set_value( 21 );
}

[[gnu::noinline]] static void probe_adapter_then_int_to_void()
{
        R_void r;
        auto   op = ( g_bcast_int.schedule() | ecor::then( []( int ) noexcept {} ) ).connect( r );
        op.start();
        g_bcast_int.set_value( 99 );
}

[[gnu::noinline]] static void probe_adapter_or_same_type()
{
        // Race two void sources — simplest case of operator||
        R_void r;
        auto   op = ( g_bcast_void.schedule() || g_bcast_void2.schedule() ).connect( r );
        op.start();
        g_bcast_void.set_value();
}

[[gnu::noinline]] static void probe_adapter_as_variant_direct()
{
        // Unify int and u8 sources via as_variant (direct, no task)
        R_as_variant r;
        auto op = ecor::as_variant( g_bcast_int.schedule() || g_bcast_u8.schedule() ).connect( r );
        op.start();
        g_bcast_int.set_value( 7 );
}

[[gnu::noinline]] static void probe_adapter_err_to_val_direct()
{
        // Convert set_error(uart_err) → set_value(uart_err) without a task
        R_err_to_val r;
        auto         op = ( g_bcast_err.schedule() | ecor::err_to_val ).connect( r );
        op.start();
        g_bcast_err.set_error( uart_err::overrun );
}

// ─────────────────────────────────────────────────────────────────────────────
// Section C — task probes  (each measures one coroutine pattern)
// ─────────────────────────────────────────────────────────────────────────────

[[gnu::noinline]] static void probe_task_trivial_void( ecor::task_ctx& ctx )
{
        R_void r;
        auto   op = coro_trivial_void( ctx ).connect( r );
        op.start();
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_trivial_int( ecor::task_ctx& ctx )
{
        R_int r;
        auto  op = coro_trivial_int( ctx ).connect( r );
        op.start();
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_await_broadcast( ecor::task_ctx& ctx )
{
        R_void r;
        auto   op = coro_await_bcast( ctx ).connect( r );
        op.start();
        run( ctx.core );
        g_bcast_void.set_value();
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_await_seq( ecor::task_ctx& ctx )
{
        R_void r;
        auto   op = coro_await_seq( ctx ).connect( r );
        op.start();
        run( ctx.core );
        g_seq_u32.set_value();
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_chain( ecor::task_ctx& ctx )
{
        // Outer task co_awaiting inner task<int> — two coroutine frames
        R_void r;
        auto   op = coro_chain_outer( ctx ).connect( r );
        op.start();
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_then_adapter( ecor::task_ctx& ctx )
{
        // then adapter used inside a coroutine
        R_void r;
        auto   op = coro_then_inside( ctx ).connect( r );
        op.start();
        run( ctx.core );
        g_bcast_int.set_value( 3 );
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_or_race( ecor::task_ctx& ctx )
{
        R_void r;
        auto   op = coro_or_race( ctx ).connect( r );
        op.start();
        run( ctx.core );
        g_bcast_void.set_value();
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_as_variant( ecor::task_ctx& ctx )
{
        R_void r;
        auto   op = coro_as_variant( ctx ).connect( r );
        op.start();
        run( ctx.core );
        g_bcast_u8.set_value( uint8_t{ 5 } );
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_sink_err( ecor::task_ctx& ctx )
{
        R_void r;
        auto   op = coro_sink_err( ctx ).connect( r );
        op.start();
        run( ctx.core );
        g_fifo_err.set_error( uart_err::parity );
        run( ctx.core );
}

[[gnu::noinline]] static void probe_task_custom_error( ecor::task_ctx& ctx )
{
        R_task_uart r;
        auto        op = coro_custom_err( ctx ).connect( r );
        op.start();
        run( ctx.core );
}

// ─────────────────────────────────────────────────────────────────────────────
// Section D — task_holder probes
// ─────────────────────────────────────────────────────────────────────────────

[[gnu::noinline]] static void probe_holder_basic_restart( ecor::task_ctx& ctx )
{
        // Holder restarts coro_holder_body every time seq fires
        ecor::task_holder holder{ ctx, coro_holder_body };
        holder.start();
        run( ctx.core );
        g_seq_u32.set_value();  // body completes → holder restarts
        run( ctx.core );
}

[[gnu::noinline]] static void probe_holder_cooperative_stop( ecor::task_ctx& ctx )
{
        // Holder wraps coro_wait_stopped; stop() signals the body's stop token
        ecor::task_holder holder_stop{ ctx, coro_wait_stopped };
        holder_stop.start();
        run( ctx.core );  // body suspends at wait_until_stopped

        R_void stop_recv;
        auto   stop_done = holder_stop.stop();
        auto   stop_op   = std::move( stop_done ).connect( stop_recv );
        stop_op.start();
        run( ctx.core, 8 );
}

// ─────────────────────────────────────────────────────────────────────────────
// Section E — inplace stop machinery
// ─────────────────────────────────────────────────────────────────────────────

[[gnu::noinline]] static void probe_stop_source_token()
{
        ecor::inplace_stop_source src;
        auto                      tok = src.get_token();
        g_sink                        = tok.stop_requested() ? 1 : 0;
        src.request_stop();
        g_sink = tok.stop_requested() ? 1 : 0;
}

[[gnu::noinline]] static void probe_stop_callback()
{
        ecor::inplace_stop_source   src;
        int                         fired = 0;
        ecor::inplace_stop_callback cb{ src.get_token(), [&fired]() noexcept {
                                               ++fired;
                                       } };
        src.request_stop();
        g_sink = fired;
}

// ─────────────────────────────────────────────────────────────────────────────
// Section F — circular_buffer_memory
// ─────────────────────────────────────────────────────────────────────────────

[[gnu::noinline]] static void probe_cbm_alloc_dealloc()
{
        // Stack-local buffer: measures CBM's alloc/dealloc path cost alone,
        // independent of g_mem (which tasks also use).
        uint8_t                                                          buf[256];
        ecor::circular_buffer_memory< ecor::smallest_index_type< 256 > > mem{ buf };

        void* p1 = ecor::allocate( mem, 32, 4 );
        void* p2 = ecor::allocate( mem, 16, 8 );
        g_sink   = ( p1 && p2 ) ? 1 : 0;
        ecor::deallocate( mem, p1, 32, 4 );
        ecor::deallocate( mem, p2, 16, 8 );
}

// ─────────────────────────────────────────────────────────────────────────────
// Entry points
// ─────────────────────────────────────────────────────────────────────────────

extern "C" void ecor_size_probe();

int main()
{
        ecor_size_probe();
        return 0;
}

void ecor_size_probe()
{
        // ── A: sources ───────────────────────────────────────────────────────
        probe_src_broadcast_void();
        probe_src_broadcast_u8();
        probe_src_broadcast_stoppable();
        probe_src_fifo_void();
        probe_src_fifo_u8();
        probe_src_fifo_err();
        probe_src_seq_u32();
        probe_src_seq_u16();

        // ── B: adapters (direct) ─────────────────────────────────────────────
        probe_adapter_then_int_to_int();
        probe_adapter_then_int_to_void();
        probe_adapter_or_same_type();
        probe_adapter_as_variant_direct();
        probe_adapter_err_to_val_direct();

        // ── C: tasks ─────────────────────────────────────────────────────────
        ecor::task_ctx ctx{ g_mem };
        probe_task_trivial_void( ctx );
        probe_task_trivial_int( ctx );
        probe_task_await_broadcast( ctx );
        probe_task_await_seq( ctx );
        probe_task_chain( ctx );
        probe_task_then_adapter( ctx );
        probe_task_or_race( ctx );
        probe_task_as_variant( ctx );
        probe_task_sink_err( ctx );
        probe_task_custom_error( ctx );

        // ── D: task_holder ───────────────────────────────────────────────────
        probe_holder_basic_restart( ctx );
        probe_holder_cooperative_stop( ctx );

        // ── E: stop machinery ────────────────────────────────────────────────
        probe_stop_source_token();
        probe_stop_callback();

        // ── F: memory ────────────────────────────────────────────────────────
        probe_cbm_alloc_dealloc();
}
