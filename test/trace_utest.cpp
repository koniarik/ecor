
/// MIT License
///
/// Copyright (c) 2025-2026 koniarik
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

// NOTE: do NOT define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN here.
// base_utest.cpp provides the main entry point.

#include "./util.hpp"
#include "doctest.h"
#include "ecor/ecor.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <vector>

namespace ecor
{

// ─── Recording trace infrastructure ──────────────────────────────────────────

enum class ev : uint8_t
{
        alloc,
        dealloc,
        promise_construct,
        op_start,
        resume,
        set_value,
        set_error,
        set_stopped,
        return_v,
        return_void,
        destroy_with_continuation,
        unhandled_exception,
        final_suspend,
        await_resume,
        await_suspend,
        await_set_value,
        await_set_error,
        await_set_stopped,
};

struct trace_log
{
        struct entry
        {
                ev          kind;
                void const* who;
                int         ival = 0;
        };
        std::vector< entry > evs;
        void                 push( ev k, void const* who, int v = 0 )
        {
                evs.push_back( { k, who, v } );
        }
        std::size_t count( ev kind ) const
        {
                std::size_t n = 0;
                for ( auto& e : evs )
                        n += ( e.kind == kind ) ? 1 : 0;
                return n;
        }
};

// Each TEST_CASE sets this before driving the event loop and resets it to nullptr after.
static trace_log* g_log = nullptr;

// Returns all entries matching `kind`.
static std::vector< std::size_t > idx_of( trace_log const& l, ev kind )
{
        std::vector< std::size_t > r;
        for ( std::size_t i = 0; i < l.evs.size(); ++i )
                if ( l.evs[i].kind == kind )
                        r.push_back( i );
        return r;
}

// Returns true if the event-kind sequence contains `expected` as a subsequence
// (other events may appear between the listed ones).
static bool contains_in_order( trace_log const& l, std::initializer_list< ev > expected )
{
        auto it = expected.begin();
        for ( auto& e : l.evs ) {
                if ( it == expected.end() )
                        break;
                if ( e.kind == *it )
                        ++it;
        }
        return it == expected.end();
}

struct rec_trace
{
        static void on_alloc( auto&, std::size_t sz, void* p ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::alloc, p, int( sz ) );
        }
        static void on_dealloc( void* p, std::size_t sz ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::dealloc, p, int( sz ) );
        }
        void on_promise_construct( auto& p, auto&, auto&&... ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::promise_construct, &p );
        }
        void on_op_start( auto& op ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::op_start, &op );
        }
        void on_resume( auto& p ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::resume, &p );
        }
        void on_set_value( auto& p, auto&... v ) noexcept
        {
                if ( !g_log )
                        return;
                if constexpr ( sizeof...( v ) == 1 )
                        g_log->push( ev::set_value, &p, int( ( v, ... ) ) );
                else
                        g_log->push( ev::set_value, &p );
        }
        void on_set_error( auto& p, auto& e ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::set_error, &p, int( e ) );
        }
        void on_set_stopped( auto& p ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::set_stopped, &p );
        }
        void on_return( auto& p, auto& v ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::return_v, &p, int( v ) );
        }
        void on_return( auto& p ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::return_void, &p );
        }
        void on_destroy_with_continuation( auto& p ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::destroy_with_continuation, &p );
        }
        void on_unhandled_exception( auto& p ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::unhandled_exception, &p );
        }
        void on_final_suspend( auto& p ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::final_suspend, &p );
        }
        void on_await_resume( auto& a ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::await_resume, &a );
        }
        void on_await_suspend( auto& a ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::await_suspend, &a );
        }
        void on_await_set_value( auto& a, auto&... v ) noexcept
        {
                if ( !g_log )
                        return;
                if constexpr ( sizeof...( v ) == 1 )
                        g_log->push( ev::await_set_value, &a, int( ( v, ... ) ) );
                else
                        g_log->push( ev::await_set_value, &a );
        }
        void on_await_set_error( auto& a, auto& e ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::await_set_error, &a, int( e ) );
        }
        void on_await_set_stopped( auto& a ) noexcept
        {
                if ( g_log )
                        g_log->push( ev::await_set_stopped, &a );
        }
};

struct rec_cfg
{
        using extra_error_signatures = completion_signatures<>;
        using trace_type             = rec_trace;
};

// ─── Zero-overhead static assertion (C.4) ────────────────────────────────────

struct _empty_trace_cfg
{
        using extra_error_signatures = completion_signatures<>;
        using trace_type             = task_default_trace;
};
static_assert( sizeof( task< void, _empty_trace_cfg > ) == sizeof( task< void > ) );

// ─── Receivers used by several tests ─────────────────────────────────────────

// Pointer-based receiver so results are visible even after the receiver is moved
// into the op_state. All fields are raw pointers; may be null if a particular
// signal is not of interest to the test.
struct _tr_recv_int
{
        using receiver_concept = receiver_t;
        int*        out        = nullptr;
        task_error* err_out    = nullptr;

        void set_value( int v ) noexcept
        {
                if ( out )
                        *out = v;
        }
        void set_error( task_error e ) noexcept
        {
                if ( err_out )
                        *err_out = e;
        }
        void set_stopped() noexcept
        {
        }
};

struct _tr_recv_void
{
        using receiver_concept = receiver_t;
        bool*       called     = nullptr;
        task_error* err_out    = nullptr;
        bool*       stopped    = nullptr;

        void set_value() noexcept
        {
                if ( called )
                        *called = true;
        }
        void set_error( task_error e ) noexcept
        {
                if ( err_out )
                        *err_out = e;
        }
        void set_stopped() noexcept
        {
                if ( stopped )
                        *stopped = true;
        }
};

// ─── Coroutine helpers (file scope — required by ecor style rules) ────────────

static task< int, rec_cfg > tr_int( task_ctx& ctx, int v )
{
        co_return v;
}

static task< void, rec_cfg > tr_void( task_ctx& ctx )
{
        (void) ctx;
        co_return;
}

static task< int, rec_cfg >
tr_await_int( task_ctx& ctx, broadcast_source< set_value_t( int ) >& src )
{
        int v = co_await src.schedule();
        co_return v;
}

static task< void, rec_cfg > tr_throw( task_ctx& ctx )
{
        (void) ctx;
        throw int( 42 );
        co_return;
}

static task< void, rec_cfg >
tr_await_err( task_ctx& ctx, broadcast_source< set_value_t(), set_error_t( task_error ) >& src )
{
        co_await src.schedule();
        co_return;
}

static task< void, rec_cfg >
tr_await_stop( task_ctx& ctx, broadcast_source< set_value_t(), set_stopped_t() >& src )
{
        co_await src.schedule();
        co_return;
}

static task< int, rec_cfg > tr_two_awaits(
    task_ctx&                               ctx,
    broadcast_source< set_value_t( int ) >& a,
    broadcast_source< set_value_t( int ) >& b )
{
        int x = co_await a.schedule();
        int y = co_await b.schedule();
        co_return x + y;
}

// ─── Group A — Single-task lifecycle ─────────────────────────────────────────

TEST_CASE( "trace - A1 int task value completion" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log   = &log;
        int out = 0;

        {  // scope: op destroyed here → on_dealloc fires while g_log is live
                auto op = tr_int( ctx, 7 ).connect( _tr_recv_int{ &out } );
                op.start();
                while ( ctx.core.run_once() ) {
                }
        }

        g_log = nullptr;

        CHECK( out == 7 );

        // Subsequence: alloc → promise_construct → op_start → resume → return_v(7) → set_value(7)
        //              → final_suspend → dealloc
        CHECK( contains_in_order(
            log,
            { ev::alloc,
              ev::promise_construct,
              ev::op_start,
              ev::resume,
              ev::return_v,
              ev::set_value,
              ev::final_suspend,
              ev::dealloc } ) );

        // Exact counts
        CHECK( log.count( ev::alloc ) == 1 );
        CHECK( log.count( ev::dealloc ) == 1 );
        CHECK( log.count( ev::promise_construct ) == 1 );
        CHECK( log.count( ev::op_start ) == 1 );
        CHECK( log.count( ev::resume ) == 1 );
        CHECK( log.count( ev::return_v ) == 1 );
        CHECK( log.count( ev::set_value ) == 1 );
        CHECK( log.count( ev::final_suspend ) == 1 );
        CHECK( log.count( ev::set_error ) == 0 );
        CHECK( log.count( ev::set_stopped ) == 0 );
        CHECK( log.count( ev::destroy_with_continuation ) == 0 );
        CHECK( log.count( ev::unhandled_exception ) == 0 );

        // return_v carries the correct value
        auto rv = idx_of( log, ev::return_v );
        CHECK( log.evs[rv[0]].ival == 7 );

        // set_value carries the correct value
        auto sv = idx_of( log, ev::set_value );
        CHECK( log.evs[sv[0]].ival == 7 );

        // alloc/dealloc addresses and sizes match
        auto ai = idx_of( log, ev::alloc );
        auto di = idx_of( log, ev::dealloc );
        CHECK( log.evs[ai[0]].who == log.evs[di[0]].who );
        CHECK( log.evs[ai[0]].ival == log.evs[di[0]].ival );

        // All promise-bearing events point to the same instance
        auto        pci   = idx_of( log, ev::promise_construct );
        auto        ri    = idx_of( log, ev::resume );
        auto        fsi   = idx_of( log, ev::final_suspend );
        void const* paddr = log.evs[pci[0]].who;
        CHECK( log.evs[ri[0]].who == paddr );
        CHECK( log.evs[rv[0]].who == paddr );
        CHECK( log.evs[sv[0]].who == paddr );
        CHECK( log.evs[fsi[0]].who == paddr );

        // op_start.who differs from the promise address (it is the op_state)
        auto osi = idx_of( log, ev::op_start );
        CHECK( log.evs[osi[0]].who != paddr );
}

TEST_CASE( "trace - A2 void task completion" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log              = &log;
        bool       called  = false;
        task_error err_out = task_error::none;

        {
                auto op = tr_void( ctx ).connect( _tr_recv_void{ &called, &err_out } );
                op.start();
                while ( ctx.core.run_once() ) {
                }
        }

        g_log = nullptr;

        CHECK( called );

        CHECK( contains_in_order(
            log,
            { ev::alloc,
              ev::promise_construct,
              ev::op_start,
              ev::resume,
              ev::return_void,
              ev::set_value,
              ev::final_suspend,
              ev::dealloc } ) );

        // Exact counts
        CHECK( log.count( ev::alloc ) == 1 );
        CHECK( log.count( ev::dealloc ) == 1 );
        CHECK( log.count( ev::promise_construct ) == 1 );
        CHECK( log.count( ev::op_start ) == 1 );
        CHECK( log.count( ev::resume ) == 1 );
        CHECK( log.count( ev::return_void ) == 1 );
        CHECK( log.count( ev::return_v ) == 0 );
        CHECK( log.count( ev::set_value ) == 1 );
        CHECK( log.count( ev::set_error ) == 0 );
        CHECK( log.count( ev::set_stopped ) == 0 );
        CHECK( log.count( ev::destroy_with_continuation ) == 0 );
        CHECK( log.count( ev::unhandled_exception ) == 0 );
        CHECK( log.count( ev::final_suspend ) == 1 );
}

TEST_CASE( "trace - A3 throwing task unhandled exception and set_error" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log              = &log;
        task_error err_out = task_error::none;

        {
                auto op = tr_throw( ctx ).connect( _tr_recv_void{ nullptr, &err_out } );
                op.start();
                while ( ctx.core.run_once() ) {
                }
        }

        g_log = nullptr;

        CHECK( err_out == task_error::task_unhandled_exception );

        CHECK( contains_in_order(
            log,
            { ev::alloc,
              ev::promise_construct,
              ev::op_start,
              ev::resume,
              ev::unhandled_exception,
              ev::set_error,
              ev::final_suspend,
              ev::dealloc } ) );

        // set_error carries task_unhandled_exception
        auto sei = idx_of( log, ev::set_error );
        CHECK( log.evs[sei[0]].ival == int( task_error::task_unhandled_exception ) );

        // Exact counts
        CHECK( log.count( ev::alloc ) == 1 );
        CHECK( log.count( ev::dealloc ) == 1 );
        CHECK( log.count( ev::promise_construct ) == 1 );
        CHECK( log.count( ev::op_start ) == 1 );
        CHECK( log.count( ev::resume ) == 1 );
        CHECK( log.count( ev::unhandled_exception ) == 1 );
        CHECK( log.count( ev::set_error ) == 1 );
        CHECK( log.count( ev::final_suspend ) == 1 );
        CHECK( log.count( ev::return_v ) == 0 );
        CHECK( log.count( ev::return_void ) == 0 );
        CHECK( log.count( ev::set_value ) == 0 );
        CHECK( log.count( ev::set_stopped ) == 0 );
        CHECK( log.count( ev::destroy_with_continuation ) == 0 );
}

TEST_CASE( "trace - A4 on_dealloc arguments match on_alloc" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log   = &log;
        int out = 0;

        {
                auto op = tr_int( ctx, 7 ).connect( _tr_recv_int{ &out } );
                op.start();
                while ( ctx.core.run_once() ) {
                }
        }

        g_log = nullptr;

        auto ai = idx_of( log, ev::alloc );
        auto di = idx_of( log, ev::dealloc );
        REQUIRE( ai.size() == 1 );
        REQUIRE( di.size() == 1 );
        CHECK( log.evs[ai[0]].who == log.evs[di[0]].who );
        CHECK( log.evs[ai[0]].ival == log.evs[di[0]].ival );
        CHECK( log.evs[ai[0]].ival > 0 );
}

// ─── Group B — Awaiting child senders ────────────────────────────────────────

TEST_CASE( "trace - B1 await child value" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log = &log;

        broadcast_source< set_value_t( int ) > src;
        int                                    out = 0;

        {
                auto op = tr_await_int( ctx, src ).connect( _tr_recv_int{ &out } );
                op.start();

                // First run_once: initial resume → task suspends on co_await
                ctx.core.run_once();

                // Fire the child sender
                src.set_value( 11 );

                // Second run_once: task resumes, runs to completion
                ctx.core.run_once();
        }

        g_log = nullptr;

        CHECK( out == 11 );

        CHECK( contains_in_order(
            log,
            { ev::op_start,
              ev::resume,
              ev::await_suspend,
              ev::await_set_value,
              ev::resume,
              ev::await_resume,
              ev::return_v,
              ev::set_value,
              ev::final_suspend } ) );

        // await_suspend.who == await_set_value.who == await_resume.who (same awaiter)
        auto asui = idx_of( log, ev::await_suspend );
        auto asvi = idx_of( log, ev::await_set_value );
        auto ari  = idx_of( log, ev::await_resume );
        REQUIRE( asui.size() == 1 );
        REQUIRE( asvi.size() == 1 );
        REQUIRE( ari.size() == 1 );
        CHECK( log.evs[asui[0]].who == log.evs[asvi[0]].who );
        CHECK( log.evs[asui[0]].who == log.evs[ari[0]].who );

        // await_set_value carries correct value
        CHECK( log.evs[asvi[0]].ival == 11 );

        // await_suspend comes before await_set_value
        CHECK( asui[0] < asvi[0] );

        // Exact counts
        CHECK( log.count( ev::alloc ) == 1 );
        CHECK( log.count( ev::dealloc ) == 1 );
        CHECK( log.count( ev::resume ) == 2 );
        CHECK( log.count( ev::await_suspend ) == 1 );
        CHECK( log.count( ev::await_resume ) == 1 );
        CHECK( log.count( ev::await_set_value ) == 1 );
        CHECK( log.count( ev::await_set_error ) == 0 );
        CHECK( log.count( ev::await_set_stopped ) == 0 );
        CHECK( log.count( ev::return_v ) == 1 );
        CHECK( log.count( ev::set_value ) == 1 );
        CHECK( log.count( ev::set_error ) == 0 );
        CHECK( log.count( ev::set_stopped ) == 0 );
        CHECK( log.count( ev::final_suspend ) == 1 );
}

TEST_CASE( "trace - B2 await child error propagates" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log = &log;

        broadcast_source< set_value_t(), set_error_t( task_error ) > src;
        task_error                                                   err_out = task_error::none;

        {
                auto op = tr_await_err( ctx, src ).connect( _tr_recv_void{ nullptr, &err_out } );
                op.start();
                ctx.core.run_once();
                src.set_error( task_error::task_unfinished );
                ctx.core.run_once();
        }

        g_log = nullptr;

        CHECK( err_out == task_error::task_unfinished );

        CHECK( contains_in_order(
            log,
            { ev::op_start, ev::resume, ev::await_suspend, ev::await_set_error, ev::set_error } ) );

        // Exact counts
        CHECK( log.count( ev::resume ) == 1 );
        CHECK( log.count( ev::await_suspend ) == 1 );
        CHECK( log.count( ev::await_set_error ) == 1 );
        CHECK( log.count( ev::await_resume ) == 0 );
        CHECK( log.count( ev::return_v ) == 0 );
        CHECK( log.count( ev::return_void ) == 0 );
        CHECK( log.count( ev::set_error ) == 1 );
        CHECK( log.count( ev::set_value ) == 0 );
        CHECK( log.count( ev::set_stopped ) == 0 );
        // Error/stop bypasses the coroutine body; final_suspend is NOT reached.
        CHECK( log.count( ev::final_suspend ) == 0 );
        CHECK( log.count( ev::destroy_with_continuation ) == 0 );
        CHECK( log.count( ev::dealloc ) == 1 );

        // await_set_error carries the correct error value
        auto asei = idx_of( log, ev::await_set_error );
        CHECK( log.evs[asei[0]].ival == int( task_error::task_unfinished ) );
}

TEST_CASE( "trace - B3 await child stop propagates" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log = &log;

        broadcast_source< set_value_t(), set_stopped_t() > src;
        bool                                               stopped = false;

        {
                auto op = tr_await_stop( ctx, src )
                              .connect( _tr_recv_void{ nullptr, nullptr, &stopped } );
                op.start();
                ctx.core.run_once();
                src.set_stopped();
                ctx.core.run_once();
        }

        g_log = nullptr;

        CHECK( stopped );

        CHECK( contains_in_order(
            log,
            { ev::op_start,
              ev::resume,
              ev::await_suspend,
              ev::await_set_stopped,
              ev::set_stopped } ) );

        // Exact counts
        CHECK( log.count( ev::resume ) == 1 );
        CHECK( log.count( ev::await_suspend ) == 1 );
        CHECK( log.count( ev::await_set_stopped ) == 1 );
        CHECK( log.count( ev::await_resume ) == 0 );
        CHECK( log.count( ev::return_v ) == 0 );
        CHECK( log.count( ev::return_void ) == 0 );
        CHECK( log.count( ev::set_value ) == 0 );
        CHECK( log.count( ev::set_error ) == 0 );
        CHECK( log.count( ev::set_stopped ) == 1 );
        // Error/stop bypasses the coroutine body; final_suspend is NOT reached.
        CHECK( log.count( ev::final_suspend ) == 0 );
        CHECK( log.count( ev::destroy_with_continuation ) == 0 );
        CHECK( log.count( ev::dealloc ) == 1 );
}

TEST_CASE( "trace - B4 two consecutive co_awaits" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log = &log;

        broadcast_source< set_value_t( int ) > a;
        broadcast_source< set_value_t( int ) > b;
        int                                    out = 0;

        {
                auto op = tr_two_awaits( ctx, a, b ).connect( _tr_recv_int{ &out } );
                op.start();
                ctx.core.run_once();  // initial resume → suspends on co_await a
                a.set_value( 5 );
                ctx.core.run_once();  // resumes → suspends on co_await b
                b.set_value( 6 );
                ctx.core.run_once();  // resumes → co_return 11
        }

        g_log = nullptr;

        CHECK( out == 11 );

        // Strict ordering of the two await cycles
        CHECK( contains_in_order(
            log,
            { ev::await_suspend,
              ev::await_set_value,
              ev::resume,
              ev::await_resume,
              ev::await_suspend,
              ev::await_set_value,
              ev::resume,
              ev::await_resume,
              ev::return_v } ) );

        // Exact counts
        CHECK( log.count( ev::resume ) == 3 );
        CHECK( log.count( ev::await_suspend ) == 2 );
        CHECK( log.count( ev::await_resume ) == 2 );
        CHECK( log.count( ev::await_set_value ) == 2 );
        CHECK( log.count( ev::return_v ) == 1 );
        CHECK( log.count( ev::set_value ) == 1 );
        CHECK( log.count( ev::final_suspend ) == 1 );

        // return_v carries 11
        auto rvi = idx_of( log, ev::return_v );
        CHECK( log.evs[rvi[0]].ival == 11 );
        auto svi = idx_of( log, ev::set_value );
        CHECK( log.evs[svi[0]].ival == 11 );

        // Optimized builds may reuse coroutine-frame storage, so awaiter addresses are not a
        // stable cross-await identity.
        auto asui = idx_of( log, ev::await_suspend );
        REQUIRE( asui.size() == 2 );
}

// ─── Group C — Edge cases ────────────────────────────────────────────────────

TEST_CASE( "trace - C1 destroy_with_continuation when op destroyed before await fires" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log = &log;

        broadcast_source< set_value_t( int ) > src;

        {
                // Scope-limit the op_state so it is destroyed while the task is still
                // suspended waiting on src.
                int  out = 0;
                auto op  = tr_await_int( ctx, src ).connect( _tr_recv_int{ &out } );
                op.start();
                ctx.core.run_once();  // task suspends on co_await src.schedule()
                // op destroyed here — task frame still alive, continuation still set
        }

        g_log = nullptr;

        // destroy_with_continuation must have fired
        CHECK( log.count( ev::destroy_with_continuation ) == 1 );

        // dealloc must have fired (frame freed)
        CHECK( log.count( ev::dealloc ) == 1 );

        // no normal completion hooks
        CHECK( log.count( ev::final_suspend ) == 0 );
        CHECK( log.count( ev::set_value ) == 0 );
        CHECK( log.count( ev::set_error ) == 0 );
        CHECK( log.count( ev::await_resume ) == 0 );

        // destroy_with_continuation.who is the promise address (same as promise_construct.who)
        auto dwci = idx_of( log, ev::destroy_with_continuation );
        auto pci  = idx_of( log, ev::promise_construct );
        REQUIRE( dwci.size() == 1 );
        REQUIRE( pci.size() == 1 );
        CHECK( log.evs[dwci[0]].who == log.evs[pci[0]].who );
}

TEST_CASE( "trace - C2 alloc failure: on_alloc fires with null; no downstream hooks" )
{
        // Provide a 1-byte buffer — guaranteed too small for any coroutine frame.
        std::array< uint8_t, 1 >                           storage;
        std::span< uint8_t, 1 >                            sp{ storage };
        circular_buffer_memory< smallest_index_type< 1 > > cbm{ sp };
        task_ctx                                           ctx{ cbm };

        trace_log log;
        g_log              = &log;
        task_error err_out = task_error::none;

        {
                auto op = tr_void( ctx ).connect( _tr_recv_void{ nullptr, &err_out } );
                op.start();
        }

        g_log = nullptr;

        // on_alloc fires once (with a null pointer on failure)
        CHECK( log.count( ev::alloc ) == 1 );
        auto ai = idx_of( log, ev::alloc );
        CHECK( log.evs[ai[0]].who == nullptr );

        // No hooks that require a valid frame
        CHECK( log.count( ev::promise_construct ) == 0 );
        CHECK( log.count( ev::op_start ) == 0 );
        CHECK( log.count( ev::resume ) == 0 );
        CHECK( log.count( ev::final_suspend ) == 0 );
        CHECK( log.count( ev::dealloc ) == 0 );

        // Receiver should have observed task_allocation_failure
        CHECK( err_out == task_error::task_allocation_failure );
}

TEST_CASE( "trace - C3 two tasks parallel events do not corrupt each other" )
{
        nd_mem    mem;
        task_ctx  ctx{ mem };
        trace_log log;
        g_log = &log;

        int out_a = 0;
        int out_b = 0;

        {
                auto op_a = tr_int( ctx, 3 ).connect( _tr_recv_int{ &out_a } );
                auto op_b = tr_int( ctx, 9 ).connect( _tr_recv_int{ &out_b } );
                op_a.start();
                op_b.start();
                while ( ctx.core.run_once() ) {
                }
        }

        g_log = nullptr;

        CHECK( out_a == 3 );
        CHECK( out_b == 9 );

        // Two distinct promise addresses
        auto pci = idx_of( log, ev::promise_construct );
        REQUIRE( pci.size() == 2 );
        void const* pa = log.evs[pci[0]].who;
        void const* pb = log.evs[pci[1]].who;
        CHECK( pa != pb );

        // Two distinct alloc pointers
        auto ai = idx_of( log, ev::alloc );
        REQUIRE( ai.size() == 2 );
        CHECK( log.evs[ai[0]].who != log.evs[ai[1]].who );

        // Total counts
        CHECK( log.count( ev::alloc ) == 2 );
        CHECK( log.count( ev::dealloc ) == 2 );
        CHECK( log.count( ev::op_start ) == 2 );
        CHECK( log.count( ev::set_value ) == 2 );
        CHECK( log.count( ev::final_suspend ) == 2 );
        CHECK( log.count( ev::set_error ) == 0 );
        CHECK( log.count( ev::set_stopped ) == 0 );

        // For each promise, events with matching `who` form a valid A.1 sequence
        auto matching = [&]( void const* p, ev kind ) {
                for ( auto& e : log.evs )
                        if ( e.kind == kind && e.who == p )
                                return true;
                return false;
        };
        for ( void const* p : { pa, pb } ) {
                CHECK( matching( p, ev::promise_construct ) );
                CHECK( matching( p, ev::resume ) );
                CHECK( matching( p, ev::return_v ) );
                CHECK( matching( p, ev::set_value ) );
                CHECK( matching( p, ev::final_suspend ) );
        }
}

}  // namespace ecor
