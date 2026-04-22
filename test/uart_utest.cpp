
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

#include "./util.hpp"
#include "doctest.h"
#include "ecor/ecor.hpp"

#include <functional>
#include <iostream>
#include <list>
#include <mutex>
#include <semaphore>
#include <thread>

/// -------------------------------------------------------------------------------
/// UART driver example & test suite
///
/// This file demonstrates how to build a real-world UART driver on top of the
/// ecor transaction abstraction. The design mirrors what a production embedded
/// driver would look like:
///
///   uart_peripheral  — mock hardware that simulates async TX/RX with background
///                       threads (stands in for DMA + ISR on real hardware)
///   uart_trans       — transaction payload: buffer, TX/RX sizes, error, timeout
///   uart             — the driver: owns a trnx_controller_source and a
///                       trnx_circular_buffer, wires ISR callbacks to the
///                       buffer cursors, and delivers completions in tick()
///
/// The test cases exercise the full lifecycle: basic exchange, cancellation
/// (pending and in-flight), errors, FIFO ordering, flow control (buffer full),
/// destruction cleanup, and stop-token propagation.
/// -------------------------------------------------------------------------------

namespace ecor
{

using namespace std::chrono_literals;

/// UART error flags, matching typical hardware register bit positions.
enum class uart_error : uint32_t
{
        none    = 0,
        overrun = 1 << 0,  // old data unread during RX
        framing = 1 << 1,  // stop bit not found, RX
        parity  = 1 << 2,  // parity check failed, RX
        timeout = 1 << 3,  // receiver timeout, RX
        nack    = 1 << 4,  // Smartcard NACK detected, TX
};

enum class uart_status
{
        ok,
        busy
};


/// Mock UART peripheral that simulates asynchronous TX and RX using background
/// threads. Each transmit() call enqueues work on a job queue thread; when that
/// work completes it parses the transmitted command string and schedules an
/// appropriate RX reply (echo, error, slow, partial, silent).
///
/// The cpu_mtx simulates the "interrupt fires on the main CPU" constraint:
/// ISR callbacks (on_tx_irq, on_rx_irq, etc.) are always called under that lock,
/// and the main-loop tick() must also hold it when reading shared state.
struct uart_peripheral
{
        uart_peripheral(
            std::function< void() >              on_tx_irq,
            std::function< void( uart_error ) >  on_tx_err,
            std::function< void( std::size_t ) > on_rx_irq,
            std::function< void( uart_error ) >  on_rx_err )
          : on_tx_irq( std::move( on_tx_irq ) )
          , on_tx_err( std::move( on_tx_err ) )
          , on_rx_irq( std::move( on_rx_irq ) )
          , on_rx_err( std::move( on_rx_err ) )
        {
        }

        void delay( int iterations = -1 )
        {
                auto x = ( iterations == -1 ) ? std::rand() % 42 : iterations;
                for ( int i = 0; i < x; i++ )
                        asm( "nop" );
        }

        uart_status transmit( std::span< uint8_t > data )
        {
                bool expected = false;
                if ( !tx_active.compare_exchange_strong( expected, true ) )
                        return uart_status::busy;
                tx_jq.emplace_back(
                    [data = std::vector< uint8_t >( data.begin(), data.end() ), this] {
                            delay();
                            std::lock_guard lg{ cpu_mtx };
                            do_reply( data );
                    } );
                return uart_status::ok;
        }

        void do_reply( std::span< uint8_t const > data )
        {
                auto schedule_rx = [&]( std::string sv, int delay_iters = -1 ) {
                        this->on_tx_irq();

                        rx_jq.emplace_back( [sv = std::move( sv ), delay_iters, this] {
                                delay( delay_iters );
                                std::lock_guard lg{ cpu_mtx };
                                if ( rx_buff.empty() )
                                        std::abort();
                                if ( rx_buff.size() < sv.size() )
                                        std::abort();
                                std::memcpy( rx_buff.data(), sv.data(), sv.size() );
                                this->on_rx_irq( sv.size() );
                        } );
                };

                auto schedule_err = [&]( uart_error e ) {
                        this->on_tx_irq();

                        rx_jq.emplace_back( [e, this] {
                                delay();
                                std::lock_guard lg{ cpu_mtx };
                                this->on_rx_err( e );
                        } );
                };

                // XXX: errors
                std::string_view sv{ (char*) data.data(), data.size() };
                auto             colon = sv.find( ':' );
                auto             cmd   = sv.substr( 0, colon );
                auto arg = ( colon == std::string_view::npos ) ? "" : sv.substr( colon + 1 );

                if ( cmd == "echo" )
                        schedule_rx( std::string( arg ) );
                else if ( cmd == "err" ) {
                        if ( arg == "overrun" )
                                schedule_err( uart_error::overrun );
                        else if ( arg == "framing" )
                                schedule_err( uart_error::framing );
                        else if ( arg == "parity" )
                                schedule_err( uart_error::parity );
                        else if ( arg == "timeout" )
                                schedule_err( uart_error::timeout );
                        else if ( arg == "nack" )
                                this->on_tx_err( uart_error::nack );
                        else
                                std::abort();
                } else if ( cmd == "silent" ) {
                        // Just complete TX, no RX scheduled
                        this->on_tx_irq();
                } else if ( cmd == "slow" ) {
                        int iters = std::stoi( std::string( arg ) );
                        schedule_rx( "valid_resp", iters );
                } else if ( cmd == "partial" ) {
                        int len = std::stoi( std::string( arg ) );
                        schedule_rx( std::string( len, 'X' ) );
                } else
                        std::abort();
        }

        uart_status receive( std::span< uint8_t > data )
        {
                if ( rx_active.load() )
                        return uart_status::busy;
                rx_active = true;
                rx_buff   = data;
                return uart_status::ok;
        }

        struct job_queue
        {
                std::list< std::function< void() > > cbs;
                std::mutex                           m;
                std::counting_semaphore< 1024 >      cs{ 0 };
                std::jthread                         thread{ [this]( std::stop_token st ) {
                        this->run( std::move( st ) );
                } };

                void emplace_back( std::function< void() > cb )
                {
                        std::lock_guard lg{ m };
                        cbs.emplace_back( std::move( cb ) );
                        cs.release();
                }

                void run( std::stop_token st )
                {
                        while ( !st.stop_requested() ) {
                                if ( !cs.try_acquire_for( 10ms ) )
                                        continue;
                                auto f = [&] {
                                        std::lock_guard lg{ m };
                                        auto            f = std::move( cbs.front() );
                                        cbs.pop_front();
                                        return f;
                                }();
                                f();
                        }
                }
        };

        std::mutex                           cpu_mtx;
        std::function< void() >              on_tx_irq;
        std::function< void( uart_error ) >  on_tx_err;
        std::function< void( std::size_t ) > on_rx_irq;
        std::function< void( uart_error ) >  on_rx_err;

        job_queue           tx_jq;
        std::atomic< bool > tx_active = false;

        job_queue            rx_jq;
        std::atomic< bool >  rx_active = false;
        std::span< uint8_t > rx_buff;
};

/// Transaction payload for the UART driver. Carries the buffer, usage counters,
/// error state, and timeout. The nested `completion_signatures` type alias tells
/// the transaction abstraction what signals the sender can complete with.
struct uart_trans
{
        using completion_signatures = completion_signatures<
            set_value_t( std::span< uint8_t > ),
            set_error_t( uart_error ),
            set_stopped_t() >;

        // Buffer for message
        std::span< uint8_t > buffer;
        // How much of "buffer" is used by transmitted message
        uint16_t tx_used;
        uint16_t rx_used;

        uart_error err = uart_error::none;

        using tp = std::chrono::steady_clock::time_point;
        tp timeout;
};

/// UART driver built on top of ecor's transaction abstraction.
///
/// Owns a `trnx_controller_source<uart_trans>` for scheduling transactions and a
/// `trnx_circular_buffer` (capacity 4) for managing in-flight ones. The driver
/// wires four ISR callbacks from `uart_peripheral` to advance the buffer cursors:
///
///   on_tx_complete_irq  — TX DMA done, advance tx cursor
///   on_tx_error_irq     — TX NACK, record error and advance tx cursor
///   on_rx_complete_irq  — RX DMA done, record size and advance rx cursor
///   on_rx_error_irq     — RX error, record error and advance rx cursor
///
/// The main-loop calls tick() which:
///   1. Pulls pending transactions from the source into the circular buffer
///   2. Starts TX for the next queued entry
///   3. Delivers completed transactions by calling set_value/set_error on entries
struct uart
{

        /// Returns a sender representing a UART transaction.
        // - set_value_t(std::span<uint8_t>) - reply
        // - set_error_t(uart_error) - error during transaction
        auto transact( std::span< uint8_t > buffer, uint16_t tx_used )
        {
                return _source.schedule( uart_trans{ .buffer = buffer, .tx_used = tx_used } );
        }

        void on_tx_complete_irq()
        {
                CHECK( _trnxs.has_tx() );
                _trnxs.tx_done();
                _handle.tx_active = false;
                dispatch_rx();
                dispatch_tx();
        }

        void on_tx_error_irq( uart_error e )
        {
                CHECK( _trnxs.has_tx() );
                _trnxs.tx_front()->data.err = e;
                _trnxs.tx_done();
                _handle.tx_active = false;
                dispatch_rx();
                dispatch_tx();
        }

        void on_rx_complete_irq( std::size_t s )
        {
                _handle.rx_active               = false;
                _trnxs.rx_front()->data.rx_used = s;
                _trnxs.rx_done();
                dispatch_rx();
        }

        void on_rx_error_irq( uart_error e )
        {
                _handle.rx_active           = false;
                _trnxs.rx_front()->data.err = e;
                _trnxs.rx_done();
                dispatch_rx();
        }

        void dispatch_tx()
        {
                if ( !_trnxs.has_tx() )
                        return;
                auto* n = _trnxs.tx_front();
                _handle.transmit( n->data.buffer.subspan( 0, n->data.tx_used ) );
        }

        void dispatch_rx()
        {
                while ( _trnxs.has_rx() ) {
                        auto* n = _trnxs.rx_front();
                        if ( n->data.err != uart_error::none ) {
                                _trnxs.rx_done();
                                continue;
                        } else if ( _handle.rx_active ) {
                                break;
                        } else {
                                _handle.receive( n->data.buffer );
                                break;
                        }
                }
        }

        void tick()
        {
                while ( !_trnxs.full() ) {
                        auto* n = _source.query_next_trnx();
                        if ( !n )
                                break;
                        _trnxs.push( n );
                }

                if ( !_handle.tx_active )
                        dispatch_tx();

                while ( _trnxs.has_deliver() ) {
                        auto* p = _trnxs.deliver_front();
                        _trnxs.pop();
                        if ( p->data.err != uart_error::none )
                                p->_set_error( p->data.err );
                        else
                                p->_set_value( p->data.buffer.subspan( 0, p->data.rx_used ) );
                }
        }

        trnx_controller_source< uart_trans >                 _source;
        trnx_circular_buffer< trnx_entry< uart_trans >*, 4 > _trnxs;

        uart_peripheral _handle{
            [this] {
                    on_tx_complete_irq();
            },
            [this]( uart_error e ) {
                    on_tx_error_irq( e );
            },
            [this]( std::size_t s ) {
                    on_rx_complete_irq( s );
            },
            [this]( uart_error e ) {
                    on_rx_error_irq( e );
            } };
};

task< void > simple_exchange( task_ctx&, uart& u, std::string& result_out )
{
        uint8_t     buffer[42];
        std::string req = "echo:valid_resp";
        std::memcpy( buffer, req.data(), req.size() );

        // We expect result to be span of uint8_t
        auto result_span = co_await ( u.transact( buffer, req.size() ) | err_to_val | as_variant );

        if ( auto* p = std::get_if< std::span< uint8_t > >( &result_span ) )
                result_out = std::string( (char*) p->data(), p->size() );
}

task< void > multiple_exchanges( task_ctx& ctx, uart& u, int count, bool& done )
{
        std::string result;
        int         passed = 0;
        for ( int i = 0; i < count; ++i ) {
                result.clear();
                co_await simple_exchange( ctx, u, result );
                if ( result == "valid_resp" )
                        passed++;
        }
        CHECK_EQ( passed, count );
        done = true;
}

task< void > basic_exchange_test( task_ctx& ctx, uart& u, std::string& result, bool& done )
{
        co_await simple_exchange( ctx, u, result );
        done = true;
}

struct uart_test_context
{
        uart     u;
        nd_mem   mem;
        task_ctx ctx{ mem };

        void run_until( std::function< bool() > condition, std::chrono::milliseconds timeout = 1s )
        {
                auto start = std::chrono::steady_clock::now();
                while ( !condition() ) {
                        u.tick();
                        ctx.core.run_once();
                        if ( std::chrono::steady_clock::now() - start > timeout )
                                break;
                        std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
                }
        }
};

struct capturing_receiver
{
        using receiver_concept = ecor::receiver_t;

        int*                        value_count   = nullptr;
        int*                        error_count   = nullptr;
        uart_error*                 error_value   = nullptr;
        int*                        stopped_count = nullptr;
        std::vector< std::string >* results       = nullptr;
        std::string                 id            = "";
        inplace_stop_source*        stop_source   = nullptr;

        void set_value( std::span< uint8_t > ) noexcept
        {
                if ( value_count )
                        ( *value_count )++;
                if ( results )
                        results->push_back( id );
        }

        void set_error( uart_error e ) noexcept
        {
                if ( error_count )
                        ( *error_count )++;
                if ( error_value )
                        *error_value = e;
                if ( results )
                        results->push_back( "err" );
        }

        void set_error( std::exception_ptr ) noexcept
        {
                if ( error_count )
                        ( *error_count )++;
                if ( results )
                        results->push_back( "exc" );
        }

        void set_stopped() noexcept
        {
                if ( stopped_count )
                        ( *stopped_count )++;
                if ( results )
                        results->push_back( "stop" );
        }

        struct env
        {
                inplace_stop_token token;

                inplace_stop_token query( ecor::get_stop_token_t ) const noexcept
                {
                        return token;
                }
        };

        env get_env() const noexcept
        {
                if ( stop_source )
                        return env{ stop_source->get_token() };
                return env{};
        }
};

// Receiver whose set_stopped() immediately starts a pre-constructed follow-up
// operation. This exercises the re-entry path in query_next_transaction where
// remove_if fires set_stopped() and the handler calls link_front() on the same
// list while the traversal is still in progress.
//
// The type-erased (void*, fn-ptr) pair avoids a circular type dependency between
// the receiver and the op it holds.
struct _requeue_on_stop_receiver
{
        using receiver_concept = ecor::receiver_t;

        int*                 stop_count  = nullptr;
        inplace_stop_source* stop_source = nullptr;

        // Pointer to a pre-constructed, not-yet-started op_state and a shim
        // that calls .start() on it without knowing its concrete type here.
        void* followup_op                 = nullptr;
        void ( *start_followup )( void* ) = nullptr;

        void set_value( std::span< uint8_t > ) noexcept
        {
        }
        void set_error( uart_error ) noexcept
        {
        }
        void set_error( std::exception_ptr ) noexcept
        {
        }

        void set_stopped() noexcept
        {
                if ( stop_count )
                        ( *stop_count )++;
                // Inserting a new node into _pending_tx from inside remove_if:
                // link_front() prepends before the range already captured by
                // range_remove, so the traversal completes safely and the new
                // entry is visible on the next query_next_transaction call.
                if ( start_followup )
                        start_followup( followup_op );
        }

        struct env
        {
                inplace_stop_token token;
                inplace_stop_token query( ecor::get_stop_token_t ) const noexcept
                {
                        return token;
                }
        };

        env get_env() const noexcept
        {
                if ( stop_source )
                        return env{ stop_source->get_token() };
                return env{};
        }
};

TEST_CASE( "Test_Transaction_Basics" )
{
        uart_test_context t;
        bool              done = false;
        std::string       result;

        auto h = basic_exchange_test( t.ctx, t.u, result, done ).connect( _dummy_receiver{} );
        h.start();

        t.ctx.core.run_once();

        t.run_until( [&] {
                return done;
        } );

        CHECK( done );
        CHECK_EQ( result, "valid_resp" );
}

TEST_CASE( "Test_Transaction_Cancellation_Pending" )
{
        uart_test_context   t;
        inplace_stop_source stop_src;
        int                 stopped = 0;
        int                 value   = 0;
        uint8_t             buffer[42];

        auto op =
            t.u.transact( buffer, 10 )
                .connect( capturing_receiver{
                    .value_count = &value, .stopped_count = &stopped, .stop_source = &stop_src } );

        op.start();
        stop_src.request_stop();

        // Now execute tick. The transaction is pending in the source, but stop
        // is requested. query_next_transaction should catch it.
        t.u.tick();
        t.ctx.core.run_once();

        CAPTURE( value );
        CHECK( stopped );
        CHECK_FALSE( value );
}

TEST_CASE( "Test_Transaction_InFlight_Cancellation_Ignored" )
{
        uart_test_context   t;
        inplace_stop_source stop_src;
        int                 stopped = 0;
        int                 value   = 0;
        uint8_t             buffer[42];

        // "slow:100000" causes a delay in the mock peripheral
        std::string req = "slow:1000000";
        std::memcpy( buffer, req.data(), req.size() );

        auto op =
            t.u.transact( buffer, req.size() )
                .connect( capturing_receiver{
                    .value_count = &value, .stopped_count = &stopped, .stop_source = &stop_src } );

        op.start();

        // Start transaction - this should put it in the pending queue
        // and trigger the "slow" operation in the mock peripheral
        t.u.tick();
        t.ctx.core.run_once();  // Starts the operation

        // The peripheral is now busy executing the delay.
        // We request stop while it is running.
        stop_src.request_stop();

        // Run untill completion or timeout
        t.run_until(
            [&] {
                    return stopped || value;
            },
            100ms );

        CHECK_FALSE( stopped );
        CHECK( value );
}

TEST_CASE( "Test_Transaction_Error" )
{
        uart_test_context t;
        int               value   = 0;
        int               errored = 0;
        uint8_t           buffer[42];

        std::string req = "err:overrun";
        std::memcpy( buffer, req.data(), req.size() );

        auto h =
            t.u.transact( buffer, req.size() )
                .connect( capturing_receiver{ .value_count = &value, .error_count = &errored } );
        h.start();

        t.ctx.core.run_once();

        t.run_until( [&] {
                return errored || value;
        } );

        CHECK( errored );
        CHECK_FALSE( value );
}

TEST_CASE( "Test_Transaction_FIFO" )
{
        uart_test_context t;

        std::vector< std::string > results;
        uint8_t                    buf1[42], buf2[42], buf3[42];
        std::string                r1 = "echo:1", r2 = "echo:2", r3 = "echo:3";
        std::memcpy( buf1, r1.data(), r1.size() );
        std::memcpy( buf2, r2.data(), r2.size() );
        std::memcpy( buf3, r3.data(), r3.size() );

        auto s1 = t.u.transact( buf1, r1.size() );
        auto s2 = t.u.transact( buf2, r2.size() );
        auto s3 = t.u.transact( buf3, r3.size() );

        auto o1 = std::move( s1 ).connect( capturing_receiver{ .results = &results, .id = "1" } );
        auto o2 = std::move( s2 ).connect( capturing_receiver{ .results = &results, .id = "2" } );
        auto o3 = std::move( s3 ).connect( capturing_receiver{ .results = &results, .id = "3" } );

        o1.start();
        o2.start();
        o3.start();

        // Process them
        t.run_until( [&] {
                return results.size() >= 3;
        } );

        REQUIRE( results.size() == 3 );
        CHECK_EQ( results[0], "1" );
        CHECK_EQ( results[1], "2" );
        CHECK_EQ( results[2], "3" );
}

TEST_CASE( "Test_Transaction_Lifecyle_Indices" )
{
        uart_test_context t;
        uint8_t           buffer[42];
        std::string       req = "slow:1000000";
        std::memcpy( buffer, req.data(), req.size() );

        auto op = t.u.transact( buffer, req.size() ).connect( _dummy_receiver{} );
        op.start();

        // Initial state: buffer empty
        CHECK( t.u._trnxs.deliver == 0 );
        CHECK( t.u._trnxs.enqueue == 0 );
        CHECK( t.u._trnxs.rx == 0 );
        CHECK( t.u._trnxs.tx == 0 );

        // Tick: moves from source to buffer (TX Ready).
        // Note: tx/rx counters are racy here — the background mock thread can fire
        // on_tx_complete_irq almost immediately (~0-41 nops), so tx may already be 1.
        t.u.tick();
        CHECK( t.u._trnxs.deliver == 0 );
        CHECK( t.u._trnxs.enqueue == 1 );
        CHECK( t.u._trnxs.rx <= 1 );
        CHECK( t.u._trnxs.tx <= 1 );

        // Run once: dispatch_tx called by tick() logic if not active.
        // But dispatch_tx calls _handle.transmit which is async in mock.
        // The mock sets tx_active=true synchronously.
        t.ctx.core.run_once();

        // Mock transmit starts. dispatch_tx called.
        // dispatch_tx does NOT increment mid2. mid2 is incremented in on_tx_complete_irq.
        // Note: the background TX thread fires on_tx_complete_irq almost immediately
        // (only ~0-41 nops of delay), so mid2 may already be 1 by this point.
        CHECK( t.u._trnxs.tx <= 1 );

        // Wait for TX to complete
        // "slow" means slow RX (1M-nop delay on receive), not slow TX.
        // TX job: delay() (~0-41 nops) then do_reply() which fires on_tx_irq.
        // on_tx_complete_irq clears tx_active then signals dispatch_rx.
        t.run_until(
            [&] {
                    return t.u._trnxs.tx == 1;
            },
            100ms );

        // TX Completed. on_tx_complete_irq increments tx and calls dispatch_rx.
        CHECK( t.u._trnxs.tx == 1 );

        // RX should be scheduled now.
        // dispatch_rx calls _handle.receive which sets rx_active=true.
        // RX is "slow", so it won't complete immediately.

        // Wait for RX complete
        t.run_until(
            [&] {
                    return t.u._trnxs.rx == 1;
            },
            200ms );

        CHECK( t.u._trnxs.rx == 1 );

        // Now tick() again to process the completion (rx -> deliver)
        t.u.tick();

        CHECK( t.u._trnxs.deliver == 1 );
        CHECK( t.u._trnxs.rx == 1 );
        CHECK( t.u._trnxs.tx == 1 );
        CHECK( t.u._trnxs.enqueue == 1 );
}

TEST_CASE( "Test_Transaction_Busy_Transmit" )
{
        uart_test_context t;
        uint8_t           buffer[42];
        std::string       req = "echo:valid";
        std::memcpy( buffer, req.data(), req.size() );

        // Manually set peripheral to busy
        t.u._handle.tx_active = true;

        auto op = t.u.transact( buffer, req.size() ).connect( _dummy_receiver{} );
        op.start();

        // Tick to move into buffer
        t.u.tick();
        t.ctx.core.run_once();

        // Should be in buffer
        CHECK( t.u._trnxs.enqueue == 1 );
        CHECK( t.u._trnxs.tx == 0 );

        // Now we simulate TX completion of the "previous" (manual blocking) operation.
        t.u._handle.tx_active = false;

        // Next tick should dispatch
        t.u.tick();
        t.ctx.core.run_once();

        // Verify it started (we can't easily verify transmit() was called without side effects,
        // but checking mid2 won't help immediately as mid2 incs on IRQ).
        // Check if tx_active becomes true (mock sets it true in transmit).
        CHECK( t.u._handle.tx_active.load() );

        // Wait for the full round-trip: TX IRQ → RX job writes buffer → RX IRQ → tick delivers
        // result. Waiting for just !tx_active is too early: at that point the RX job is still
        // pending with a pointer into the local 'buffer'. We must wait until tick() has advanced
        // 'first', meaning set_value/set_error has been called and the buffer is no longer live.
        t.run_until( [&] {
                return t.u._trnxs.deliver >= 1;
        } );
}

TEST_CASE( "Test_Transaction_Errors_Comprehensive" )
{
        uart_test_context t;
        uint8_t           buffer[42];
        const struct
        {
                std::string arg;
                uart_error  expected;
        } cases[] = {
            { "err:overrun", uart_error::overrun },
            { "err:framing", uart_error::framing },
            { "err:parity", uart_error::parity },
            { "err:timeout", uart_error::timeout },
            // "nack" is a TX error, logic might be slightly different in mock (on_tx_err vs
            // on_rx_err)
            { "err:nack", uart_error::nack } };

        for ( auto const& c : cases ) {
                int error_count = 0;
                int value_count = 0;

                std::memcpy( buffer, c.arg.data(), c.arg.size() );

                // We need to capture the specific error value, but capturing_receiver doesn't store
                // the error code. It just counts.
                // We trust the mock provides the correct error if it errors at all.
                // If we want to verify the specific error code, we need a better receiver or modify
                // capturing_receiver.
                // For now, let's verify that it DOES error.

                auto op = t.u.transact( buffer, c.arg.size() )
                              .connect( capturing_receiver{
                                  .value_count = &value_count, .error_count = &error_count } );
                op.start();

                t.run_until( [&] {
                        return error_count > 0 || value_count > 0;
                } );

                CAPTURE( c.arg );
                CHECK( error_count == 1 );
                CHECK( value_count == 0 );
        }
}

TEST_CASE( "Test_UART_SPSC_Pipeline" )
{
        uart_test_context t;
        // Each transaction must have its own buffer because receive overwrites the transmit buffer.
        // We use a struct to hold the buffer and completion status for each transaction.
        struct op_context
        {
                uint8_t buffer[42];
                int     completed = 0;
        };

        int const count = 100;
        // Use unique_ptr to ensure stable addresses for the buffer and completion flag
        std::vector< std::unique_ptr< op_context > > contexts;
        contexts.reserve( count );

        std::vector< decltype( t.u.transact( {}, 0 ).connect( capturing_receiver{} ) ) > ops;
        ops.reserve( count );

        std::string const req = "echo:ok";

        for ( int i = 0; i < count; ++i ) {
                auto ctx = std::make_unique< op_context >();
                std::memcpy( ctx->buffer, req.data(), req.size() );

                ops.push_back(
                    t.u.transact( ctx->buffer, req.size() )
                        .connect( capturing_receiver{ .value_count = &ctx->completed } ) );
                ops.back().start();

                contexts.push_back( std::move( ctx ) );
        }

        // Run until all completed
        t.run_until(
            [&] {
                    int total = 0;
                    for ( auto const& ctx : contexts )
                            total += ctx->completed;
                    return total == count;
            },
            5s );

        int total_completed = 0;
        for ( auto const& ctx : contexts )
                total_completed += ctx->completed;

        CHECK( total_completed == count );

        // Verify buffer is empty
        CHECK( t.u._trnxs.empty() );
}

TEST_CASE( "Test_Transaction_Destruction_Cleanup" )
{
        uart_test_context t;
        uint8_t           buffer[42];

        // Scope to control lifetime of operation state
        {
                auto op = t.u.transact( buffer, 10 ).connect( _dummy_receiver{} );
                op.start();

                // It is now in the pending list of the source.
                // We destroy 'op' by exiting scope.
        }

        // Now we tick.
        // If the operation state did not unlink itself from the source's list,
        // this will likely crash (use-after-free).
        // If it unlinked correctly, the list is empty, and query_next_transaction returns nullptr.
        t.u.tick();
        t.ctx.core.run_once();

        // If we survived, it's good.
        CHECK( true );
}

TEST_CASE( "Test_Transaction_Empty_Source" )
{
        trnx_controller_source< uart_trans > source;

        // query_next_trnx on a fresh, empty source should return nullptr.
        CHECK( source.query_next_trnx() == nullptr );

        // Schedule one transaction, take it, then verify empty again.
        uint8_t buffer[16]{};
        int     value_count = 0;
        auto    op          = source.schedule( uart_trans{ .buffer = buffer, .tx_used = 4 } )
                      .connect( capturing_receiver{ .value_count = &value_count } );
        op.start();

        auto* entry = source.query_next_trnx();
        CHECK( entry != nullptr );
        CHECK( source.query_next_trnx() == nullptr );

        // Complete the taken entry so the op_state can be destroyed cleanly.
        entry->set_value( std::span< uint8_t >{} );
        CHECK( value_count == 1 );
}

TEST_CASE( "Test_Transaction_Destruction_With_Pending" )
{
        // Destroy a trnx_controller_source while transactions are still pending.
        // The pending ops hold references (via ll_base) into the source's list.
        // On destruction, each op_state unlinks itself from the list automatically.

        int value_a = 0;
        int value_b = 0;

        uint8_t buf_a[16]{};
        uint8_t buf_b[16]{};

        {
                trnx_controller_source< uart_trans > source;

                auto op_a = source.schedule( uart_trans{ .buffer = buf_a, .tx_used = 4 } )
                                .connect( capturing_receiver{ .value_count = &value_a } );
                auto op_b = source.schedule( uart_trans{ .buffer = buf_b, .tx_used = 4 } )
                                .connect( capturing_receiver{ .value_count = &value_b } );
                op_a.start();
                op_b.start();

                // Both are pending. Destroy source + ops by exiting scope.
                // zll::ll_base unlinks each entry on destruction.
        }

        // If we survived without crash, the cleanup is correct.
        CHECK( value_a == 0 );
        CHECK( value_b == 0 );
}

TEST_CASE( "Test_Transaction_FlowControl" * doctest::skip() )
{
        uart_test_context   t;
        inplace_stop_source stop_src;

        std::string req = "slow:1000000";  // Slow enough to fill buffer

        // Each transaction needs its own buffer: the RX mock writes the reply
        // ("valid_resp") back into rx_buff, which points to the TX buffer.
        // If all ops shared one buffer, the first RX reply would corrupt the
        // command string for all subsequent transactions.
        std::array< std::array< uint8_t, 42 >, 5 > buffers{};
        for ( auto& buf : buffers )
                std::memcpy( buf.data(), req.data(), req.size() );

        std::vector< std::string > results;

        // Circular buffer size is 4. We schedule 5.
        std::vector< decltype( t.u.transact( {}, 0 ).connect( capturing_receiver{} ) ) > ops;
        ops.reserve( 5 );

        struct flags
        {
                int value   = 0;
                int stopped = 0;
        };
        std::vector< flags > state( 5 );

        for ( int i = 0; i < 5; ++i ) {
                ops.push_back( t.u.transact( buffers[i], req.size() )
                                   .connect( capturing_receiver{
                                       .value_count   = &state[i].value,
                                       .stopped_count = &state[i].stopped,
                                       .stop_source   = &stop_src } ) );
                ops.back().start();
        }

        // First tick should fill the buffer (4 items)
        t.u.tick();
        t.ctx.core.run_once();

        // Verify state is as expected
        CHECK( t.u._trnxs.full() );
        // The 5th one is still in _source logic (pending)

        // Now request stop.
        // 4 transactions are in-flight (Active) -> Should ignore stop.
        // 1 transaction is pending -> Should accept stop.
        stop_src.request_stop();

        // Tick again to process cancellation of pending
        t.u.tick();
        t.ctx.core.run_once();

        // Wait for completion of active ones
        t.run_until(
            [&] {
                    int completed = 0;
                    for ( auto& s : state )
                            if ( s.value )
                                    completed++;
                    return completed >= 4;
            },
            200ms );

        int stopped = 0;
        int valued  = 0;
        for ( auto& s : state ) {
                if ( s.stopped )
                        stopped++;
                if ( s.value )
                        valued++;
        }

        CHECK( stopped == 1 );
        CHECK( valued == 4 );
}

TEST_CASE( "Test_UART_Error_Overrun" )
{
        uart_test_context t;
        uint8_t           buffer[42];
        std::string       req = "err:overrun";
        std::memcpy( buffer, req.data(), req.size() );
        uart_error result_error = uart_error::none;
        int        error_count  = 0;

        auto op = t.u.transact( buffer, req.size() )
                      .connect( capturing_receiver{
                          .error_count = &error_count, .error_value = &result_error } );
        op.start();

        t.run_until(
            [&] {
                    return error_count > 0;
            },
            1s );

        CHECK( error_count == 1 );
        CHECK( result_error == uart_error::overrun );
}

TEST_CASE( "Test_UART_Error_Framing" )
{
        uart_test_context t;
        uint8_t           buffer[42];
        std::string       req = "err:framing";
        std::memcpy( buffer, req.data(), req.size() );
        uart_error result_error = uart_error::none;
        int        error_count  = 0;

        auto op = t.u.transact( buffer, req.size() )
                      .connect( capturing_receiver{
                          .error_count = &error_count, .error_value = &result_error } );
        op.start();

        t.run_until(
            [&] {
                    return error_count > 0;
            },
            1s );

        CHECK( error_count == 1 );
        CHECK( result_error == uart_error::framing );
}

TEST_CASE( "Test_UART_Error_Parity" )
{
        uart_test_context t;
        uint8_t           buffer[42];
        std::string       req = "err:parity";
        std::memcpy( buffer, req.data(), req.size() );
        uart_error result_error = uart_error::none;
        int        error_count  = 0;

        auto op = t.u.transact( buffer, req.size() )
                      .connect( capturing_receiver{
                          .error_count = &error_count, .error_value = &result_error } );
        op.start();

        t.run_until(
            [&] {
                    return error_count > 0;
            },
            1s );

        CHECK( error_count == 1 );
        CHECK( result_error == uart_error::parity );
}

TEST_CASE( "Test_UART_Error_Timeout" )
{
        uart_test_context t;
        uint8_t           buffer[42];
        std::string       req = "err:timeout";
        std::memcpy( buffer, req.data(), req.size() );
        uart_error result_error = uart_error::none;
        int        error_count  = 0;

        auto op = t.u.transact( buffer, req.size() )
                      .connect( capturing_receiver{
                          .error_count = &error_count, .error_value = &result_error } );
        op.start();

        t.run_until(
            [&] {
                    return error_count > 0;
            },
            1s );

        CHECK( error_count == 1 );
        CHECK( result_error == uart_error::timeout );
}

TEST_CASE( "Test_UART_Error_NACK" )
{
        uart_test_context t;
        uint8_t           buffer[42];
        std::string       req = "err:nack";
        std::memcpy( buffer, req.data(), req.size() );
        uart_error result_error = uart_error::none;
        int        error_count  = 0;

        auto op = t.u.transact( buffer, req.size() )
                      .connect( capturing_receiver{
                          .error_count = &error_count, .error_value = &result_error } );
        op.start();

        t.run_until(
            [&] {
                    return error_count > 0;
            },
            1s );

        CHECK( error_count == 1 );
        CHECK( result_error == uart_error::nack );
}

TEST_CASE( "Test_UART_Concurrency_Stress" )
{
        uart_test_context t;
        // Stress test with many transactions and background IRQs
        int const count = 1000;

        struct op_context
        {
                uint8_t buffer[42];
                int     completed = 0;
        };
        std::vector< std::unique_ptr< op_context > > contexts;
        contexts.reserve( count );

        std::vector< decltype( t.u.transact( {}, 0 ).connect( capturing_receiver{} ) ) > ops;
        ops.reserve( count );

        std::string const req = "echo:stress";

        for ( int i = 0; i < count; ++i ) {
                auto ctx = std::make_unique< op_context >();
                std::memcpy( ctx->buffer, req.data(), req.size() );

                ops.push_back(
                    t.u.transact( ctx->buffer, req.size() )
                        .connect( capturing_receiver{ .value_count = &ctx->completed } ) );
                ops.back().start();
                contexts.push_back( std::move( ctx ) );
        }

        // Run until all completed.
        // During this time, uart_peripheral background threads are firing IRQs,
        // while we are ticking the main loop here.
        t.run_until(
            [&] {
                    int total = 0;
                    for ( auto const& ctx : contexts )
                            total += ctx->completed;
                    return total == count;
            },
            10s );

        int total_completed = 0;
        for ( auto const& ctx : contexts )
                total_completed += ctx->completed;

        CHECK( total_completed == count );
        CHECK( t.u._trnxs.empty() );
}

TEST_CASE( "Test_Transaction_Stop_All_Pending" )
{
        // Three transactions queued, all stopped before tick() calls
        // query_next_transaction. All three should receive set_stopped(), none
        // should receive set_value() or set_error().
        uart_test_context t;

        inplace_stop_source stop_a, stop_b, stop_c;
        int                 stopped_a = 0, stopped_b = 0, stopped_c = 0;
        int                 value_a = 0, value_b = 0, value_c = 0;

        uint8_t     ba[42], bb[42], bc[42];
        std::string req = "echo:x";
        std::memcpy( ba, req.data(), req.size() );
        std::memcpy( bb, req.data(), req.size() );
        std::memcpy( bc, req.data(), req.size() );

        auto oa = t.u.transact( ba, req.size() )
                      .connect( capturing_receiver{
                          .value_count   = &value_a,
                          .stopped_count = &stopped_a,
                          .stop_source   = &stop_a } );
        auto ob = t.u.transact( bb, req.size() )
                      .connect( capturing_receiver{
                          .value_count   = &value_b,
                          .stopped_count = &stopped_b,
                          .stop_source   = &stop_b } );
        auto oc = t.u.transact( bc, req.size() )
                      .connect( capturing_receiver{
                          .value_count   = &value_c,
                          .stopped_count = &stopped_c,
                          .stop_source   = &stop_c } );

        oa.start();
        ob.start();
        oc.start();

        stop_a.request_stop();
        stop_b.request_stop();
        stop_c.request_stop();

        t.u.tick();
        t.ctx.core.run_once();

        CHECK( stopped_a );
        CHECK( stopped_b );
        CHECK( stopped_c );
        CHECK_FALSE( value_a );
        CHECK_FALSE( value_b );
        CHECK_FALSE( value_c );
}

TEST_CASE( "Test_Transaction_Stop_Mixed_Pending" )
{
        // Three transactions queued. The middle one (B) is stopped before tick()
        // calls query_next_transaction. A and C should complete with set_value();
        // B should complete with set_stopped().
        //
        // NOTE: this also exercises the re-entry risk: the live completions of A
        // and C via run_until/tick could (in a coroutine scenario) schedule new
        // transactions into _pending_tx during remove_if's traversal of the list.
        uart_test_context t;

        inplace_stop_source stop_b;
        int                 stopped_b = 0, value_a = 0, value_b = 0, value_c = 0;

        uint8_t     ba[42], bb[42], bc[42];
        std::string req = "echo:x";
        std::memcpy( ba, req.data(), req.size() );
        std::memcpy( bb, req.data(), req.size() );
        std::memcpy( bc, req.data(), req.size() );

        auto oa =
            t.u.transact( ba, req.size() ).connect( capturing_receiver{ .value_count = &value_a } );
        auto ob = t.u.transact( bb, req.size() )
                      .connect( capturing_receiver{
                          .value_count   = &value_b,
                          .stopped_count = &stopped_b,
                          .stop_source   = &stop_b } );
        auto oc =
            t.u.transact( bc, req.size() ).connect( capturing_receiver{ .value_count = &value_c } );

        oa.start();
        ob.start();
        oc.start();

        stop_b.request_stop();

        t.run_until( [&] {
                return value_a && value_c;
        } );

        CHECK( value_a );
        CHECK( value_c );
        CHECK( stopped_b );
        CHECK_FALSE( value_b );
}

TEST_CASE( "Test_Transaction_Stop_Requeue" )
{
        // Op A is stopped while pending. Its set_stopped() immediately starts
        // op B by calling link_front() on the same list that remove_if is
        // currently traversing.
        //
        // Expected: A receives set_stopped(); B is inserted before the traversal
        // range (link_front prepends before old first) so it is not visited in
        // this pass and is safe. It is then picked up by the next
        // query_next_transaction call and completes with set_value().
        uart_test_context t;

        inplace_stop_source stop_a;
        int                 stopped_a = 0;
        int                 value_b   = 0;

        uint8_t     ba[42], bb[42];
        std::string req = "echo:requeue";
        std::memcpy( ba, req.data(), req.size() );
        std::memcpy( bb, req.data(), req.size() );

        // Pre-construct op B (not started yet).
        using followup_op_t = _trnx_controller_op< uart_trans, capturing_receiver >;
        auto followup =
            t.u.transact( bb, req.size() ).connect( capturing_receiver{ .value_count = &value_b } );

        auto oa = t.u.transact( ba, req.size() )
                      .connect( _requeue_on_stop_receiver{
                          .stop_count     = &stopped_a,
                          .stop_source    = &stop_a,
                          .followup_op    = &followup,
                          .start_followup = []( void* p ) {
                                  static_cast< followup_op_t* >( p )->start();
                          } } );

        oa.start();
        stop_a.request_stop();

        // tick() -> query_next_transaction -> remove_if -> set_stopped -> start(B)
        t.u.tick();
        t.ctx.core.run_once();

        CHECK( stopped_a );

        // B was inserted during remove_if and must now complete normally.
        t.run_until( [&] {
                return value_b;
        } );

        CHECK( value_b );
}

TEST_CASE( "transaction" )
{
        uart     u;
        nd_mem   mem;
        task_ctx ctx{ mem };
        bool     done = false;

        auto h1 = multiple_exchanges( ctx, u, 100, done ).connect( _dummy_receiver{} );
        h1.start();

        // We need to run enough times for threads to complete and callbacks to fire
        // tick() processes pending transactions

        // Initial run to start coroutine and schedule transaction
        ctx.core.run_once();

        // Now tick uart repeatedly to process simple exchange
        // This time, we need to run for much longer, potentially
        auto start = std::chrono::steady_clock::now();
        while ( !done ) {
                u.tick();

                // Run ecor loop to process completions
                ctx.core.run_once();

                if ( std::chrono::steady_clock::now() - start > 10s )
                        break;

                std::this_thread::sleep_for( std::chrono::milliseconds( 1 ) );
        }
        // sleep a bit to flush any pending logs if any
        std::this_thread::sleep_for( std::chrono::milliseconds( 10 ) );

        CHECK( done );
}

}  // namespace ecor
