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

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace ecor
{
namespace
{

        /// Payload with a distinct, larger footprint than `int`, used to check that nodes of very
        /// different sizes coexist in one queue.
        struct blob
        {
                uint8_t data[64]{};
                int     tag = 0;

                blob() = default;
                explicit blob( int t )
                  : tag( t )
                {
                }
        };

        /// Counts live instances so tests can assert that payloads are destroyed exactly once.
        struct tracked
        {
                static inline int alive = 0;

                int id = 0;

                explicit tracked( int i )
                  : id( i )
                {
                        ++alive;
                }
                tracked( tracked&& o ) noexcept
                  : id( o.id )
                {
                        ++alive;
                }
                tracked& operator=( tracked&& ) = delete;
                tracked( tracked const& )       = delete;
                ~tracked()
                {
                        --alive;
                }
        };

        /// Counts move-constructions, so a test can show that delivery itself performs none.
        struct counted
        {
                static inline int moves = 0;

                int id = 0;

                explicit counted( int i )
                  : id( i )
                {
                }
                counted( counted&& o ) noexcept
                  : id( o.id )
                {
                        ++moves;
                }
                counted& operator=( counted&& ) = delete;
                counted( counted const& )       = delete;
        };

        /// Reads the delivered event without keeping it, which is all the borrowed reference
        /// allows.
        struct borrow_recv
        {
                using receiver_concept = receiver_t;

                int* seen;

                void set_value( counted& ev )
                {
                        *seen = ev.id;
                }

                void set_stopped()
                {
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };

        /// Receiver for `pop()`: records every completion as a string so ordering is easy to
        /// assert.
        struct pop_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;
                inplace_stop_token          token{};

                void set_value( int v )
                {
                        log->push_back( "int:" + std::to_string( v ) );
                }

                void set_value( std::string v )
                {
                        log->push_back( "str:" + v );
                }

                void set_stopped()
                {
                        log->push_back( "stopped" );
                }

                [[nodiscard]] stop_token_env< inplace_stop_token > get_env() const noexcept
                {
                        return { token };
                }
        };

        /// Same as `pop_recv`, but with no stop token in its environment — exercises the branch
        /// where no `inplace_stop_callback` is registered at all.
        struct plain_pop_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;

                void set_value( int v )
                {
                        log->push_back( "int:" + std::to_string( v ) );
                }

                void set_value( std::string v )
                {
                        log->push_back( "str:" + v );
                }

                void set_stopped()
                {
                        log->push_back( "stopped" );
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };

        /// Receiver for `push()` and `close()`.
        struct sig_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;
                std::string                 name;
                inplace_stop_token          token{};

                void set_value()
                {
                        log->push_back( name + ":value" );
                }

                void set_stopped()
                {
                        log->push_back( name + ":stopped" );
                }

                [[nodiscard]] stop_token_env< inplace_stop_token > get_env() const noexcept
                {
                        return { token };
                }
        };

        /// Everything opted in — most tests exercise cancellation and shutdown, which the
        /// default configuration deliberately omits.
        struct nd_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr bool is_consumer_stoppable = true;
                static constexpr bool is_producer_stoppable = true;
                static constexpr bool is_closeable          = true;
        };

        struct cb_cfg : queue_default_cfg< circular_buffer_memory< uint16_t > >
        {
                static constexpr bool is_consumer_stoppable = true;
                static constexpr bool is_producer_stoppable = true;
                static constexpr bool is_closeable          = true;
        };

        /// Closeable, but neither waiter kind reacts to a stop token.
        struct unstoppable_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr bool is_closeable = true;
        };

        template < typename T >
        concept _has_queue_list = requires { typename _queue_list< T >; };

        template < typename Q >
        concept _has_core_member = requires( Q& q ) { q._core; };

        template < typename Q >
        concept _has_emplace = requires( Q& q ) { q.template _emplace< int >( 0 ); };

        template < typename Q >
        concept _has_close = requires( Q& q ) { q.close(); };

        template < typename Q >
        concept _has_closed = requires( Q& q ) { q.closed(); };

        /// The default configuration: no cancellation and no shutdown protocol, so `pop()`
        /// and `push()` complete only with `set_value`.
        using minimal_cfg = queue_default_cfg< nd_mem >;

        /// Receiver with no `set_stopped()` at all — only connectable to a queue whose
        /// completion signatures omit it.
        struct value_only_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;

                void set_value( int v )
                {
                        log->push_back( "int:" + std::to_string( v ) );
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };

        /// Consumers stay cancellable, producers do not.
        struct cb_no_producer_stop_cfg : queue_default_cfg< circular_buffer_memory< uint16_t > >
        {
                static constexpr bool is_consumer_stoppable = true;
                static constexpr bool is_closeable          = true;
        };

        using q_t = async_queue< nd_cfg, int, std::string >;

}  // namespace

TEST_CASE( "inplace_variant - empty state" )
{
        inplace_variant< int, std::string > v;

        CHECK( !v );
        CHECK( v.index() == v.npos );
        CHECK( !v.holds< int >() );
        CHECK( v.get_if< int >() == nullptr );

        int visited = 0;
        CHECK( !v.visit( [&]( auto& ) {
                ++visited;
        } ) );
        CHECK( visited == 0 );
}

TEST_CASE( "inplace_variant - emplace, holds, get and visit" )
{
        q_t::value_type v;
        CHECK( v.emplace< int >( 42 ) == 42 );

        CHECK( v );
        CHECK( v.index() == 0 );
        CHECK( v.holds< int >() );
        CHECK( !v.holds< std::string >() );
        CHECK( v.get< int >() == 42 );
        REQUIRE( v.get_if< int >() != nullptr );
        CHECK( *v.get_if< int >() == 42 );
        CHECK( v.get_if< std::string >() == nullptr );

        int seen = 0;
        CHECK( v.visit( [&]( auto& x ) {
                if constexpr ( std::same_as< std::remove_reference_t< decltype( x ) >, int > )
                        seen = x;
        } ) );
        CHECK( seen == 42 );
}

TEST_CASE( "inplace_variant - storage fits the largest alternative" )
{
        using v_t = inplace_variant< uint8_t, blob >;
        static_assert( v_t::storage_size == sizeof( blob ) );
        static_assert( alignof( v_t ) >= alignof( blob ) );

        v_t v;
        v.emplace< blob >( 5 );
        CHECK( v.holds< blob >() );
        CHECK( v.get< blob >().tag == 5 );

        v.emplace< uint8_t >( 9 );
        CHECK( v.holds< uint8_t >() );
        CHECK( !v.holds< blob >() );
        CHECK( v.get< uint8_t >() == 9 );
}

TEST_CASE( "inplace_variant - emplace_empty constructs without a preceding reset" )
{
        tracked::alive = 0;
        {
                inplace_variant< tracked, int > v;
                CHECK( v.emplace_empty< tracked >( 4 ).id == 4 );
                CHECK( v.holds< tracked >() );
                CHECK( v.index() == 0 );
                CHECK( tracked::alive == 1 );

                // Same result as emplace() on an empty variant.
                inplace_variant< tracked, int > w;
                w.emplace< tracked >( 4 );
                CHECK( w.index() == v.index() );
                CHECK( w.get< tracked >().id == v.get< tracked >().id );
        }
        CHECK( tracked::alive == 0 );
}

TEST_CASE( "inplace_variant - emplace over an existing value destroys it" )
{
        tracked::alive = 0;
        {
                inplace_variant< tracked, int > v;
                v.emplace< tracked >( 1 );
                CHECK( tracked::alive == 1 );

                v.emplace< tracked >( 2 );
                CHECK( tracked::alive == 1 );
                CHECK( v.get< tracked >().id == 2 );

                v.emplace< int >( 3 );
                CHECK( tracked::alive == 0 );
                CHECK( v.holds< int >() );
        }
        CHECK( tracked::alive == 0 );
}

TEST_CASE( "inplace_variant - move and reset destroy the payload once" )
{
        tracked::alive = 0;
        {
                inplace_variant< tracked > a;
                a.emplace< tracked >( 7 );
                CHECK( tracked::alive == 1 );

                inplace_variant< tracked > b{ std::move( a ) };
                CHECK( !a );
                CHECK( b );
                CHECK( b.get< tracked >().id == 7 );
                CHECK( tracked::alive == 1 );

                inplace_variant< tracked > c;
                c = std::move( b );
                CHECK( !b );
                CHECK( c.get< tracked >().id == 7 );
                CHECK( tracked::alive == 1 );

                c.reset();
                CHECK( !c );
                CHECK( tracked::alive == 0 );
        }
        CHECK( tracked::alive == 0 );
}

TEST_CASE( "inplace_variant - destructor releases the payload" )
{
        tracked::alive = 0;
        {
                inplace_variant< tracked > v;
                v.emplace< tracked >( 1 );
                CHECK( tracked::alive == 1 );
        }
        CHECK( tracked::alive == 0 );
}

TEST_CASE( "async_queue - synchronous push and pop keep one FIFO across types" )
{
        nd_mem mem;
        q_t    q{ mem };

        CHECK( q.empty() );
        CHECK( q.size() == 0 );
        CHECK( !q.closed() );

        CHECK( q.try_push( 1 ) );
        CHECK( q.try_push( std::string{ "a" } ) );
        CHECK( q.try_push( 2 ) );

        CHECK( q.size() == 3 );
        CHECK( !q.empty() );

        auto h1 = q.try_pop();
        REQUIRE( h1 );
        CHECK( h1.holds< int >() );
        CHECK( h1.get< int >() == 1 );

        auto h2 = q.try_pop();
        REQUIRE( h2 );
        CHECK( h2.holds< std::string >() );
        CHECK( h2.get< std::string >() == "a" );

        auto h3 = q.try_pop();
        REQUIRE( h3 );
        CHECK( h3.holds< int >() );
        CHECK( h3.get< int >() == 2 );

        CHECK( q.empty() );
        CHECK( !q.try_pop() );
}

TEST_CASE( "async_queue - elements of very different sizes coexist" )
{
        nd_mem                               mem;
        async_queue< nd_cfg, uint8_t, blob > q{ mem };

        CHECK( q.try_push( uint8_t{ 3 } ) );
        CHECK( q.try_push( blob{ 9 } ) );

        auto a = q.try_pop();
        REQUIRE( a );
        CHECK( a.holds< uint8_t >() );
        CHECK( a.get< uint8_t >() == 3 );

        auto b = q.try_pop();
        REQUIRE( b );
        CHECK( b.holds< blob >() );
        CHECK( b.get< blob >().tag == 9 );
}

TEST_CASE( "async_queue - destructor destroys items left in the queue" )
{
        tracked::alive = 0;
        {
                nd_mem                         mem;
                async_queue< nd_cfg, tracked > q{ mem };

                CHECK( q.try_push( tracked{ 1 } ) );
                CHECK( q.try_push( tracked{ 2 } ) );
                CHECK( tracked::alive == 2 );
        }
        CHECK( tracked::alive == 0 );
}

TEST_CASE( "async_queue - pop completes immediately when an item is waiting" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;

        CHECK( q.try_push( 5 ) );

        auto op = q.pop().connect( pop_recv{ &log } );
        op.start();

        CHECK( log == std::vector< std::string >{ "int:5" } );
        CHECK( q.empty() );
}

TEST_CASE( "async_queue - pop parks and is completed by a later push" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;

        auto op = q.pop().connect( pop_recv{ &log } );
        op.start();
        CHECK( log.empty() );

        CHECK( q.try_push( std::string{ "hi" } ) );

        CHECK( log == std::vector< std::string >{ "str:hi" } );
        CHECK( q.empty() );
}

TEST_CASE( "async_queue - pop works without a stop token in the environment" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;

        auto op = q.pop().connect( plain_pop_recv{ &log } );
        op.start();
        CHECK( log.empty() );

        CHECK( q.try_push( 11 ) );
        CHECK( log == std::vector< std::string >{ "int:11" } );
}

TEST_CASE( "async_queue - parked consumers are served in FIFO order" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > a_log;
        std::vector< std::string > b_log;

        auto op_a = q.pop().connect( pop_recv{ &a_log } );
        auto op_b = q.pop().connect( pop_recv{ &b_log } );
        op_a.start();
        op_b.start();

        CHECK( q.try_push( 1 ) );
        CHECK( a_log == std::vector< std::string >{ "int:1" } );
        CHECK( b_log.empty() );

        CHECK( q.try_push( 2 ) );
        CHECK( b_log == std::vector< std::string >{ "int:2" } );
}

TEST_CASE( "async_queue - a destroyed parked consumer unlinks itself" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;

        {
                auto op = q.pop().connect( pop_recv{ &log } );
                op.start();
        }

        CHECK( q.try_push( 1 ) );
        CHECK( log.empty() );
        CHECK( q.size() == 1 );
}

TEST_CASE( "async_queue - cancelling a parked consumer completes it eagerly" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;
        inplace_stop_source        src;

        auto op = q.pop().connect( pop_recv{ &log, src.get_token() } );
        op.start();
        CHECK( log.empty() );

        src.request_stop();
        CHECK( log == std::vector< std::string >{ "stopped" } );

        // The waiter must be gone: a later push stays queued.
        CHECK( q.try_push( 1 ) );
        CHECK( q.size() == 1 );
        CHECK( log == std::vector< std::string >{ "stopped" } );
}

TEST_CASE( "async_queue - a consumer started under an already-requested stop is stopped" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;
        inplace_stop_source        src;

        src.request_stop();

        auto op = q.pop().connect( pop_recv{ &log, src.get_token() } );
        op.start();

        CHECK( log == std::vector< std::string >{ "stopped" } );
        CHECK( q.try_push( 1 ) );
        CHECK( q.size() == 1 );
}

TEST_CASE( "async_queue - push sender completes immediately when there is room" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;

        auto op = q.push( 4 ).connect( sig_recv{ &log, "p" } );
        op.start();

        CHECK( log == std::vector< std::string >{ "p:value" } );
        CHECK( q.size() == 1 );
}

TEST_CASE( "async_queue - a full memory resource parks the producer until a pop frees room" )
{
        uint8_t                            buffer[128]{};
        circular_buffer_memory< uint16_t > mem{ buffer };
        async_queue< cb_cfg, int >         q{ mem };
        std::vector< std::string >         log;

        std::size_t accepted = 0;
        while ( q.try_push( static_cast< int >( accepted ) ) )
                ++accepted;

        REQUIRE( accepted > 0 );
        CHECK( q.size() == accepted );

        // The resource is exhausted, so the producer parks.
        auto op = q.push( 999 ).connect( sig_recv{ &log, "p" } );
        op.start();
        CHECK( log.empty() );
        CHECK( q.size() == accepted );

        // Freeing one node lets the parked producer through.
        auto h = q.try_pop();
        REQUIRE( h );
        CHECK( h.get< int >() == 0 );
        CHECK( log == std::vector< std::string >{ "p:value" } );
        CHECK( q.size() == accepted );
}

TEST_CASE( "async_queue - cancelling a parked producer completes it eagerly" )
{
        uint8_t                            buffer[128]{};
        circular_buffer_memory< uint16_t > mem{ buffer };
        async_queue< cb_cfg, int >         q{ mem };
        std::vector< std::string >         log;
        inplace_stop_source                src;

        while ( q.try_push( 0 ) )
                ;

        auto op = q.push( 1 ).connect( sig_recv{ &log, "p", src.get_token() } );
        op.start();
        CHECK( log.empty() );

        src.request_stop();
        CHECK( log == std::vector< std::string >{ "p:stopped" } );

        // The producer must be unlinked: freeing room does not complete it a second time.
        auto h = q.try_pop();
        REQUIRE( h );
        CHECK( log == std::vector< std::string >{ "p:stopped" } );
}

TEST_CASE( "async_queue - close stops producers, drains items, then stops consumers" )
{
        uint8_t                            buffer[128]{};
        circular_buffer_memory< uint16_t > mem{ buffer };
        async_queue< cb_cfg, int >         q{ mem };
        std::vector< std::string >         prod_log;
        std::vector< std::string >         cons_log;
        std::vector< std::string >         close_log;

        while ( q.try_push( 7 ) )
                ;
        auto const queued = q.size();
        REQUIRE( queued > 0 );

        auto prod = q.push( 8 ).connect( sig_recv{ &prod_log, "p" } );
        prod.start();
        CHECK( prod_log.empty() );

        auto close_op = q.close().connect( sig_recv{ &close_log, "c" } );

        CHECK( q.closed() );
        CHECK( prod_log == std::vector< std::string >{ "p:stopped" } );

        close_op.start();
        CHECK( close_log.empty() );

        // Items accepted before the close are still delivered.
        for ( std::size_t i = 0; i < queued - 1; ++i )
                CHECK( q.try_pop() );
        CHECK( close_log.empty() );

        CHECK( q.try_pop() );
        CHECK( q.empty() );
        CHECK( close_log == std::vector< std::string >{ "c:value" } );

        // A consumer arriving after the close is stopped at once.
        auto op = q.pop().connect( pop_recv{ &cons_log } );
        op.start();
        CHECK( cons_log == std::vector< std::string >{ "stopped" } );

        CHECK( !q.try_push( 1 ) );
}

TEST_CASE( "async_queue - close stops parked consumers and completes at once when empty" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > cons_log;
        std::vector< std::string > close_log;

        auto op = q.pop().connect( pop_recv{ &cons_log } );
        op.start();

        auto close_op = q.close().connect( sig_recv{ &close_log, "c" } );
        CHECK( cons_log == std::vector< std::string >{ "stopped" } );

        close_op.start();
        CHECK( close_log == std::vector< std::string >{ "c:value" } );
}

TEST_CASE( "async_queue - push sender is stopped when the queue is already closed" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;

        auto close_op = q.close().connect( sig_recv{ &log, "c" } );
        close_op.start();
        CHECK( log == std::vector< std::string >{ "c:value" } );

        auto op = q.push( 1 ).connect( sig_recv{ &log, "p" } );
        op.start();
        CHECK( log == std::vector< std::string >{ "c:value", "p:stopped" } );
}

TEST_CASE( "async_queue - is_consumer_stoppable=false ignores the receiver's stop token" )
{
        nd_mem                              mem;
        async_queue< unstoppable_cfg, int > q{ mem };
        std::vector< std::string >          log;
        inplace_stop_source                 src;

        auto op = q.pop().connect( pop_recv{ &log, src.get_token() } );
        op.start();
        CHECK( log.empty() );

        // No stop callback was registered, so the parked consumer stays parked.
        src.request_stop();
        CHECK( log.empty() );

        // It is still a live waiter and is served by the next push.
        CHECK( q.try_push( 1 ) );
        CHECK( log == std::vector< std::string >{ "int:1" } );

        static_assert(
            !_queue_consumer_stoppable< async_queue< unstoppable_cfg, int >, pop_recv >,
            "cancellation must not be instantiated when the flag is off" );

        // The stop callback lives in a base that is empty for an unstoppable waiter, so the
        // operation state is measurably smaller.
        static_assert(
            sizeof( decltype( q.pop().connect( pop_recv{ &log } ) ) ) <
                sizeof( decltype( std::declval< async_queue< nd_cfg, int >& >().pop().connect(
                    pop_recv{ &log } ) ) ),
            "an unstoppable consumer must not carry the stop callback" );
        static_assert(
            !_queue_consumer_stoppable< async_queue< queue_default_cfg< nd_mem >, int >, pop_recv >,
            "the default configuration must not be cancellable" );
        static_assert(
            !_has_close< async_queue< queue_default_cfg< nd_mem >, int > >,
            "the default configuration must not be closeable" );
}

TEST_CASE( "async_queue - is_producer_stoppable=false ignores the receiver's stop token" )
{
        uint8_t                                     buffer[128]{};
        circular_buffer_memory< uint16_t >          mem{ buffer };
        async_queue< cb_no_producer_stop_cfg, int > q{ mem };
        std::vector< std::string >                  log;
        inplace_stop_source                         src;

        while ( q.try_push( 0 ) )
                ;

        auto op = q.push( 1 ).connect( sig_recv{ &log, "p", src.get_token() } );
        op.start();
        CHECK( log.empty() );

        src.request_stop();
        CHECK( log.empty() );

        // Still parked, so freeing room completes it normally.
        CHECK( q.try_pop() );
        CHECK( log == std::vector< std::string >{ "p:value" } );

        // Consumers keep their cancellation in this configuration.
        static_assert(
            _queue_consumer_stoppable< async_queue< cb_no_producer_stop_cfg, int >, pop_recv >,
            "consumer cancellation must survive when only the producer flag is off" );
        static_assert(
            !_queue_producer_stoppable< async_queue< cb_no_producer_stop_cfg, int >, sig_recv >,
            "producer cancellation must be gone" );
}

TEST_CASE( "async_queue - is_closeable=false removes the shutdown protocol" )
{
        using min_q = async_queue< minimal_cfg, int >;

        static_assert( !_has_close< min_q >, "close() must not exist on a non-closeable queue" );
        static_assert( !_has_closed< min_q >, "closed() must not exist on a non-closeable queue" );
        static_assert(
            _has_close< async_queue< nd_cfg, int > >, "close() must exist on a closeable queue" );

        // With neither cancellation nor close able to stop a waiter, set_stopped_t() is not in
        // the signatures, so a receiver without set_stopped() connects.
        static_assert(
            std::same_as< min_q::_pop_completions_t, completion_signatures< set_value_t( int& ) > >,
            "pop() must not promise set_stopped() it can never deliver" );
        static_assert(
            std::same_as< min_q::_push_completions_t, completion_signatures< set_value_t() > >,
            "push() must not promise set_stopped() it can never deliver" );

        // A closeable queue keeps the signature even with cancellation off.
        static_assert(
            std::same_as<
                async_queue< unstoppable_cfg, int >::_pop_completions_t,
                completion_signatures< set_value_t( int& ), set_stopped_t() > >,
            "close() can still stop a parked consumer, so the signature must remain" );

        nd_mem                     mem;
        min_q                      q{ mem };
        std::vector< std::string > log;

        auto op = q.pop().connect( value_only_recv{ &log } );
        op.start();
        CHECK( log.empty() );

        CHECK( q.try_push( 8 ) );
        CHECK( log == std::vector< std::string >{ "int:8" } );

        CHECK( q.try_push( 9 ) );
        auto h = q.try_pop();
        REQUIRE( h );
        CHECK( h.get< int >() == 9 );
        CHECK( q.empty() );
}

TEST_CASE( "async_queue - layout is pinned, so the README size table cannot drift silently" )
{
        // A tripwire, not a measurement. The table under "Flash cost of each combination" in
        // README.md is .text + .rodata for an ARM target and cannot be checked from a host
        // test. What *can* be checked is the layout underneath it, and in practice the flash
        // figures only move when this layout does.
        //
        // IF ANY ASSERTION BELOW FIRES: the queue's structure changed. Re-measure the README
        // table before editing these numbers — see .claude/skills/code-size for the harness.
        //
        // Sizes are in pointer-sized units so they hold on both the 64-bit host and a 32-bit
        // target.
        constexpr std::size_t ptr = sizeof( void* );

        // Three function pointers, plus size/align narrowed so they share one word.
        static_assert( sizeof( _queue_node_vtable ) == 4 * ptr );

        // One shared link node for the whole queue: just the zll header.
        static_assert( sizeof( _queue_link ) == 2 * ptr );

        // A queued item adds its vtable pointer; a waiter adds exactly one vptr, which is what
        // keeps it to a single virtual and a single polymorphic base.
        static_assert( sizeof( _queue_node ) == 3 * ptr );
        static_assert( sizeof( _queue_waiter ) == 3 * ptr );

        // The core differs between configurations by exactly the `_done` list that
        // `is_closeable` removes, and by nothing else.
        static_assert( sizeof( _queue_core< nd_mem, true > ) == 11 * ptr );
        static_assert( sizeof( _queue_core< nd_mem, false > ) == 9 * ptr );
        static_assert(
            sizeof( _queue_core< nd_mem, true > ) - sizeof( _queue_core< nd_mem, false > ) ==
            2 * ptr );

        // The queue is a thin facade over its core — it adds no state of its own.
        static_assert( sizeof( async_queue< nd_cfg, int, char > ) == 11 * ptr );
        static_assert( sizeof( async_queue< minimal_cfg, int, char > ) == 9 * ptr );

        // The stoppable flags change emitted code, not layout: cancellation state lives in the
        // operation states, not in the queue. Only `is_closeable` moves the object size.
        static_assert(
            sizeof( async_queue< unstoppable_cfg, int, char > ) ==
            sizeof( async_queue< nd_cfg, int, char > ) );
}

TEST_CASE( "async_queue - a doubly-inherited link node is rejected at compile time" )
{
        // All queue nodes share one _queue_link so the zll list machinery is emitted once.
        // A node that reached the link twice would leave ll_header ambiguous and quietly
        // corrupt a list, so _queue_list constrains on std::derived_from, which is defined via
        // the pointer conversion and therefore fails for an ambiguous base.
        struct left : _queue_link
        {
        };
        struct right : _queue_link
        {
        };
        struct twice : left, right
        {
        };

        static_assert( _has_queue_list< left >, "an unambiguous link node must be usable" );
        static_assert( !_has_queue_list< twice >, "an ambiguous link node must be rejected" );

        // The same property is what makes the downcast in _queue_list sound.
        static_assert( std::derived_from< _queue_node, _queue_link > );
        static_assert( std::derived_from< _queue_waiter, _queue_link > );
}

TEST_CASE( "async_queue - mutable internals are not reachable from outside" )
{
        // The operation states are friends and reach the core directly; nothing else should.
        // Type aliases stay public so the senders and these tests can name them.
        static_assert( !_has_core_member< q_t >, "_core must be private" );
        static_assert( !_has_emplace< q_t >, "_emplace must be private" );

        static_assert( requires { typename q_t::_core_t; } );
        static_assert( requires { typename q_t::value_type; } );
}

TEST_CASE( "async_queue - queues over one memory resource share their core instantiation" )
{
        using a_t = async_queue< nd_cfg, int, std::string >;
        using b_t = async_queue< nd_cfg, blob, uint8_t, int >;

        static_assert(
            std::same_as< a_t::_core_t, b_t::_core_t >,
            "queue cores must not be instantiated per element-type pack" );
        static_assert(
            std::same_as< a_t::_core_t, _queue_core< nd_mem, true > >,
            "the core must depend only on the memory resource and is_closeable" );
}

TEST_CASE( "async_queue - a task awaits pop through as_variant" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        q_t                        q{ mem };
        std::vector< std::string > log;

        auto body = [&]( task_ctx& c ) -> task< void > {
                for ( int i = 0; i < 2; ++i ) {
                        auto v = co_await ( q.pop() | as_variant );
                        std::visit(
                            [&]( auto const& x ) {
                                    if constexpr ( std::same_as<
                                                       std::remove_cvref_t< decltype( x ) >,
                                                       int > )
                                            log.push_back( "int:" + std::to_string( x ) );
                                    else
                                            log.push_back( "str:" + x );
                            },
                            v );
                }
                std::ignore = c;
        };

        task_holder h{ ctx, body };
        h.start();
        ctx.core.run_n( 4 );
        CHECK( log.empty() );

        CHECK( q.try_push( 3 ) );
        ctx.core.run_n( 4 );
        CHECK( log == std::vector< std::string >{ "int:3" } );

        CHECK( q.try_push( std::string{ "x" } ) );
        ctx.core.run_n( 4 );
        CHECK( log == std::vector< std::string >{ "int:3", "str:x" } );

        auto stop_op = h.stop().connect( _dummy_receiver{} );
        stop_op.start();
        ctx.core.run_n( 8 );
}

TEST_CASE( "async_queue - a task awaits push" )
{
        nd_mem   mem;
        task_ctx ctx{ mem };
        q_t      q{ mem };
        int      pushed = 0;

        auto body = [&]( task_ctx& c ) -> task< void > {
                co_await q.push( 42 );
                ++pushed;
                std::ignore = c;
        };

        task_holder h{ ctx, body };
        h.start();
        ctx.core.run_n( 4 );

        CHECK( pushed >= 1 );
        CHECK( q.size() >= 1 );

        auto stop_op = h.stop().connect( _dummy_receiver{} );
        stop_op.start();
        ctx.core.run_n( 8 );
}

namespace
{

        /// Handler with one overload per event type, all returning the same sender type as
        /// `pump_config::sender_type` requires.
        ///
        /// The overloads are ADL free functions rather than members: a member `handle` cannot
        /// itself be a coroutine returning `task`, since a task's promise reads its context from
        /// the first argument and for a member that is `this`.
        struct rec_handler
        {
                std::vector< std::string >* log;
                int                         errors  = 0;
                int                         stopped = 0;

                void on_error( task_error )
                {
                        ++errors;
                }

                void on_stopped()
                {
                        ++stopped;
                }
        };

        task< void > handle( task_ctx& ctx, rec_handler& h, int& ev )
        {
                h.log->push_back( "int:" + std::to_string( ev ) );
                co_await ecor::suspend;
                h.log->push_back( "int-done:" + std::to_string( ev ) );
                std::ignore = ctx;
        }

        task< void > handle( task_ctx& ctx, rec_handler& h, std::string& ev )
        {
                h.log->push_back( "str:" + ev );
                std::ignore = ctx;
                co_return;
        }

        struct pump_cfg : pump_default_cfg< task_ctx, task< void > >
        {
                static constexpr bool is_stoppable = true;
        };
        using pump_t = event_pump< pump_cfg, q_t, rec_handler >;

}  // namespace

TEST_CASE( "event_pump - drains events one handler at a time" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        q_t                        q{ mem };
        std::vector< std::string > log;
        rec_handler                h{ &log };

        pump_t p{ ctx, q, h };
        p.start();

        // Idle until something is pushed.
        ctx.core.run_n( 8 );
        CHECK( log.empty() );

        CHECK( q.try_push( 1 ) );
        ctx.core.run_n( 16 );
        CHECK( log == std::vector< std::string >{ "int:1", "int-done:1" } );
        CHECK( q.empty() );

        // A burst is handled in order, and never two at once: the suspending int handler must
        // finish before the string handler starts.
        log.clear();
        CHECK( q.try_push( 2 ) );
        CHECK( q.try_push( std::string{ "a" } ) );
        ctx.core.run_n( 32 );
        CHECK( log == std::vector< std::string >{ "int:2", "int-done:2", "str:a" } );
        CHECK( q.empty() );

        auto stop_op = p.stop().connect( _dummy_receiver{} );
        stop_op.start();
        ctx.core.run_n( 16 );
}

TEST_CASE( "event_pump - stop completes and leaves queued events alone" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        q_t                        q{ mem };
        std::vector< std::string > log;
        rec_handler                h{ &log };

        pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_n( 8 );

        std::vector< std::string > done;
        auto                       stop_op = p.stop().connect( sig_recv{ &done, "stop" } );
        stop_op.start();
        ctx.core.run_n( 16 );
        CHECK( done == std::vector< std::string >{ "stop:value" } );

        // Events pushed after the pump stopped stay in the queue.
        CHECK( q.try_push( 7 ) );
        ctx.core.run_n( 16 );
        CHECK( log.empty() );
        CHECK( q.size() == 1 );
}


TEST_CASE( "async_queue - pop() lends a reference into the node, copying nothing" )
{
        nd_mem                              mem;
        async_queue< minimal_cfg, counted > q{ mem };

        CHECK( q.try_push( counted{ 7 } ) );

        // Delivery itself must not move the payload: the receiver is handed a reference to it
        // where it already sits in the node.
        counted::moves = 0;
        int  seen      = -1;
        auto op        = q.pop().connect( borrow_recv{ &seen } );
        op.start();
        CHECK( seen == 7 );
        CHECK( counted::moves == 0 );
}

TEST_CASE( "async_queue - a receiver may take the event during the call" )
{
        nd_mem                              mem;
        async_queue< minimal_cfg, counted > q{ mem };

        // Moving out of the borrowed reference is the supported way to keep the event; the
        // node is destroyed immediately afterwards, so the moved-from payload is what dies.
        struct taking_recv
        {
                using receiver_concept = receiver_t;

                std::optional< counted >* out;

                void set_value( counted& ev )
                {
                        out->emplace( std::move( ev ) );
                }

                void set_stopped()
                {
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };

        CHECK( q.try_push( counted{ 3 } ) );

        counted::moves = 0;
        std::optional< counted > kept;
        auto                     op = q.pop().connect( taking_recv{ &kept } );
        op.start();

        REQUIRE( kept.has_value() );
        CHECK( kept->id == 3 );
        CHECK( counted::moves == 1 );  // exactly the one the receiver asked for
        CHECK( q.empty() );
}


TEST_CASE( "event_pump - stop() while a handler is mid-flight" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        q_t                        q{ mem };
        std::vector< std::string > log;
        rec_handler                h{ &log };

        pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_n( 8 );

        // Get a handler genuinely in flight: the int handler suspends once.
        CHECK( q.try_push( 5 ) );
        ctx.core.run_n( 2 );
        REQUIRE( log == std::vector< std::string >{ "int:5" } );  // started, not finished

        // Stopping now must not destroy the operation state the suspended handler is still
        // using; the handler has to be allowed to finish.
        std::vector< std::string > done;
        auto                       stop_op = p.stop().connect( sig_recv{ &done, "stop" } );
        stop_op.start();
        ctx.core.run_n( 16 );

        CHECK( log == std::vector< std::string >{ "int:5", "int-done:5" } );
        CHECK( done == std::vector< std::string >{ "stop:value" } );
}


namespace
{
        /// Handler whose work blocks on a source that the test controls, so the handler is
        /// genuinely in flight — not merely queued behind a `suspend`.
        struct blocking_handler
        {
                ll_source< unit, set_value_t(), set_stopped_t() >* gate;
                int*                                               finished;
        };

        task< void > handle( task_ctx& ctx, blocking_handler& h, int& )
        {
                co_await h.gate->schedule();
                ++*h.finished;
                std::ignore = ctx;
        }

        task< void > handle( task_ctx& ctx, blocking_handler& h, std::string& )
        {
                co_await h.gate->schedule();
                ++*h.finished;
                std::ignore = ctx;
        }

        using blocking_pump_t = event_pump< pump_cfg, q_t, blocking_handler >;
}  // namespace

TEST_CASE( "event_pump - stop() must not destroy a handler that is still blocked" )
{
        nd_mem   mem;
        task_ctx ctx{ mem };
        q_t      q{ mem };

        ll_source< unit, set_value_t(), set_stopped_t() > gate;
        int                                               finished = 0;
        blocking_handler                                  h{ &gate, &finished };

        blocking_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_n( 8 );

        CHECK( q.try_push( 1 ) );
        ctx.core.run_n( 8 );
        REQUIRE( !gate.empty() );  // the handler is parked on the gate, mid-flight

        std::vector< std::string > done;
        auto                       stop_op = p.stop().connect( sig_recv{ &done, "stop" } );
        stop_op.start();
        ctx.core.run_n( 8 );

        // The handler is still out there holding the gate. Releasing it must reach live
        // storage, and the pump must not have reported itself idle before that happened.
        REQUIRE( !gate.empty() );
        if ( auto* e = gate.query_next() )
                e->set_value();
        ctx.core.run_n( 8 );
        CHECK( finished == 1 );
        CHECK( done == std::vector< std::string >{ "stop:value" } );
}

}  // namespace ecor
