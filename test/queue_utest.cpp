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

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
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

        /// Neither movable nor copyable: it can only be constructed in place.
        struct pinned
        {
                int id = 0;

                explicit pinned( int i )
                  : id( i )
                {
                }
                pinned( pinned&& ) = delete;
        };

        /// An `inplace_variant` and an `async_queue` over the distinct types
        /// `std::integral_constant< std::size_t, I >...`.
        template < typename Seq >
        struct over_types;

        template < std::size_t... I >
        struct over_types< std::index_sequence< I... > >
        {
                using variant = inplace_variant< std::integral_constant< std::size_t, I >... >;

                template < typename CFG >
                using queue = async_queue< CFG, std::integral_constant< std::size_t, I >... >;
        };
        using over_255_types = over_types< std::make_index_sequence< 255 > >;

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
        /// default configuration deliberately omits — and element types such as `std::string`
        /// admitted.
        struct nd_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr async_queue_api pop_api      = async_queue_api::cancellable;
                static constexpr async_queue_api push_api     = async_queue_api::cancellable;
                static constexpr bool            is_closeable = true;
                static constexpr bool            trivially_copyable_only = false;
        };

        struct cb_cfg : queue_default_cfg< circular_buffer_memory< uint16_t > >
        {
                static constexpr async_queue_api pop_api      = async_queue_api::cancellable;
                static constexpr async_queue_api push_api     = async_queue_api::cancellable;
                static constexpr bool            is_closeable = true;
        };

        /// Closeable, but neither waiter kind reacts to a stop token.
        /// Closeable and fully asynchronous, but neither direction reacts to a stop token.
        struct unstoppable_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr async_queue_api pop_api      = async_queue_api::parking;
                static constexpr async_queue_api push_api     = async_queue_api::parking;
                static constexpr bool            is_closeable = true;
        };

        template < typename T >
        concept _has_queue_list = requires { typename _queue_list< T >; };

        template < typename Q >
        concept _has_core_member = requires( Q& q ) { q._core; };

        template < typename Q >
        concept _has_push_internal =
            requires( Q& q, _queue_node_desc const& d, void* p ) { q._push( d, p ); };

        template < typename Q >
        concept _has_pop = requires( Q& q ) { q.pop(); };

        template < typename Q >
        concept _has_try_pop = requires( Q& q ) { q.try_pop(); };

        template < typename Q >
        concept _has_push = requires( Q& q ) { q.push( 0 ); };

        template < typename Q >
        concept _has_close = requires( Q& q ) { q.close(); };

        template < typename Q >
        concept _has_closed = requires( Q& q ) { q.closed(); };

        /// The default configuration: nothing optional at all.
        using minimal_cfg = queue_default_cfg< nd_mem >;

        /// The default plus `pop()`, for tests that exercise the async dequeue with no
        /// cancellation or shutdown around it, admitting element types that count their moves.
        struct pop_only_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr async_queue_api pop_api                 = async_queue_api::parking;
                static constexpr bool            trivially_copyable_only = false;
        };

        /// The default, admitting element types that are not trivially copyable.
        struct typed_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr bool trivially_copyable_only = false;
        };

        /// Not trivially copyable, for its copy constructor, yet trivially destructible.
        struct copy_noted
        {
                int v = 0;

                copy_noted() = default;
                copy_noted( copy_noted const& o ) noexcept
                  : v( o.v )
                {
                }
                copy_noted( copy_noted&& ) noexcept = default;
        };

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
                static constexpr async_queue_api pop_api      = async_queue_api::cancellable;
                static constexpr async_queue_api push_api     = async_queue_api::parking;
                static constexpr bool            is_closeable = true;
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

TEST_CASE( "inplace_variant - the storage is the first base, the index the second" )
{
        inplace_variant< uint8_t, int, double > v;
        static_assert( decltype( v )::npos == 255 );
        static_assert( sizeof( inplace_variant< int > ) == 2 * sizeof( std::size_t ) );
        CHECK( !v );
        v.emplace< int >( 7 );
        CHECK( v.index() == 1 );
        CHECK( static_cast< void* >( &v ) == v._data() );
        CHECK( static_cast< void* >( &v.get< int >() ) == v._data() );
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

TEST_CASE( "inplace_variant - emplace_empty_from constructs an alternative that cannot move" )
{
        inplace_variant< int, pinned > v;

        auto& p = v.emplace_empty_from< pinned >( [] {
                return pinned{ 3 };
        } );
        CHECK( v.holds< pinned >() );
        CHECK( &p == &v.get< pinned >() );
        CHECK( p.id == 3 );
}

TEST_CASE( "inplace_variant - holds 255 alternatives, every index below npos" )
{
        using v_t  = over_255_types::variant;
        using last = std::integral_constant< std::size_t, 254 >;

        v_t v;
        v.emplace< last >();
        CHECK( v.holds< last >() );
        CHECK( v.index() == 254 );
        CHECK( v.index() < v_t::npos );
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

TEST_CASE( "async_queue - takes 255 element types" )
{
        using q_255 = over_255_types::queue< minimal_cfg >;
        using last  = std::integral_constant< std::size_t, 254 >;

        nd_mem mem;
        q_255  q{ mem };
        CHECK( q.try_push( last{} ) );
        auto v = q.try_pop();
        REQUIRE( v );
        CHECK( v.holds< last >() );
        CHECK( v.index() == 254 );
}

TEST_CASE( "async_queue - only trivially copyable element types are admitted by default" )
{
        static_assert( queue_default_cfg< nd_mem >::trivially_copyable_only );
        static_assert( _queue_admits< minimal_cfg, int, uint64_t, blob > );
        static_assert( !_queue_admits< minimal_cfg, int, std::string > );
        static_assert( !_queue_admits< minimal_cfg, counted > );

        // A configuration that turns the check off admits the rest.
        static_assert( !nd_cfg::trivially_copyable_only );
        static_assert( _queue_admits< nd_cfg, int, std::string > );
}

TEST_CASE( "async_queue - the node descriptor's payload offset is where the payload is" )
{
        struct alignas( 16 ) wide
        {
                char c;
        };
        struct nothing
        {
        };

        auto check = []< typename T >( T val ) {
                auto const&         d = _queue_node_desc_of< T, 0 >;
                _queue_node_of< T > n{ d, val };
                auto const          off = reinterpret_cast< unsigned char const* >( &n._val ) -
                                          reinterpret_cast< unsigned char const* >( &n );
                CHECK( d.payload == ( std::is_empty_v< T > ? 0 : sizeof( T ) ) );
                if ( d.payload )
                        CHECK( std::size_t( off ) == d.offset );
        };
        check( uint8_t{ 1 } );
        check( uint64_t{ 2 } );
        check( wide{ 'x' } );
        check( blob{ 3 } );
        check( nothing{} );
}

TEST_CASE( "async_queue - trivially copyable types need no vtable" )
{
        // Every element type trivially copyable: no vtables at all, and every take byte-wise.
        using plain_q = async_queue< minimal_cfg, uint8_t, uint64_t, blob >;
        static_assert( plain_q::_plain );
        static_assert( plain_q::_desc_of< blob >.vt == nullptr );

        nd_mem  mem;
        plain_q q{ mem };
        CHECK( q.try_push( uint8_t{ 7 } ) );
        CHECK( q.try_push( uint64_t{ 0x0123456789abcdefULL } ) );
        CHECK( q.try_push( blob{ 5 } ) );
        auto a = q.try_pop();
        REQUIRE( a );
        CHECK( a.get< uint8_t >() == 7 );
        auto b = q.try_pop();
        REQUIRE( b );
        CHECK( b.get< uint64_t >() == 0x0123456789abcdefULL );
        auto c = q.try_pop();
        REQUIRE( c );
        CHECK( c.get< blob >().tag == 5 );
        CHECK( !q.try_pop() );

        // The decision is per element type: next to a `std::string`, which needs a vtable, an
        // `int` has none, and taking tells the two apart.
        using mixed_q = async_queue< typed_cfg, int, std::string >;
        static_assert( !mixed_q::_plain && !mixed_q::_all_vtables );
        static_assert( mixed_q::_desc_of< int >.vt == nullptr );

        mixed_q m{ mem };
        CHECK( m.try_push( 1 ) );
        CHECK( m.try_push( std::string( 40, 's' ) ) );
        CHECK( m.try_push( 2 ) );
        auto x = m.try_pop();
        REQUIRE( x );
        CHECK( x.get< int >() == 1 );
        auto y = m.try_pop();
        REQUIRE( y );
        CHECK( y.get< std::string >() == std::string( 40, 's' ) );
        auto z = m.try_pop();
        REQUIRE( z );
        CHECK( z.get< int >() == 2 );
        CHECK( !m.try_pop() );

        // pop() delivers by the descriptor's index, so it needs no vtable either.
        using pop_q = async_queue< pop_only_cfg, int >;
        static_assert( pop_q::_plain );
        static_assert( pop_q::_desc_of< int >.vt == nullptr );

        // Runtime CHECKs: the vtables are weak symbols, and GCC under -fsanitize=undefined
        // declines to fold `&weak != nullptr` into a constant expression. A vtable's destroy is
        // never null: a trivially destructible payload shares one empty function.
        using noted_q = async_queue< typed_cfg, copy_noted >;
        CHECK( mixed_q::_desc_of< std::string >.vt != nullptr );
        CHECK( mixed_q::_desc_of< std::string >.vt->destroy != &_queue_node_no_destroy );
        CHECK( noted_q::_desc_of< copy_noted >.vt->destroy == &_queue_node_no_destroy );
}

namespace
{
        using bundle_q_t = async_queue< minimal_cfg, uint8_t, uint64_t, blob >;

        int wrapped_pushes = 0;

        /// Stands in for a firmware's thread-safe wrapper: it does its own work around every push
        /// and is not a template, yet callers pass elements of any type to it.
        bool wrapped_push( bundle_q_t& q, bundle_q_t::push_bundle const& b )
        {
                ++wrapped_pushes;
                return q.try_push( b );
        }
}  // namespace

TEST_CASE( "async_queue - a non-template wrapper pushes any element through push_bundle" )
{
        nd_mem     mem;
        bundle_q_t q{ mem };

        CHECK( wrapped_push( q, uint8_t{ 7 } ) );
        CHECK( wrapped_push( q, uint64_t{ 0x0123456789abcdefULL } ) );
        CHECK( wrapped_push( q, blob{ 5 } ) );
        CHECK( wrapped_pushes == 3 );

        auto a = q.try_pop();
        REQUIRE( a );
        CHECK( a.get< uint8_t >() == 7 );
        auto b = q.try_pop();
        REQUIRE( b );
        CHECK( b.get< uint64_t >() == 0x0123456789abcdefULL );
        auto c = q.try_pop();
        REQUIRE( c );
        CHECK( c.get< blob >().tag == 5 );
        CHECK( !q.try_pop() );
}

TEST_CASE( "async_queue - push_bundle takes an rvalue of any element type" )
{
        using q_t2 = async_queue< nd_cfg, int, std::string >;
        using b_t  = q_t2::push_bundle;
        static_assert( std::is_constructible_v< b_t, int&& > );
        static_assert( !std::is_constructible_v< b_t, int& > );
        static_assert( !std::is_constructible_v< b_t, int const& > );
        static_assert( std::is_constructible_v< b_t, std::string&& > );
        static_assert( !std::is_constructible_v< b_t, std::string& > );
        static_assert( !std::is_constructible_v< b_t, std::string const& > );
        static_assert( !std::is_constructible_v< b_t, long&& > );

        nd_mem      mem;
        q_t2        q{ mem };
        std::string s( 40, 's' );
        CHECK( q.try_push( b_t{ 1 } ) );
        CHECK( q.try_push( b_t{ std::move( s ) } ) );

        // A closed queue refuses the push and leaves the element alone.
        std::ignore = q.close();
        std::string t( 40, 't' );
        CHECK( !q.try_push( b_t{ std::move( t ) } ) );
        CHECK( t == std::string( 40, 't' ) );

        auto h = q.try_pop();
        REQUIRE( h );
        CHECK( h.get< int >() == 1 );
        auto g = q.try_pop();
        REQUIRE( g );
        CHECK( g.get< std::string >() == std::string( 40, 's' ) );
        CHECK( !q.try_pop() );
}

namespace
{
        using typed_bundle_q_t = async_queue< typed_cfg, int, std::string >;

        /// As `wrapped_push`, over element types that are not all trivially copyable.
        bool wrapped_typed_push( typed_bundle_q_t& q, typed_bundle_q_t::push_bundle const& b )
        {
                return q.try_push( b );
        }
}  // namespace

TEST_CASE( "async_queue - a non-template wrapper moves elements that are not trivially copyable" )
{
        nd_mem           mem;
        typed_bundle_q_t q{ mem };
        std::string      s( 40, 'w' );
        CHECK( wrapped_typed_push( q, 7 ) );
        CHECK( wrapped_typed_push( q, std::move( s ) ) );
        CHECK( wrapped_typed_push( q, std::string( "temp" ) ) );

        auto a = q.try_pop();
        REQUIRE( a );
        CHECK( a.get< int >() == 7 );
        auto b = q.try_pop();
        REQUIRE( b );
        CHECK( b.get< std::string >() == std::string( 40, 'w' ) );
        auto c = q.try_pop();
        REQUIRE( c );
        CHECK( c.get< std::string >() == "temp" );
        CHECK( !q.try_pop() );
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

TEST_CASE( "async_queue - a pop() receiver stopped in start() may destroy the operation state" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;
        inplace_stop_source        src;
        op_slot                    slot;
        src.request_stop();

        auto& op = slot.emplace_from( [&] {
                return q.pop().connect( self_destroying_recv{ &log, &slot, src.get_token() } );
        } );
        op.start();

        CHECK( log == std::vector< std::string >{ "stopped" } );
        CHECK( slot.untouched() );
}

TEST_CASE( "async_queue - a pop() receiver may destroy the operation state when given an item" )
{
        nd_mem                     mem;
        q_t                        q{ mem };
        std::vector< std::string > log;
        op_slot                    slot;

        auto& op = slot.emplace_from( [&] {
                return q.pop().connect( self_destroying_recv{ &log, &slot } );
        } );
        op.start();
        CHECK( q.try_push( 5 ) );

        CHECK( log == std::vector< std::string >{ "value" } );
        CHECK( slot.untouched() );
        CHECK( q.empty() );
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

namespace
{
        /// A bounded resource, parking, and element types that are not trivially copyable.
        struct cb_typed_cfg : queue_default_cfg< circular_buffer_memory< uint16_t > >
        {
                static constexpr async_queue_api push_api                = async_queue_api::parking;
                static constexpr bool            trivially_copyable_only = false;
        };
}  // namespace

TEST_CASE( "async_queue - a parked producer pushes the value it was given" )
{
        uint8_t                                  buffer[256]{};
        circular_buffer_memory< uint16_t >       mem{ buffer };
        async_queue< cb_typed_cfg, std::string > q{ mem };
        std::vector< std::string >               log;

        std::size_t accepted = 0;
        while ( q.try_push( std::string( "x" ) ) )
                ++accepted;
        REQUIRE( accepted > 0 );

        // Long enough to live on the heap, so a moved-from copy would be visibly empty.
        std::string const payload( 40, 'p' );
        auto              op = q.push( payload ).connect( sig_recv{ &log, "p" } );
        op.start();
        CHECK( log.empty() );

        for ( std::size_t i = 0; i < accepted; ++i ) {
                auto h = q.try_pop();
                REQUIRE( h );
                CHECK( h.get< std::string >() == "x" );
        }
        CHECK( log == std::vector< std::string >{ "p:value" } );
        auto last = q.try_pop();
        REQUIRE( last );
        CHECK( last.get< std::string >() == payload );
}

TEST_CASE( "async_queue - a push that finds no room leaves its element untouched" )
{
        using q_t2 = async_queue< cb_typed_cfg, std::string >;

        uint8_t                            buffer[256]{};
        circular_buffer_memory< uint16_t > mem{ buffer };
        q_t2                               q{ mem };
        while ( q.try_push( std::string( "x" ) ) )
                ;

        std::string s( 40, 'k' );
        CHECK( !q.try_push( q_t2::push_bundle( std::move( s ) ) ) );
        CHECK( s == std::string( 40, 'k' ) );
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

TEST_CASE( "async_queue - a push() receiver stopped in start() may destroy the operation state" )
{
        uint8_t                            buffer[128]{};
        circular_buffer_memory< uint16_t > mem{ buffer };
        async_queue< cb_cfg, int >         q{ mem };
        std::vector< std::string >         log;
        inplace_stop_source                src;
        op_slot                            slot;
        src.request_stop();

        while ( q.try_push( 0 ) )
                ;

        auto& op = slot.emplace_from( [&] {
                return q.push( 1 ).connect( self_destroying_recv{ &log, &slot, src.get_token() } );
        } );
        op.start();

        CHECK( log == std::vector< std::string >{ "stopped" } );
        CHECK( slot.untouched() );
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

TEST_CASE( "async_queue - pop_api=parking ignores the receiver's stop token" )
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
            "cancellation must not be instantiated below queue_api::cancellable" );

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

TEST_CASE( "async_queue - push_api=parking ignores the receiver's stop token" )
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
            "consumer cancellation must survive when only the producer side is lower" );
        static_assert(
            !_queue_producer_stoppable< async_queue< cb_no_producer_stop_cfg, int >, sig_recv >,
            "producer cancellation must be gone" );
}

TEST_CASE( "async_queue - is_closeable=false removes the shutdown protocol" )
{
        using min_q = async_queue< pop_only_cfg, int >;

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

        // The descriptor head is four bytes of narrowed scalars, padded to the pointer, and the
        // pointer to the vtable; an allocated node's descriptor adds the node's size, alignment
        // and payload offset, padded again. The vtable is three function pointers.
        static_assert( sizeof( _queue_desc_head ) == 2 * ptr );
        static_assert( sizeof( _queue_node_desc ) == 3 * ptr );
        static_assert( sizeof( _queue_node_vtable ) == 3 * ptr );

        // One shared link node for the whole queue: just the zll header.
        static_assert( sizeof( _queue_link ) == 2 * ptr );

        // A queued item adds its descriptor pointer; a waiter adds exactly one vptr, which is what
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
        static_assert( !_has_push_internal< q_t >, "_push must be private" );

        static_assert( requires { typename q_t::_core_t; } );
        static_assert( requires { typename q_t::value_type; } );
}

TEST_CASE( "async_queue - async_queue_api::try_only removes the sender and its per-type dispatch" )
{
        static_assert(
            !_has_pop< async_queue< minimal_cfg, int > > &&
                !_has_push< async_queue< minimal_cfg, int > >,
            "neither sender exists at async_queue_api::try_only" );
        static_assert(
            _has_pop< async_queue< pop_only_cfg, int > >,
            "pop() must exist above async_queue_api::try_only" );
        static_assert(
            !_has_push< async_queue< pop_only_cfg, int > >,
            "the two directions are configured independently" );

        // Cancellation is a state of the sender, not a separate axis: it cannot be asked for
        // without the sender, because `plain` and `stoppable` are values of one enum.
        static_assert( async_queue_api::try_only < async_queue_api::parking );
        static_assert( async_queue_api::parking < async_queue_api::cancellable );

        // Delivery reads the descriptor's index, so a node needs nothing per element type for
        // pop(): a trivially copyable element type has no vtable with pop() or without it.
        static_assert( async_queue< pop_only_cfg, int >::_desc_of< int >.vt == nullptr );

        // try_push/try_pop keep working without the senders.
        nd_mem                          mem;
        async_queue< minimal_cfg, int > q{ mem };
        CHECK( q.try_push( 4 ) );
        auto v = q.try_pop();
        REQUIRE( v );
        CHECK( v.get< int >() == 4 );
}

TEST_CASE( "async_queue - push() shares one operation base across element types and queues" )
{
        using a_t    = async_queue< nd_cfg, int, std::string >;
        using b_t    = async_queue< nd_cfg, blob, std::string >;
        using base_t = _queue_push_op_base_of< a_t, sig_recv >;

        static_assert( std::same_as< base_t, _queue_push_op_base_of< b_t, sig_recv > > );
        static_assert( std::derived_from< _queue_push_op< a_t, int, sig_recv >, base_t > );
        static_assert( std::derived_from< _queue_push_op< a_t, std::string, sig_recv >, base_t > );
        static_assert( std::derived_from< _queue_push_op< b_t, blob, sig_recv >, base_t > );
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

/// Coroutine bodies for the task tests below.
///
/// These are free functions rather than coroutine lambdas: the promise allocates the frame
/// through `operator new( size, ctx, args... )`, and for a lambda GCC passes the closure as
/// that first argument while the closure type is still incomplete, which is a hard error
/// rather than a substitution failure. A free function taking `task_ctx&` first avoids it.
static task< void > qu_pop_variant_task( task_ctx&, q_t& q, std::vector< std::string >& log )
{
        for ( int i = 0; i < 2; ++i ) {
                auto v = co_await ( q.pop() | as_variant );
                std::visit(
                    [&]( auto const& x ) {
                            if constexpr (
                                std::same_as< std::remove_cvref_t< decltype( x ) >, int > )
                                    log.push_back( "int:" + std::to_string( x ) );
                            else
                                    log.push_back( "str:" + x );
                    },
                    v );
        }
}

static task< void > qu_push_task( task_ctx&, q_t& q, int& pushed )
{
        co_await q.push( 42 );
        ++pushed;
}

TEST_CASE( "async_queue - a task awaits pop through as_variant" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        q_t                        q{ mem };
        std::vector< std::string > log;

        task_holder h{ ctx, [&]( task_ctx& c ) {
                              return qu_pop_variant_task( c, q, log );
                      } };
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

        task_holder h{ ctx, [&]( task_ctx& c ) {
                              return qu_push_task( c, q, pushed );
                      } };
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

        /// A plain queue — trivially copyable elements and no `pop()` — from which the pump takes
        /// its events byte-wise.
        using plain_pump_q = async_queue< minimal_cfg, int, blob >;

        struct plain_handler
        {
                std::vector< std::string >* log;
        };

        task< void > handle( task_ctx& ctx, plain_handler& h, int& ev )
        {
                h.log->push_back( "int:" + std::to_string( ev ) );
                std::ignore = ctx;
                co_return;
        }

        task< void > handle( task_ctx& ctx, plain_handler& h, blob& ev )
        {
                h.log->push_back( "blob:" + std::to_string( ev.tag ) );
                std::ignore = ctx;
                co_return;
        }

        using plain_pump_t = event_pump< pump_cfg, plain_pump_q, plain_handler >;

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

TEST_CASE( "event_pump - drains a plain queue, taking its events byte-wise" )
{
        static_assert( plain_pump_q::_plain );

        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        plain_pump_q               q{ mem };
        std::vector< std::string > log;
        plain_handler              h{ &log };

        plain_pump_t p{ ctx, q, h };
        p.start();
        CHECK( q.try_push( 3 ) );
        CHECK( q.try_push( blob{ 4 } ) );
        ctx.core.run_n( 32 );
        CHECK( log == std::vector< std::string >{ "int:3", "blob:4" } );
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
        nd_mem                               mem;
        async_queue< pop_only_cfg, counted > q{ mem };

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
        nd_mem                               mem;
        async_queue< pop_only_cfg, counted > q{ mem };

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

TEST_CASE( "event_pump - an item taken before stop() is handled under the stop request" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        q_t                        q{ mem };
        std::vector< std::string > log, slog;
        rec_handler                h{ &log };

        pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        CHECK( q.try_push( 7 ) );  // taken into the pump's slot; the handler has not started
        CHECK( q.empty() );

        auto st = p.stop().connect( sig_recv{ &slog, "stop" } );
        st.start();
        ctx.core.run_n( 8 );

        // A taken event is never dropped: its handler runs and sees the stop at its first
        // suspension.
        CHECK( log == std::vector< std::string >{ "int:7" } );
        CHECK( h.stopped == 1 );
        CHECK( slog == std::vector< std::string >{ "stop:value" } );
}

// ---------------------------------------------------------------------------------------------
// push_and_wait()

namespace
{
        /// A call's element. A handler fills `reply` in place.
        struct req
        {
                int id    = 0;
                int reply = 0;
        };

        /// Calls complete with "done", can be cancelled, and the queue can be closed; `pop()`
        /// exists so that release-based consumers can be exercised.
        struct call_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr async_queue_api pop_api      = async_queue_api::parking;
                static constexpr bool            is_closeable = true;
                using push_and_wait_signatures                = completion_signatures<
                    set_value_t(),
                    set_error_t( task_error ),
                    set_stopped_t() >;
        };
        using call_q = async_queue< call_cfg, int, req >;

        /// Calls carry an `int` reply.
        struct reply_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr async_queue_api pop_api = async_queue_api::parking;
                using push_and_wait_signatures           = completion_signatures<
                    set_value_t( int ),
                    set_error_t( task_error ),
                    set_stopped_t() >;
        };
        using reply_q = async_queue< reply_cfg, req >;

        /// The smallest set: calls can only complete done.
        struct done_cfg : queue_default_cfg< nd_mem >
        {
                using push_and_wait_signatures = completion_signatures< set_value_t() >;
        };
        using done_q = async_queue< done_cfg, int >;

        /// Calls on an element type that is not trivially copyable.
        struct typed_call_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr bool trivially_copyable_only = false;
                using push_and_wait_signatures                = completion_signatures<
                    set_value_t(),
                    set_error_t( task_error ),
                    set_stopped_t() >;
        };
        using typed_call_q = async_queue< typed_call_cfg, counted >;

        /// Receiver for a call on `call_q` or `typed_call_q`.
        struct call_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;
                std::string                 name;
                inplace_stop_token          token{};

                void set_value()
                {
                        log->push_back( name + ":value" );
                }

                void set_error( task_error )
                {
                        log->push_back( name + ":error" );
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

        /// Receiver for a call on `reply_q`.
        struct reply_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;
                inplace_stop_token          token{};

                void set_value( int v )
                {
                        log->push_back( "value:" + std::to_string( v ) );
                }

                void set_error( task_error )
                {
                        log->push_back( "error" );
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

        /// Receiver for a call on `done_q`: the set has no other row.
        struct done_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;

                void set_value()
                {
                        log->push_back( "done" );
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };

        /// `pop()` receiver for `call_q`, recording the delivered value and where it lives.
        struct call_pop_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;
                void const**                seen_at = nullptr;

                void set_value( int& v )
                {
                        log->push_back( "int:" + std::to_string( v ) );
                        if ( seen_at )
                                *seen_at = &v;
                }

                void set_value( req& r )
                {
                        log->push_back( "req:" + std::to_string( r.id ) );
                        if ( seen_at )
                                *seen_at = &r;
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

        /// Pump handler over `call_q`: each handler suspends once, so a call is genuinely in
        /// flight across a tick, and the `req` handler answers in place.
        struct call_handler
        {
                std::vector< std::string >* log;
        };

        task< void > handle( task_ctx& ctx, call_handler& h, int& ev )
        {
                h.log->push_back( "int:" + std::to_string( ev ) + " start" );
                co_await ecor::suspend;
                h.log->push_back( "int:" + std::to_string( ev ) + " done" );
                std::ignore = ctx;
        }

        task< void > handle( task_ctx& ctx, call_handler& h, req& ev )
        {
                h.log->push_back( "req:" + std::to_string( ev.id ) + " start" );
                co_await ecor::suspend;
                ev.reply = ev.id * 2;
                h.log->push_back( "req:" + std::to_string( ev.id ) + " done" );
                std::ignore = ctx;
        }

        using call_pump_t =
            event_pump< pump_default_cfg< task_ctx, task< void > >, call_q, call_handler >;
        using stoppable_call_pump_t = event_pump< pump_cfg, call_q, call_handler >;

        /// Pump handler over `typed_call_q`.
        struct typed_call_handler
        {
                int seen = 0;
        };

        task< void > handle( task_ctx& ctx, typed_call_handler& h, counted& ev )
        {
                h.seen = ev.id;
                co_await ecor::suspend;
                std::ignore = ctx;
        }

        using typed_call_pump_t = event_pump<
            pump_default_cfg< task_ctx, task< void > >,
            typed_call_q,
            typed_call_handler >;

        /// Pump handler over `reply_q` returning the reply through its task.
        struct reply_handler
        {
        };

        task< int > handle( task_ctx& ctx, reply_handler&, req& ev )
        {
                ev.reply    = ev.id + 1;
                std::ignore = ctx;
                co_return ev.id + 1;
        }

        using reply_pump_t =
            event_pump< pump_default_cfg< task_ctx, task< int > >, reply_q, reply_handler >;

        /// Handler sender completing at once with the outcome its handler chose.
        struct outcome_sender
        {
                using sender_concept        = sender_t;
                using completion_signatures = ecor::completion_signatures<
                    set_value_t( int ),
                    set_error_t( task_error ),
                    set_stopped_t() >;

                enum class kind : uint8_t
                {
                        value,
                        error,
                        stopped
                };

                kind k;
                int  v;

                template < receiver R >
                struct op
                {
                        using operation_state_concept = operation_state_t;

                        R    r;
                        kind k;
                        int  v;

                        void start()
                        {
                                if ( k == kind::value )
                                        r.set_value( v );
                                else if ( k == kind::error )
                                        r.set_error( task_error::task_unfinished );
                                else
                                        r.set_stopped();
                        }
                };

                template < receiver R >
                op< R > connect( R r )
                {
                        return { std::move( r ), k, v };
                }
        };

        struct outcome_handler
        {
                outcome_sender::kind k;

                outcome_sender handle( task_ctx&, req& ev )
                {
                        return { k, ev.id + 1 };
                }
        };

        using outcome_pump_t =
            event_pump< pump_default_cfg< task_ctx, outcome_sender >, reply_q, outcome_handler >;

        template < typename Q >
        concept _has_push_and_wait = requires( Q& q, int& v ) { q.push_and_wait( v ); };

        template < typename Q >
        concept _accepts_const_call = requires( Q& q, int const& v ) { q.push_and_wait( v ); };

        template < typename Q >
        concept _accepts_rvalue_call = requires( Q& q ) { q.push_and_wait( int{ 1 } ); };

}  // namespace

static task< void > pw_reply_task( task_ctx&, reply_q& q, req& r, int& out )
{
        out = co_await q.push_and_wait( r );
}

static task< void > pw_done_task( task_ctx&, call_q& q, req& r, int& done )
{
        co_await q.push_and_wait( r );
        ++done;
}

TEST_CASE( "push_and_wait - exists exactly when the signature set is not empty" )
{
        static_assert( !_has_push_and_wait< async_queue< minimal_cfg, int > > );
        static_assert( !_has_push_and_wait< q_t > );
        static_assert( _has_push_and_wait< done_q > );
        static_assert( _has_push_and_wait< call_q > );

        // The element is an lvalue of an element type: neither a temporary nor an `int const`,
        // which is not one.
        static_assert( !_accepts_rvalue_call< done_q > );
        static_assert( !_accepts_const_call< done_q > );

        // The sender completes with exactly the configured set.
        using call_sender_t =
            decltype( std::declval< reply_q& >().push_and_wait( std::declval< req& >() ) );
        static_assert( std::same_as<
                       call_sender_t::completion_signatures,
                       reply_cfg::push_and_wait_signatures > );

        // What the queue checks the set for.
        static_assert( _queue_call_sigs< done_cfg::push_and_wait_signatures >::values == 1 );
        static_assert( _queue_call_sigs< done_cfg::push_and_wait_signatures >::nullary );
        static_assert( !_queue_call_sigs< reply_cfg::push_and_wait_signatures >::nullary );
        static_assert(
            _queue_call_sigs<
                completion_signatures< set_value_t(), set_value_t( int ) > >::values == 2 );
        static_assert( !_queue_call_sigs<
                       completion_signatures< set_value_t( int, int ) > >::at_most_one_argument );
        static_assert(
            _queue_call_sigs<
                completion_signatures< set_stopped_t(), set_stopped_t() > >::stopped == 2 );
        static_assert( !_queue_call_sigs< completion_signatures< get_stopped_t() > >::basic );
}

TEST_CASE( "push_and_wait - layout is pinned alongside the rest of the queue" )
{
        constexpr std::size_t ptr = sizeof( void* );

        // The call node is the item node plus the element's address, the receiver's address and
        // the vtable pointer; its descriptor is the head alone.
        static_assert( sizeof( _queue_call_node ) == 6 * ptr );
        static_assert( sizeof( _queue_call_desc_of< int, 0 > ) == sizeof( _queue_desc_head ) );

        // The operation state adds the core reference and the receiver, nothing else, when the
        // set has no stopped row.
        static_assert( sizeof( _queue_call_op_of< done_q, done_recv > ) == 8 * ptr );

        // A call descriptor shares index and vtable with the item descriptor of the same type.
        // The vtables are checked at run time: they are weak symbols, and GCC under
        // -fsanitize=undefined declines to fold `&weak != nullptr` into a constant expression.
        static_assert( call_q::_call_desc_of< req >.kind == _queue_node_kind::call );
        static_assert( call_q::_desc_of< req >.kind == _queue_node_kind::item );
        static_assert( call_q::_call_desc_of< req >.index == call_q::_desc_of< req >.index );
        CHECK( typed_call_q::_call_desc_of< counted >.vt != nullptr );
        CHECK( typed_call_q::_call_desc_of< counted >.vt == typed_call_q::_desc_of< counted >.vt );

        // Queues with and without calls share one core: the code that tells a call from an
        // allocated node lives in the queue, which knows the call's signatures.
        static_assert( std::same_as< call_q::_core_t, q_t::_core_t > );
}

TEST_CASE( "push_and_wait - a typed reply leaves the queue without pop() and try_pop()" )
{
        static_assert( _has_try_pop< call_q > );
        static_assert( !_has_try_pop< reply_q > );
        static_assert( !_has_pop< reply_q > );
}

TEST_CASE( "push_and_wait - one operation state per core, receiver and signature set" )
{
        using a_t = async_queue< call_cfg, int, req >;
        using b_t = async_queue< call_cfg, req >;
        static_assert( std::same_as<
                       _queue_call_op_of< a_t, call_recv >,
                       _queue_call_op_of< b_t, call_recv > > );
}

TEST_CASE( "push_and_wait - a parked pop() consumer completes the call within start()" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > plog, clog;

        auto pop = q.pop().connect( call_pop_recv{ &plog } );
        pop.start();

        int  seven = 7;
        auto op    = q.push_and_wait( seven ).connect( call_recv{ &clog, "c" } );
        op.start();

        CHECK( plog == std::vector< std::string >{ "int:7" } );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
        CHECK( q.empty() );
        CHECK( q.size() == 0 );
}

TEST_CASE( "push_and_wait - pop() borrows the caller's object" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > plog, clog;

        void const* seen = nullptr;
        auto        pop  = q.pop().connect( call_pop_recv{ &plog, &seen } );
        pop.start();

        req  r{ .id = 4 };
        auto op = q.push_and_wait( r ).connect( call_recv{ &clog, "c" } );
        op.start();

        CHECK( plog == std::vector< std::string >{ "req:4" } );
        CHECK( seen == &r );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
}

TEST_CASE( "push_and_wait - try_pop() completes a queued call" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > clog;

        int  seven = 7;
        auto op    = q.push_and_wait( seven ).connect( call_recv{ &clog, "c" } );
        op.start();
        CHECK( q.size() == 1 );
        CHECK( clog.empty() );

        auto v = q.try_pop();
        REQUIRE( v );
        CHECK( v.get< int >() == 7 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
        CHECK( q.size() == 0 );
}

TEST_CASE( "push_and_wait - a call keeps its place in the FIFO among pushed items" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > clog;

        CHECK( q.try_push( 1 ) );
        int  two = 2;
        auto op  = q.push_and_wait( two ).connect( call_recv{ &clog, "c" } );
        op.start();
        CHECK( q.try_push( 3 ) );
        CHECK( q.size() == 3 );

        auto a = q.try_pop();
        REQUIRE( a );
        CHECK( a.get< int >() == 1 );
        CHECK( clog.empty() );

        auto b = q.try_pop();
        REQUIRE( b );
        CHECK( b.get< int >() == 2 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );

        auto c = q.try_pop();
        REQUIRE( c );
        CHECK( c.get< int >() == 3 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
        CHECK( q.empty() );
}

TEST_CASE( "push_and_wait - try_pop() moves a typed element out of the caller's object" )
{
        nd_mem                     mem;
        typed_call_q               q{ mem };
        std::vector< std::string > clog;

        counted c{ 9 };
        counted::moves = 0;
        auto op        = q.push_and_wait( c ).connect( call_recv{ &clog, "c" } );
        op.start();
        CHECK( counted::moves == 0 );

        auto v = q.try_pop();
        REQUIRE( v );
        CHECK( v.get< counted >().id == 9 );
        CHECK( counted::moves == 1 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
}

TEST_CASE( "event_pump - completes a call when its handler finishes, in place" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > hlog, clog;
        call_handler               h{ &hlog };

        call_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();  // the pump parks on the queue

        req  r{ .id = 5 };
        auto op = q.push_and_wait( r ).connect( call_recv{ &clog, "c" } );
        op.start();  // delivered to the parked pump, which holds it and schedules itself
        CHECK( q.empty() );
        CHECK( clog.empty() );

        ctx.core.run_once();  // the pump starts the handler
        ctx.core.run_once();  // the handler runs to its suspension
        CHECK( hlog == std::vector< std::string >{ "req:5 start" } );
        CHECK( clog.empty() );
        CHECK( r.reply == 0 );

        ctx.core.run_once();  // the handler finishes: the reply is in the caller's object
        CHECK( r.reply == 10 );
        CHECK( hlog == std::vector< std::string >{ "req:5 start", "req:5 done" } );
        CHECK( clog == std::vector< std::string >{ "c:value" } );

        ctx.core.run_once();  // the pump parks again
        CHECK( !ctx.core.run_once() );
}

TEST_CASE( "event_pump - handles a call's typed element without moving it" )
{
        nd_mem             mem;
        task_ctx           ctx{ mem };
        typed_call_q       q{ mem };
        typed_call_handler h;

        typed_call_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        std::vector< std::string > clog;
        counted                    c{ 3 };
        counted::moves = 0;
        auto op        = q.push_and_wait( c ).connect( call_recv{ &clog, "c" } );
        op.start();
        ctx.core.run_n( 8 );

        CHECK( h.seen == 3 );
        CHECK( counted::moves == 0 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
}

TEST_CASE( "push_and_wait - a task receives the typed reply" )
{
        nd_mem        mem;
        task_ctx      ctx{ mem };
        reply_q       q{ mem };
        reply_handler h;

        reply_pump_t p{ ctx, q, h };
        p.start();

        req  r{ .id = 41 };
        int  out = -1;
        auto t   = pw_reply_task( ctx, q, r, out ).connect( _dummy_receiver{} );
        t.start();
        ctx.core.run_n( 16 );

        CHECK( out == 42 );
        CHECK( r.reply == 42 );  // the handler wrote into the caller's object as well
        CHECK( q.empty() );
}

TEST_CASE( "push_and_wait - a task awaits a call that completes done" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > hlog;
        call_handler               h{ &hlog };

        call_pump_t p{ ctx, q, h };
        p.start();

        req  r{ .id = 6 };
        int  done = 0;
        auto t    = pw_done_task( ctx, q, r, done ).connect( _dummy_receiver{} );
        t.start();
        ctx.core.run_n( 16 );

        CHECK( done == 1 );
        CHECK( r.reply == 12 );
        CHECK( hlog == std::vector< std::string >{ "req:6 start", "req:6 done" } );
}

TEST_CASE( "event_pump - forwards the handler's completion to the call row by row" )
{
        nd_mem   mem;
        task_ctx ctx{ mem };

        using kind = outcome_sender::kind;
        for ( auto k : { kind::value, kind::error, kind::stopped } ) {
                reply_q         q{ mem };
                outcome_handler h{ k };
                outcome_pump_t  p{ ctx, q, h };
                p.start();
                ctx.core.run_once();

                std::vector< std::string > rlog;
                req                        r{ .id = 10 };
                auto                       op = q.push_and_wait( r ).connect( reply_recv{ &rlog } );
                op.start();
                CHECK( rlog.empty() );

                ctx.core.run_once();  // the pump starts the handler, which completes at once
                std::string const expected = k == kind::value ? "value:11" :
                                             k == kind::error ? "error" :
                                                                "stopped";
                CHECK( rlog == std::vector< std::string >{ expected } );
                ctx.core.run_n( 4 );
        }
}

TEST_CASE(
    "event_pump - a handler whose completion has no call signature is rejected at compile time" )
{
        using vt_t = _apply_to_sigs_t< _queue_call_vtable, reply_cfg::push_and_wait_signatures >;
        using nullary_vt_t =
            _apply_to_sigs_t< _queue_call_vtable, done_cfg::push_and_wait_signatures >;

        static_assert(
            _queue_call_accepts< vt_t, outcome_sender::completion_signatures, false >::value );
        // A value converts into the row.
        static_assert(
            _queue_call_accepts< vt_t, completion_signatures< set_value_t( long ) >, false >::
                value );
        // An error type without a row, a stop without the stopped row, and no value for a row
        // that carries one are all rejected.
        static_assert(
            !_queue_call_accepts< vt_t, completion_signatures< set_error_t( int ) >, false >::
                value );
        static_assert(
            !_queue_call_accepts< nullary_vt_t, completion_signatures< set_stopped_t() >, true >::
                value );
        static_assert(
            !_queue_call_accepts< vt_t, completion_signatures< set_value_t() >, false >::value );
        // A nullary row drops whatever value the handler produced.
        static_assert(
            _queue_call_accepts< nullary_vt_t, completion_signatures< set_value_t( int ) >, true >::
                value );

        // What the pump asserts for its queue, which also admits any handler on a queue without
        // calls.
        static_assert( !_pump_answers_calls< reply_q, completion_signatures< set_value_t() > > );
        static_assert( _pump_answers_calls< call_q, completion_signatures< set_value_t( int ) > > );
        static_assert( !_pump_answers_calls<
                       done_q,
                       completion_signatures< set_value_t(), set_stopped_t() > > );
        static_assert( _pump_answers_calls< q_t, completion_signatures< set_error_t( int ) > > );
}

TEST_CASE( "push_and_wait - cancelling a queued call removes it and completes it stopped" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > clog;
        inplace_stop_source        src;

        int  one = 1;
        auto op  = q.push_and_wait( one ).connect( call_recv{ &clog, "c", src.get_token() } );
        op.start();
        CHECK( q.size() == 1 );

        src.request_stop();
        CHECK( clog == std::vector< std::string >{ "c:stopped" } );
        CHECK( q.size() == 0 );
        CHECK( q.empty() );
        CHECK( !q.try_pop() );
}

TEST_CASE( "push_and_wait - a stop requested while in flight waits for the handler" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > hlog, clog;
        call_handler               h{ &hlog };
        inplace_stop_source        src;

        call_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        int  two = 2;
        auto op  = q.push_and_wait( two ).connect( call_recv{ &clog, "c", src.get_token() } );
        op.start();
        ctx.core.run_once();  // handler started
        ctx.core.run_once();  // handler suspended mid-flight
        REQUIRE( hlog == std::vector< std::string >{ "int:2 start" } );

        src.request_stop();
        CHECK( clog.empty() );

        ctx.core.run_once();  // the handler finishes
        CHECK( clog == std::vector< std::string >{ "c:value" } );
        ctx.core.run_n( 4 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
}

TEST_CASE( "push_and_wait - a call started under a requested stop completes stopped at once" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > clog;
        inplace_stop_source        src;
        src.request_stop();

        int  one = 1;
        auto op  = q.push_and_wait( one ).connect( call_recv{ &clog, "c", src.get_token() } );
        op.start();
        CHECK( clog == std::vector< std::string >{ "c:stopped" } );
        CHECK( q.size() == 0 );
}

TEST_CASE( "push_and_wait - a receiver stopped in start() may destroy the operation state" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > log;
        inplace_stop_source        src;
        op_slot                    slot;
        src.request_stop();

        int   one = 1;
        auto& op  = slot.emplace_from( [&] {
                return q.push_and_wait( one ).connect(
                    self_destroying_recv{ &log, &slot, src.get_token() } );
        } );
        op.start();

        CHECK( log == std::vector< std::string >{ "stopped" } );
        CHECK( slot.untouched() );
        CHECK( q.empty() );
}

TEST_CASE( "push_and_wait - close() delivers queued calls and rejects later ones" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > log;

        int  one = 1;
        auto op  = q.push_and_wait( one ).connect( call_recv{ &log, "c" } );
        op.start();

        auto cl = q.close().connect( sig_recv{ &log, "close" } );
        cl.start();
        CHECK( log.empty() );

        auto v = q.try_pop();
        REQUIRE( v );
        CHECK( log == std::vector< std::string >{ "c:value", "close:value" } );

        int  two = 2;
        auto op2 = q.push_and_wait( two ).connect( call_recv{ &log, "c2" } );
        op2.start();
        CHECK( log == std::vector< std::string >{ "c:value", "close:value", "c2:stopped" } );
        CHECK( q.size() == 0 );
}

TEST_CASE( "push_and_wait - cancelling the last queued call lets a pending close() complete" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > log;
        inplace_stop_source        src;

        int  one = 1;
        auto op  = q.push_and_wait( one ).connect( call_recv{ &log, "c", src.get_token() } );
        op.start();
        auto cl = q.close().connect( sig_recv{ &log, "close" } );
        cl.start();
        CHECK( log.empty() );

        src.request_stop();
        CHECK( log == std::vector< std::string >{ "c:stopped", "close:value" } );
}

TEST_CASE( "push_and_wait - destroying a queued call takes it out of the queue" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > clog;

        {
                int  one = 1;
                auto op  = q.push_and_wait( one ).connect( call_recv{ &clog, "c" } );
                op.start();
                CHECK( q.size() == 1 );
        }
        CHECK( q.size() == 0 );
        CHECK( q.empty() );
        CHECK( !q.try_pop() );
        CHECK( clog.empty() );
}

TEST_CASE( "push_and_wait - destroying the last queued call lets a pending close() complete" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > log;
        op_slot                    slot;

        int   one = 1;
        auto& op  = slot.emplace_from( [&] {
                return q.push_and_wait( one ).connect( call_recv{ &log, "c" } );
        } );
        op.start();
        auto cl = q.close().connect( sig_recv{ &log, "close" } );
        cl.start();
        CHECK( log.empty() );

        slot.destroy();
        CHECK( log == std::vector< std::string >{ "close:value" } );
}

TEST_CASE( "event_pump - interleaves calls and items in queue order" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > log;
        call_handler               h{ &log };

        call_pump_t p{ ctx, q, h };
        p.start();

        CHECK( q.try_push( 1 ) );
        int  two = 2;
        auto op  = q.push_and_wait( two ).connect( call_recv{ &log, "c" } );
        op.start();
        CHECK( q.try_push( 3 ) );
        CHECK( q.size() == 3 );

        ctx.core.run_n( 64 );
        CHECK(
            log == std::vector< std::string >{
                       "int:1 start",
                       "int:1 done",
                       "int:2 start",
                       "int:2 done",
                       "c:value",
                       "int:3 start",
                       "int:3 done" } );
        CHECK( q.empty() );
}

TEST_CASE( "event_pump - a call taken before stop() is handled under the stop request" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > hlog, clog, slog;
        call_handler               h{ &hlog };

        stoppable_call_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        int  two = 2;
        auto op  = q.push_and_wait( two ).connect( call_recv{ &clog, "c" } );
        op.start();  // held by the pump, whose handler has not started yet

        auto st = p.stop().connect( sig_recv{ &slog, "stop" } );
        st.start();
        ctx.core.run_once();  // the pump starts the handler regardless
        ctx.core.run_once();  // the handler sees the stop at its first suspension
        CHECK( hlog == std::vector< std::string >{ "int:2 start" } );
        CHECK( clog == std::vector< std::string >{ "c:stopped" } );

        ctx.core.run_n( 4 );
        CHECK( slog == std::vector< std::string >{ "stop:value" } );
        CHECK( q.empty() );
}

// ---------------------------------------------------------------------------------------------
// push_and_wait(): paths the cases above leave out

namespace
{
        /// Receiver for `call_q` with no stop token: the set has a stopped row, the receiver
        /// cannot be cancelled, so the operation state carries no stop callback.
        struct nostop_call_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;

                void set_value()
                {
                        log->push_back( "n:value" );
                }

                void set_error( task_error )
                {
                        log->push_back( "n:error" );
                }

                void set_stopped()
                {
                        log->push_back( "n:stopped" );
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };

        /// A queue mixing a trivially copyable and a move-only element type, so the payload is
        /// found at run time.
        using mixed_call_q = async_queue< typed_call_cfg, int, counted >;

        /// An empty element type: a payload of zero bytes.
        struct ping
        {
        };
        using ping_call_q = async_queue< call_cfg, ping, int >;

        struct ping_pop_recv
        {
                using receiver_concept = receiver_t;

                void const** seen_at;

                void set_value( ping& p )
                {
                        *seen_at = &p;
                }

                void set_value( int& v )
                {
                        *seen_at = &v;
                }

                void set_stopped()
                {
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };

        /// `pop()` receiver for `reply_q`: it cannot answer a call.
        struct req_pop_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;

                void set_value( req& r )
                {
                        log->push_back( "req:" + std::to_string( r.id ) );
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };

        /// Calls carry a `uint16_t` reply; the handler's `int` converts into it.
        struct reply16_cfg : queue_default_cfg< nd_mem >
        {
                using push_and_wait_signatures = completion_signatures<
                    set_value_t( uint16_t ),
                    set_error_t( task_error ),
                    set_stopped_t() >;
        };
        using reply16_q = async_queue< reply16_cfg, req >;
        using reply16_pump_t =
            event_pump< pump_default_cfg< task_ctx, task< int > >, reply16_q, reply_handler >;

        struct reply16_recv
        {
                using receiver_concept = receiver_t;

                std::vector< std::string >* log;

                void set_value( uint16_t v )
                {
                        log->push_back( "value:" + std::to_string( v ) );
                }

                void set_error( task_error )
                {
                        log->push_back( "error" );
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

        /// Handler over `call_q` whose tasks return a value the nullary row drops.
        struct drop_handler
        {
        };

        task< int > handle( task_ctx& ctx, drop_handler&, int& ev )
        {
                std::ignore = ctx;
                co_return ev + 1;
        }

        task< int > handle( task_ctx& ctx, drop_handler&, req& ev )
        {
                std::ignore = ctx;
                co_return ev.id + 1;
        }

        using drop_pump_t =
            event_pump< pump_default_cfg< task_ctx, task< int > >, call_q, drop_handler >;

        /// Handler over `reply_q` whose coroutine fails.
        struct failing_handler
        {
        };

        task< int > handle( task_ctx& ctx, failing_handler&, req& )
        {
                std::ignore = ctx;
                co_yield with_error( task_error::task_unfinished );
                co_return 0;
        }

        using failing_pump_t =
            event_pump< pump_default_cfg< task_ctx, task< int > >, reply_q, failing_handler >;

        /// `pop()` over a move-only element type, for the borrowed delivery of a call.
        struct typed_pop_call_cfg : queue_default_cfg< nd_mem >
        {
                static constexpr async_queue_api pop_api                 = async_queue_api::parking;
                static constexpr bool            trivially_copyable_only = false;
                using push_and_wait_signatures                           = completion_signatures<
                    set_value_t(),
                    set_error_t( task_error ),
                    set_stopped_t() >;
        };
        using typed_pop_call_q = async_queue< typed_pop_call_cfg, counted >;

        struct counted_pop_recv
        {
                using receiver_concept = receiver_t;

                void const** seen_at;

                void set_value( counted& c )
                {
                        *seen_at = &c;
                }

                [[nodiscard]] empty_env get_env() const noexcept
                {
                        return {};
                }
        };
}  // namespace

TEST_CASE( "push_and_wait - a receiver without a stop token carries no cancellation" )
{
        constexpr std::size_t ptr = sizeof( void* );
        static_assert( !_queue_call_stoppable< call_q, nostop_call_recv > );
        static_assert( _queue_call_stoppable< call_q, call_recv > );
        static_assert( sizeof( _queue_call_op_of< call_q, nostop_call_recv > ) == 8 * ptr );
        static_assert( sizeof( _queue_call_op_of< call_q, call_recv > ) > 8 * ptr );

        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > log;

        int  one = 1;
        auto op  = q.push_and_wait( one ).connect( nostop_call_recv{ &log } );
        op.start();
        CHECK( q.size() == 1 );

        auto v = q.try_pop();
        REQUIRE( v );
        CHECK( v.get< int >() == 1 );
        CHECK( log == std::vector< std::string >{ "n:value" } );
}

TEST_CASE(
    "push_and_wait - a stop after a pump took the call, before its handler starts, is ignored" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > hlog, clog;
        call_handler               h{ &hlog };
        inplace_stop_source        src;

        call_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        int  two = 2;
        auto op  = q.push_and_wait( two ).connect( call_recv{ &clog, "c", src.get_token() } );
        op.start();  // held by the pump; the handler has not started
        CHECK( q.empty() );

        src.request_stop();
        CHECK( clog.empty() );

        ctx.core.run_n( 4 );
        CHECK( hlog == std::vector< std::string >{ "int:2 start", "int:2 done" } );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
}

TEST_CASE( "push_and_wait - a stop requested after completion does nothing" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > clog;
        inplace_stop_source        src;

        {
                int  one = 1;
                auto op =
                    q.push_and_wait( one ).connect( call_recv{ &clog, "c", src.get_token() } );
                op.start();

                auto v = q.try_pop();
                REQUIRE( v );
                CHECK( clog == std::vector< std::string >{ "c:value" } );

                // The stop callback is still registered: the request must find a completed call
                // and leave it alone.
                src.request_stop();
                CHECK( clog == std::vector< std::string >{ "c:value" } );
        }
        CHECK( clog == std::vector< std::string >{ "c:value" } );
}

TEST_CASE( "push_and_wait - cancelling a queued call awaited by a task stops the task" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > tlog;
        inplace_stop_source        src;

        req  r{ .id = 1 };
        int  done = 0;
        auto t =
            pw_done_task( ctx, q, r, done ).connect( call_recv{ &tlog, "t", src.get_token() } );
        t.start();
        ctx.core.run_once();  // the task runs to its call, which queues
        CHECK( q.size() == 1 );

        src.request_stop();
        CHECK( q.size() == 0 );
        CHECK( done == 0 );
        CHECK( tlog == std::vector< std::string >{ "t:stopped" } );
        CHECK( !ctx.core.run_once() );
}

TEST_CASE( "push_and_wait - a connected call that never starts leaves no trace" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > clog;
        inplace_stop_source        src;

        {
                int  one = 1;
                auto op =
                    q.push_and_wait( one ).connect( call_recv{ &clog, "c", src.get_token() } );
                std::ignore = op;
        }
        CHECK( q.size() == 0 );
        CHECK( clog.empty() );
        src.request_stop();
        CHECK( clog.empty() );
}

TEST_CASE( "push_and_wait - destroying a queued call unregisters its stop callback" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > clog;
        inplace_stop_source        src;

        {
                int  one = 1;
                auto op =
                    q.push_and_wait( one ).connect( call_recv{ &clog, "c", src.get_token() } );
                op.start();
                CHECK( q.size() == 1 );
        }
        CHECK( q.size() == 0 );
        src.request_stop();
        CHECK( clog.empty() );
}

TEST_CASE( "push_and_wait - try_pop() completes calls of both element kinds on a mixed queue" )
{
        static_assert( !mixed_call_q::_plain && !mixed_call_q::_all_vtables );

        nd_mem                     mem;
        mixed_call_q               q{ mem };
        std::vector< std::string > log;

        int     four = 4;
        counted five{ 5 };
        counted::moves = 0;
        auto a         = q.push_and_wait( four ).connect( call_recv{ &log, "a" } );
        a.start();
        auto b = q.push_and_wait( five ).connect( call_recv{ &log, "b" } );
        b.start();
        CHECK( q.size() == 2 );

        auto v1 = q.try_pop();
        REQUIRE( v1 );
        CHECK( v1.get< int >() == 4 );
        CHECK( log == std::vector< std::string >{ "a:value" } );

        auto v2 = q.try_pop();
        REQUIRE( v2 );
        CHECK( v2.get< counted >().id == 5 );
        CHECK( counted::moves == 1 );
        CHECK( log == std::vector< std::string >{ "a:value", "b:value" } );
}

TEST_CASE( "push_and_wait - an empty element type is called like any other" )
{
        static_assert( ping_call_q::_call_desc_of< ping >.payload == 0 );

        nd_mem                     mem;
        ping_call_q                q{ mem };
        std::vector< std::string > log;

        ping pg{};
        auto op = q.push_and_wait( pg ).connect( call_recv{ &log, "p" } );
        op.start();
        auto v = q.try_pop();
        REQUIRE( v );
        CHECK( v.holds< ping >() );
        CHECK( log == std::vector< std::string >{ "p:value" } );

        void const* seen = nullptr;
        auto        pop  = q.pop().connect( ping_pop_recv{ &seen } );
        pop.start();
        auto op2 = q.push_and_wait( pg ).connect( call_recv{ &log, "p2" } );
        op2.start();
        CHECK( seen == &pg );
        CHECK( log == std::vector< std::string >{ "p:value", "p2:value" } );
}

TEST_CASE( "event_pump - the handler's value converts into the call's reply" )
{
        nd_mem        mem;
        task_ctx      ctx{ mem };
        reply16_q     q{ mem };
        reply_handler h;

        reply16_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        std::vector< std::string > rlog;
        req                        r{ .id = 300 };
        auto                       op = q.push_and_wait( r ).connect( reply16_recv{ &rlog } );
        op.start();
        ctx.core.run_n( 8 );
        CHECK( rlog == std::vector< std::string >{ "value:301" } );
        CHECK( r.reply == 301 );
}

TEST_CASE( "event_pump - a call without a reply drops the handler's value" )
{
        nd_mem       mem;
        task_ctx     ctx{ mem };
        call_q       q{ mem };
        drop_handler h;

        drop_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        std::vector< std::string > clog;
        int                        one = 1;
        auto                       op  = q.push_and_wait( one ).connect( call_recv{ &clog, "c" } );
        op.start();
        ctx.core.run_n( 8 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
}

TEST_CASE( "event_pump - a failing handler coroutine completes the call with its error" )
{
        nd_mem          mem;
        task_ctx        ctx{ mem };
        reply_q         q{ mem };
        failing_handler h;

        failing_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        std::vector< std::string > rlog;
        req                        r{ .id = 1 };
        auto                       op = q.push_and_wait( r ).connect( reply_recv{ &rlog } );
        op.start();
        ctx.core.run_n( 8 );
        CHECK( rlog == std::vector< std::string >{ "error" } );
        CHECK( !ctx.core.run_once() );
}

TEST_CASE( "event_pump - stopping while a call's handler runs lets it finish" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > hlog, clog, slog;
        call_handler               h{ &hlog };

        stoppable_call_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        int  two = 2;
        auto op  = q.push_and_wait( two ).connect( call_recv{ &clog, "c" } );
        op.start();
        ctx.core.run_once();  // handler started
        ctx.core.run_once();  // handler suspended mid-flight
        REQUIRE( hlog == std::vector< std::string >{ "int:2 start" } );

        auto st = p.stop().connect( sig_recv{ &slog, "stop" } );
        st.start();
        CHECK( clog.empty() );

        ctx.core.run_once();  // the handler resumes and finishes
        CHECK( hlog == std::vector< std::string >{ "int:2 start", "int:2 done" } );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
        ctx.core.run_n( 4 );
        CHECK( slog == std::vector< std::string >{ "stop:value" } );
}

TEST_CASE(
    "event_pump - close() while the pump holds a call completes at once; the pump then ends" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > hlog, log;
        call_handler               h{ &hlog };

        call_pump_t p{ ctx, q, h };
        p.start();
        ctx.core.run_once();

        int  three = 3;
        auto op    = q.push_and_wait( three ).connect( call_recv{ &log, "c" } );
        op.start();  // held by the pump: nothing is queued any more

        auto cl = q.close().connect( sig_recv{ &log, "close" } );
        cl.start();
        CHECK( log == std::vector< std::string >{ "close:value" } );

        ctx.core.run_n( 8 );
        CHECK( hlog == std::vector< std::string >{ "int:3 start", "int:3 done" } );
        CHECK( log == std::vector< std::string >{ "close:value", "c:value" } );
        CHECK( !ctx.core.run_once() );  // the pump found the queue closed and stays idle
}

TEST_CASE( "event_pump - two pumps share a queue with calls" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > hlog1, hlog2, clog;
        call_handler               h1{ &hlog1 }, h2{ &hlog2 };

        call_pump_t p1{ ctx, q, h1 };
        call_pump_t p2{ ctx, q, h2 };
        p1.start();
        p2.start();
        ctx.core.run_n( 2 );  // both parked, p1 first

        int  one = 1, two = 2;
        auto a = q.push_and_wait( one ).connect( call_recv{ &clog, "a" } );
        a.start();
        auto b = q.push_and_wait( two ).connect( call_recv{ &clog, "b" } );
        b.start();
        CHECK( q.empty() );

        ctx.core.run_n( 16 );
        CHECK( hlog1 == std::vector< std::string >{ "int:1 start", "int:1 done" } );
        CHECK( hlog2 == std::vector< std::string >{ "int:2 start", "int:2 done" } );
        CHECK( clog == std::vector< std::string >{ "a:value", "b:value" } );
}

TEST_CASE( "push_and_wait - several queued calls complete in order" )
{
        nd_mem                     mem;
        call_q                     q{ mem };
        std::vector< std::string > log;

        int  a = 1, b = 2, c = 3;
        auto oa = q.push_and_wait( a ).connect( call_recv{ &log, "c1" } );
        oa.start();
        auto ob = q.push_and_wait( b ).connect( call_recv{ &log, "c2" } );
        ob.start();
        auto oc = q.push_and_wait( c ).connect( call_recv{ &log, "c3" } );
        oc.start();
        CHECK( q.size() == 3 );

        for ( int expected = 1; expected <= 3; ++expected ) {
                auto v = q.try_pop();
                REQUIRE( v );
                CHECK( v.get< int >() == expected );
        }
        CHECK( log == std::vector< std::string >{ "c1:value", "c2:value", "c3:value" } );
        CHECK( q.empty() );
}

TEST_CASE( "push_and_wait - a task and a plain receiver await calls on one queue" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        call_q                     q{ mem };
        std::vector< std::string > clog;

        req  r{ .id = 6 };
        int  done = 0;
        auto t    = pw_done_task( ctx, q, r, done ).connect( _dummy_receiver{} );
        t.start();
        ctx.core.run_once();  // the task's call is queued first

        int  one = 1;
        auto op  = q.push_and_wait( one ).connect( call_recv{ &clog, "c" } );
        op.start();
        CHECK( q.size() == 2 );

        auto v1 = q.try_pop();
        REQUIRE( v1 );
        CHECK( v1.get< req >().id == 6 );
        CHECK( clog.empty() );
        CHECK( done == 0 );

        auto v2 = q.try_pop();
        REQUIRE( v2 );
        CHECK( v2.get< int >() == 1 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );

        ctx.core.run_n( 4 );
        CHECK( done == 1 );
}

TEST_CASE( "push_and_wait - pop() borrows a move-only element in place" )
{
        nd_mem                     mem;
        typed_pop_call_q           q{ mem };
        std::vector< std::string > clog;

        void const* seen = nullptr;
        auto        pop  = q.pop().connect( counted_pop_recv{ &seen } );
        pop.start();

        counted c{ 9 };
        counted::moves = 0;
        auto op        = q.push_and_wait( c ).connect( call_recv{ &clog, "c" } );
        op.start();
        CHECK( seen == &c );
        CHECK( counted::moves == 0 );
        CHECK( clog == std::vector< std::string >{ "c:value" } );
}

}  // namespace ecor
