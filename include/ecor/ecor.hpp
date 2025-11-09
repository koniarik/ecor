#pragma once

#include <concepts>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <limits>
#include <new>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <zll.hpp>

#ifdef ECOR_DEFAULT_ASSERT
#include <cassert>
#define ECOR_ASSERT( expr ) assert( expr )

#else

#ifndef ECOR_ASSERT
#define ECOR_ASSERT( expr ) ( (void) ( ( expr ) ) )
#endif

#endif

namespace ecor
{

struct sender_t
{
};

struct receiver_t
{
};

struct operation_state_t
{
};

template < typename T >
concept memory_resource = requires( T a, std::size_t bytes, std::size_t align, void* p ) {
        { allocate( a, bytes, align ) } -> std::same_as< void* >;
        { deallocate( a, p, bytes, align ) } -> std::same_as< void >;
};

struct _dummy_receiver
{
        using receiver_concept = receiver_t;

        template < typename... Args >
        void set_value( Args&&... ) noexcept
        {
        }
        template < typename Err >
        void set_error( Err&& ) noexcept
        {
        }

        void set_stopped() noexcept
        {
        }

        struct _env
        {
        };

        [[nodiscard]] _env get_env() const noexcept
        {
                return _env{};
        }
};

template < typename T >
void* allocate( T& mem, std::size_t bytes, std::size_t align ) noexcept(
    noexcept( mem.allocate( bytes, align ) ) )
{
        return mem.allocate( bytes, align );
}

template < typename T >
void deallocate( T& mem, void* p, std::size_t bytes, std::size_t align ) noexcept(
    noexcept( mem.deallocate( p, bytes, align ) ) )
{
        return mem.deallocate( p, bytes, align );
}

template < typename T >
T _align_idx( uint8_t const* buff, T idx, std::size_t align ) noexcept
{
        auto* p = buff + idx;
        auto  a = ( (uintptr_t) p ) % align;
        if ( a )
                p += align - a;
        return (T) ( p - buff );
}

template < std::size_t N >
constexpr auto _pick_index_type() noexcept
{
        if constexpr ( N <= 0xff )
                return uint8_t{};
        else if constexpr ( N <= 0xffff )
                return uint16_t{};
        else if constexpr ( N <= 0xffffffff )
                return uint32_t{};
        else
                return;
}

template < std::size_t N >
using _index_type = decltype( _pick_index_type< N >() );

struct noop_base
{
};

template < typename IndexType, typename Base = noop_base >
struct circular_buffer_memory : Base
{
        std::span< uint8_t > _buff;

        template < std::size_t N >
                requires( std::numeric_limits< IndexType >::max() >= N )
        circular_buffer_memory( std::span< uint8_t, N > b ) noexcept
          : _buff( std::move( b ) )
        {
        }

        // Allocate `bytes` with `align`, returns nullptr if no space is available
        void* allocate( std::size_t bytes, std::size_t align ) noexcept
        {
                auto* p = _allocate( _buff, bytes, align );
                return p;
        }

        template < typename T, typename... Args >
        T* construct( Args&&... args )
        {
                void* p = allocate( sizeof( T ), alignof( T ) );
                if ( !p )
                        return nullptr;
                return new ( p ) T( (Args&&) ( args )... );
        }

        // Deallocate pointer previously allocated by allocate()
        void deallocate( void* p, std::size_t bytes, std::size_t align ) noexcept
        {
                std::ignore = bytes;
                std::ignore = align;
                _deallocate( _buff.data(), (uint8_t*) p );
        }

        template < typename T >
        void destroy( T& p )
        {
                p.~T();
                deallocate( &p, sizeof( T ), alignof( T ) );
        }

        std::size_t capacity() const noexcept
        {
                return _buff.size();
        }

        std::size_t used_bytes() const noexcept
        {
                if ( _first == npos )
                        return 0;
                if ( _first <= _last )
                        return ( _next - _first );
                return ( _buff.size() - _first ) + _next;
        }

        using index_type = IndexType;

        static constexpr index_type npos = std::numeric_limits< index_type >::max();

        struct node
        {
                index_type next_idx = npos;
                index_type prev_idx = npos;
        };
        // Index of first node in the list, npos if empty
        index_type _first = npos;
        // Index of last node in the list, npos if empty
        index_type _last = npos;
        // Index of first free byte after _last, npos if empty
        index_type _next = npos;

        // Pick index where to allocate `bytes` with `align`, returns npos if no space
        // is available. The returned index points to the node header, the actual data
        // starts after sizeof(node).
        [[nodiscard]] index_type _pick_index(
            std::span< uint8_t > buff,
            std::size_t          bytes,
            std::size_t          align ) const noexcept
        {
                index_type idx      = npos;
                int        capacity = 0;
                if ( _last == npos ) {
                        // Empty buffer
                        ECOR_ASSERT( _first == npos && _last == npos );
                        idx      = _align_idx( buff.data(), sizeof( node ), align );
                        capacity = buff.size() - idx;
                } else if ( _first <= _next ) {
                        // Non-overflow state: [ ][x][x][x][ ][ ]
                        {
                                idx      = _align_idx( buff.data(), _next + sizeof( node ), align );
                                capacity = buff.size() - idx;
                                if ( idx < buff.size() && capacity >= (int) bytes )
                                        return idx - sizeof( node );
                        }
                        // Non-overflow state, but overflow triggered: [ ][ ][ ][x][x][x]
                        {
                                idx      = _align_idx( buff.data(), sizeof( node ), align );
                                capacity = idx - _first;
                        }

                } else {  // _first > _next
                          // Overflow state: [x][x][ ][ ][x][x]
                        idx      = _align_idx( buff.data(), _next + sizeof( node ), align );
                        capacity = _first - idx;
                }
                if ( idx < buff.size() && capacity >= (int) bytes )
                        return idx - sizeof( node );
                return npos;
        }

        node _get_node( uint8_t* buff, index_type idx ) const noexcept
        {
                node n;
                std::memcpy( &n, buff + idx, sizeof( node ) );
                return n;
        }

        void _set_node( uint8_t* buff, index_type idx, node const& n ) noexcept
        {
                std::memcpy( buff + idx, &n, sizeof( node ) );
        }

        void* _allocate( std::span< uint8_t > buff, std::size_t bytes, std::size_t align ) noexcept
        {
                auto idx = _pick_index( buff, bytes, align );
                if ( idx == npos )
                        return nullptr;
                auto* p = buff.data() + idx;
                if ( _first == npos ) {
                        _first = idx;
                } else {
                        auto nn     = _get_node( buff.data(), _last );
                        nn.next_idx = idx;
                        _set_node( buff.data(), _last, nn );
                }
                node n{
                    .prev_idx = _last,
                    .next_idx = npos,
                };
                _last = idx;
                _next = idx + sizeof( node ) + bytes;
                _set_node( buff.data(), idx, n );
                return p + sizeof( node );
        }

        void _deallocate( uint8_t* buff, void* p ) noexcept
        {
                auto*      pp  = (uint8_t*) p - sizeof( node );
                index_type idx = pp - buff;
                node       n   = _get_node( buff, idx );

                if ( _first == idx ) {
                        _first = n.next_idx;
                } else {
                        auto pn     = _get_node( buff, n.prev_idx );
                        pn.next_idx = n.next_idx;
                        _set_node( buff, n.prev_idx, pn );
                }

                if ( _last == idx ) {
                        _last = n.prev_idx;
                        _next = idx;
                } else {
                        auto nn     = _get_node( buff, n.next_idx );
                        nn.prev_idx = n.prev_idx;
                        _set_node( buff, n.next_idx, nn );
                }

                // If buffer is now empty, reset _next to npos as well
                if ( _first == npos && _last == npos )
                        _next = npos;

                _set_node( buff, idx, node{ .next_idx = 0, .prev_idx = 0 } );
        }
};

template < typename T, typename IndexType, typename Base >
struct circular_buffer_allocator
{
        using value_type      = T;
        using size_type       = std::size_t;
        using difference_type = std::ptrdiff_t;

        // Required for C++17 allocator requirements
        template < typename U >
        struct rebind
        {
                using other = circular_buffer_allocator< U, IndexType, Base >;
        };

private:
        circular_buffer_memory< IndexType, Base >* _buffer;

public:
        explicit circular_buffer_allocator(
            circular_buffer_memory< IndexType, Base >& buffer ) noexcept
          : _buffer( &buffer )
        {
        }

        circular_buffer_allocator( circular_buffer_allocator const& ) noexcept = default;

        template < typename U >
        circular_buffer_allocator(
            circular_buffer_allocator< U, IndexType, Base > const& other ) noexcept
          : _buffer( other._buffer )
        {
        }

        T* allocate( std::size_t n )
        {
                if ( n == 0 )
                        return nullptr;

                std::size_t bytes = n * sizeof( T );
                void*       ptr   = _buffer->allocate( bytes, alignof( T ) );

                if ( !ptr )
                        throw std::bad_alloc();

                return static_cast< T* >( ptr );
        }

        void deallocate( T* ptr, std::size_t n )
        {
                if ( ptr )
                        _buffer->deallocate( ptr, n * sizeof( T ), alignof( T ) );
        }

        bool operator==( circular_buffer_allocator const& other ) const noexcept
        {
                return _buffer == other._buffer;
        }

        bool operator!=( circular_buffer_allocator const& other ) const noexcept
        {
                return !( *this == other );
        }

        template < typename U, typename IdxType, typename Base2 >
        friend struct circular_buffer_allocator;
};

template < typename T >
concept queryable = std::is_object_v< T >;

template < typename T >
concept sender = std::derived_from< typename std::remove_cvref_t< T >::sender_concept, sender_t > &&
                 requires( T const& s ) {
                         { get_env( s ) } -> queryable;
                 } && std::move_constructible< std::remove_cvref_t< T > > &&
                 std::constructible_from< std::remove_cvref_t< T >, T >;

template < typename T >
concept receiver =
    std::derived_from< typename std::remove_cvref_t< T >::receiver_concept, receiver_t > &&
    requires( T const& r ) {
            { get_env( r ) } -> queryable;
    } && std::move_constructible< std::remove_cvref_t< T > > &&
    std::constructible_from< std::remove_cvref_t< T >, T >;

template < typename T >
auto get_env( T const& ) noexcept
{
        struct empty_env
        {
        };
        return empty_env{};
}


struct set_value_t
{
};

template < typename T >
struct _value_setter
{
        using type = set_value_t( T );
};

template <>
struct _value_setter< void >
{
        using type = set_value_t();
};

template < typename T >
using _value_setter_t = typename _value_setter< T >::type;

struct set_error_t
{
};

struct set_stopped_t
{
};

template < typename T >
struct _is_signature : std::false_type
{
};

template < typename... Args >
struct _is_signature< set_value_t( Args... ) > : std::true_type
{
};
template < typename Err >
struct _is_signature< set_error_t( Err ) > : std::true_type
{
};
template <>
struct _is_signature< set_stopped_t() > : std::true_type
{
};

template < typename T >
concept signature = _is_signature< T >::value;

template < signature S >
struct _vtable_row;

template < typename T >
struct _tag
{
};

template < typename... Args >
struct _vtable_row< set_value_t( Args... ) >
{

        using f_t = void ( * )( void*, Args... );
        f_t _set_value;

        template < typename T >
        constexpr _vtable_row( _tag< T > ) noexcept
          : _set_value{ +[]( void* self, Args... args ) {
                  static_cast< T* >( self )->set_value( (Args&&) args... );
          } }
        {
        }

        void set( set_value_t, void* self, Args... args ) const
        {
                _set_value( self, (Args&&) args... );
        }
};

template < typename... Args >
struct _vtable_row< set_error_t( Args... ) >
{
        using f_t = void ( * )( void*, Args... );
        f_t _set_error;

        template < typename T >
        constexpr _vtable_row( _tag< T > ) noexcept
          : _set_error{ +[]( void* self, Args... args ) {
                  static_cast< T* >( self )->set_error( (Args&&) args... );
          } }
        {
        }

        void set( set_error_t, void* self, Args... args ) const
        {
                _set_error( self, (Args&&) args... );
        }
};

template <>
struct _vtable_row< set_stopped_t() >
{
        using f_t = void ( * )( void* );
        f_t _set_stopped;

        template < typename T >
        constexpr _vtable_row( _tag< T > ) noexcept
          : _set_stopped{ +[]( void* self ) {
                  static_cast< T* >( self )->set_stopped();
          } }
        {
        }

        void set( set_stopped_t, void* self ) const
        {
                _set_stopped( self );
        }
};

template < signature... S >
struct _vtable : _vtable_row< S >...
{
        template < typename T >
        constexpr _vtable( _tag< T > t ) noexcept
          : _vtable_row< S >( t )...
        {
        }

        using _vtable_row< S >::set...;
};

template < typename R, typename VTable >
static constexpr VTable _vtable_of = { _tag< R >{} };

template < typename... S >
struct completion_signatures
{
};

template < typename Sigs >
struct _vtable_of_sigs;

template < signature... S >
struct _vtable_of_sigs< completion_signatures< S... > >
{
        using type = _vtable< S... >;
};

template < typename TL, typename Tag, typename Sigs, template < class... > class Tuple >
struct _filter_map_tag;

template <
    template < class... >
    class Variant,
    typename... S,
    typename Tag,
    template < class... >
    class Tuple >
struct _filter_map_tag< Variant< S... >, Tag, completion_signatures<>, Tuple >
{
        using type = Variant< S... >;
};

template <
    template < class... >
    class Variant,
    typename... S,
    typename Tag,
    typename... Us,
    typename... Ts,
    template < class... >
    class Tuple >
struct _filter_map_tag< Variant< S... >, Tag, completion_signatures< Tag( Us... ), Ts... >, Tuple >
  : _filter_map_tag< Variant< S..., Tuple< Us... > >, Tag, completion_signatures< Ts... >, Tuple >
{
};

template <
    template < class... >
    class Variant,
    typename... S,
    typename Tag,
    typename T,
    typename... Ts,
    template < class... >
    class Tuple >
struct _filter_map_tag< Variant< S... >, Tag, completion_signatures< T, Ts... >, Tuple >
  : _filter_map_tag< Variant< S... >, Tag, completion_signatures< Ts... >, Tuple >
{
};

template < typename T >
using _type_identity_t = T;

template < typename Tag >
struct _tag_identity_t
{
        template < typename... Args >
        using type = Tag( Args... );
};

template < typename Sigs >
using _map_values_of_t =
    typename _filter_map_tag< std::variant<>, set_value_t, Sigs, std::tuple >::type;

template < typename Sigs >
using _map_errors_of_t =
    typename _filter_map_tag< std::variant<>, set_error_t, Sigs, _type_identity_t >::type;

template < typename Sigs >
using _filter_values_of_t = typename _filter_map_tag<
    completion_signatures<>,
    set_value_t,
    Sigs,
    _tag_identity_t< set_value_t >::type >::type;

template < typename Sigs >
using _filter_errors_of_t = typename _filter_map_tag<
    completion_signatures<>,
    set_error_t,
    Sigs,
    _tag_identity_t< set_error_t >::type >::type;

template < typename Sigs >
using _filter_stopped_of_t = typename _filter_map_tag<
    completion_signatures<>,
    set_stopped_t,
    Sigs,
    _tag_identity_t< set_stopped_t >::type >::type;

template < sender S, typename Env >
using _sender_completions_t =
    decltype( std::declval< S >().get_completion_signatures( std::declval< Env >() ) );

template < typename T, typename... Ts >
struct _contains_type;

template < typename T, typename U, typename... Ts >
        requires( std::same_as< T, U > )
struct _contains_type< T, U, Ts... >
{
        static constexpr bool value = true;
};

template < typename T, typename U, typename... Ts >
        requires( !std::same_as< T, U > )
struct _contains_type< T, U, Ts... > : _contains_type< T, Ts... >
{
};

template < typename T >
struct _contains_type< T >
{
        static constexpr bool value = false;
};

template < typename T, typename U >
struct _sigs_contains_type;

template < typename T, typename... Ts >
struct _sigs_contains_type< T, completion_signatures< Ts... > > : _contains_type< T, Ts... >
{
};

template < typename Sigs1, typename Sigs2 >
struct _check_compatible_sigs;

template < typename... Ts, typename Sigs2 >
struct _check_compatible_sigs< completion_signatures< Ts... >, Sigs2 >
{
        static_assert(
            ( _sigs_contains_type< Ts, Sigs2 >::value && ... ),
            "Completion signatures are not compatible" );
};

template < typename O, typename... Ts >
struct _type_merge_impl;

template < typename... S1, typename S, typename... S2, typename... Ts >
        requires( !_contains_type< S, S1... >::value )
struct _type_merge_impl< completion_signatures< S1... >, completion_signatures< S, S2... >, Ts... >
  : _type_merge_impl< completion_signatures< S1..., S >, completion_signatures< S2... >, Ts... >
{
};

template < typename... S1, typename S, typename... S2, typename... Ts >
        requires( _contains_type< S, S1... >::value )
struct _type_merge_impl< completion_signatures< S1... >, completion_signatures< S, S2... >, Ts... >
  : _type_merge_impl< completion_signatures< S1... >, completion_signatures< S2... >, Ts... >
{
};

template < typename... S1, typename... Ts >
struct _type_merge_impl< completion_signatures< S1... >, completion_signatures<>, Ts... >
  : _type_merge_impl< completion_signatures< S1... >, Ts... >
{
};
template < typename... S1 >
struct _type_merge_impl< completion_signatures< S1... > >
{
        using type = completion_signatures< S1... >;
};

template < typename... S >
struct _type_merge : _type_merge_impl< completion_signatures<>, S... >
{
};

template < typename... S >
using _type_merge_t = typename _type_merge< S... >::type;

// XXX: copy-pasta-festival from standard library concepts
template < class Token >
concept stoppable_token = requires( Token const tok ) {
        { tok.stop_requested() } noexcept -> std::same_as< bool >;
        { tok.stop_possible() } noexcept -> std::same_as< bool >;
        { Token( tok ) } noexcept;
} && std::copyable< Token > && std::equality_comparable< Token > && std::swappable< Token >;

template < class Token >
concept unstoppable_token =
    stoppable_token< Token > && requires { requires( !Token{}.stop_possible() ); };

template < class Source >
concept stoppable_source = requires( Source& src, Source const csrc ) {
        { csrc.get_token() } -> stoppable_token;
        { csrc.stop_possible() } noexcept -> std::same_as< bool >;
        { csrc.stop_requested() } noexcept -> std::same_as< bool >;
        { src.request_stop() } -> std::same_as< bool >;
};

struct inplace_stop_token;

struct inplace_stop_source
{
        inplace_stop_source() = default;

        inplace_stop_source( inplace_stop_source const& )            = delete;
        inplace_stop_source& operator=( inplace_stop_source const& ) = delete;

        inplace_stop_token get_token() const noexcept;

        bool stop_possible() const noexcept
        {
                return true;
        }

        bool stop_requested() const noexcept
        {
                return _stopped;
        }

        bool request_stop() noexcept
        {
                if ( _stopped )
                        return false;
                _stopped = true;
                return true;
        }

private:
        bool _stopped = false;
};

struct inplace_stop_token
{

        bool stop_requested() const noexcept
        {
                return _source && _source->stop_requested();
        }
        bool stop_possible() const noexcept
        {
                return _source != nullptr && _source->stop_possible();
        }

        friend bool
        operator==( inplace_stop_token const& lhs, inplace_stop_token const& rhs ) noexcept
        {
                return lhs._source == rhs._source;
        }

        friend bool
        operator!=( inplace_stop_token const& lhs, inplace_stop_token const& rhs ) noexcept
        {
                return lhs._source != rhs._source;
        }

private:
        inplace_stop_source const* _source = nullptr;

        inplace_stop_token( inplace_stop_source const* source ) noexcept
          : _source( source )
        {
        }

        friend struct inplace_stop_source;
};

inplace_stop_token inplace_stop_source::get_token() const noexcept
{
        return inplace_stop_token{ this };
}

template < signature... S >
struct event_entry : zll::ll_base< event_entry< S... > >
{
        _vtable< S... > const& vtable;

        event_entry( _vtable< S... > const& vtable ) noexcept
          : vtable( vtable )
        {
        }

        template < typename... Args >
        void _set_value( Args... args )
        {
                vtable.set( set_value_t{}, this, (Args&&) args... );
        }

        template < typename... Args >
        void _set_error( Args... args )
        {
                vtable.set( set_error_t{}, this, (Args&&) args... );
        }

        void _set_stopped()
        {
                vtable.set( set_stopped_t{}, this );
        }
};


template < signature... S >
struct _broadcast_core
{

        template < typename V >
        void set_value( V&& value )
        {
                for_each( [&]( auto& n ) {
                        n._set_value( value );
                } );
        }

        template < typename E1 >
        void set_error( E1&& err )
        {
                for_each( [&]( auto& n ) {
                        n._set_error( err );
                } );
        }

        void _set_stopped()
        {
                for_each( [&]( auto& n ) {
                        n._set_stopped();
                } );
        }

        void for_each( auto&& f )
        {
                auto l    = std::move( list );
                auto iter = l.begin();
                auto e    = l.end();
                while ( iter != e ) {
                        auto n = iter++;
                        f( *n );
                }
        }

        zll::ll_list< event_entry< S... > > list;
};

template < signature... S >
struct broadcast_sender;


template < signature... S >
struct broadcast_source
{
        broadcast_sender< S... > schedule()
        {
                return ( _core );
        }

        template < typename... Args >
        void set_value( Args... args )
        {
                _core.set_value( (Args&&) args... );
        }

        template < typename... Args >
        void set_error( Args... args )
        {
                _core.set_error( (Args&&) args... );
        }

        void set_stopped()
        {
                _core._set_stopped();
        }

private:
        _broadcast_core< S... > _core;
};


template < typename R, signature... S >
struct _list_op : event_entry< S... >, R
{
        using operation_state_concept = operation_state_t;

        _list_op( auto& list, R receiver )
          : event_entry< S... >( _vtable_of< _list_op, _vtable< S... > > )
          , R( std::move( receiver ) )
          , _list( list )
        {
        }

        void start()
        {
                _list.link_back( *this );
        }

private:
        zll::ll_list< event_entry< S... > >& _list;
};


template < typename K, signature... S >
struct seq_entry;

template < typename R, typename K, signature... S >
struct _heap_op : seq_entry< K, S... >, R
{
        using operation_state_concept = operation_state_t;

        _heap_op( auto& heap, K key, R receiver )
          : seq_entry< K, S... >( key, _vtable_of< _heap_op, _vtable< S... > > )
          , R( std::move( receiver ) )
          , _heap( heap )
        {
        }

        void start()
        {
                _heap.link( *this );
        }

private:
        zll::sh_heap< seq_entry< K, S... > >& _heap;
};

template < signature... S >
struct broadcast_sender
{
        using sender_concept = sender_t;

        broadcast_sender( _broadcast_core< S... >& core )
          : core_( core )
        {
        }

        template < typename Env >
        using _completions = completion_signatures< S... >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                return {};
        }

        struct _env
        {
        };

        _env get_env() const noexcept
        {
                return {};
        }

        template < receiver R >
        _list_op< R, S... > connect( R receiver )
        {
                return { core_.list, std::move( receiver ) };
        }

        _broadcast_core< S... >& core_;
};

template < typename K, signature... S >
struct seq_entry : zll::sh_base< seq_entry< K, S... > >
{

        _vtable< S... > const& vtable;

        K key;

        seq_entry( K k, _vtable< S... > const& vtable )
          : key( std::move( k ) )
          , vtable( vtable )
        {
        }

        template < typename... Args >
        void _set_value( Args... args )
        {
                vtable._set_value( this, (Args&&) args... );
        }

        template < typename... Args >
        void _set_error( Args... args )
        {
                vtable._set_error( this, (Args&&) args... );
        }

        void _set_stopped()
        {
                vtable._set_stopped( this );
        }

        constexpr bool operator<( seq_entry const& o ) const
        {
                return key < o.key;
        }
};

template < typename K, signature... S >
struct _seq_core
{
        template < typename V >
        void set_value( V&& value )
        {
                do_f( [&]( auto& n ) {
                        n._set_value( (V&&) value );
                } );
        }

        template < typename E1 >
        void set_error( E1&& err )
        {
                // XXX: should this treat all entries or just the front one?
                do_f( [&]( auto& n ) {
                        n._set_error( (E1&&) err );
                } );
        }

        void _set_stopped()
        {
                // XXX: should this treat all entries or just the front one?
                do_f( [&]( auto& n ) {
                        n._set_stopped();
                } );
        }

        void do_f( auto f )
        {
                if ( h.empty() )
                        return;

                auto& n = h.take();
                f( n );
        }

        [[nodiscard]] bool empty() const
        {
                return h.empty();
        }

        seq_entry< K, S... > const& front() const
        {
                return *h.top;
        }

        zll::sh_heap< seq_entry< K, S... > > h;
};

template < typename K, signature... S >
struct seq_sender;

template < typename K, signature... S >
struct seq_source
{

        // TODO: technically this is not spec-conforming
        seq_sender< K, S... > schedule( K key )
        {
                return { key, _core };
        }

        template < typename... Args >
        void set_value( Args... args )
        {
                _core.set_value( (Args&&) args... );
        }

        template < typename... Args >
        void set_error( Args... args )
        {
                _core.set_error( (Args&&) args... );
        }

        [[nodiscard]] bool empty() const
        {
                return _core.empty();
        }

        seq_entry< K, S... > const& front() const
        {
                return _core.front();
        }

private:
        _seq_core< K, S... > _core;
};

template < typename K, signature... S >
struct seq_sender
{
        using sender_concept = sender_t;

        seq_sender( K key, _seq_core< K, S... >& core )
          : key( key )
          , core_( core )
        {
        }

        using _completions = completion_signatures< S... >;

        template < typename Env >
        _completions get_completion_signatures( Env&& )
        {
                return {};
        }

        struct _env
        {
        };

        _env get_env() const noexcept
        {
                return {};
        }

        template < receiver R >
        _heap_op< R, K, S... > connect( R receiver ) &&
        {
                return { core_.h, std::move( key ), std::move( receiver ) };
        }

        K                     key;
        _seq_core< K, S... >& core_;
};

template < typename S, typename R >
using connect_type = decltype( std::declval< S >().connect( std::declval< R >() ) );

template < typename T >
struct _just_error
{
        using sender_concept = sender_t;

        T err;

        template < receiver R >
        struct _op
        {
                using operation_state_concept = operation_state_t;

                T err;
                R receiver;

                void start()
                {
                        receiver.set_error( std::move( err ) );
                }
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                return _op{ std::move( err ), std::move( receiver ) };
        }

        template < typename Env >
        using _completions = completion_signatures< set_error_t( T ) >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                return {};
        }

        struct _env
        {
        };

        _env get_env() const noexcept
        {
                return {};
        }
};

template < typename T >
_just_error< T > just_error( T err )
{
        return _just_error< T >{ std::move( err ) };
}


enum class _awaitable_state_e : uint8_t
{
        empty,
        value
};

struct _itask_op : zll::ll_base< _itask_op >
{
        virtual void resume() = 0;
};

// This now does not need itask_op to inherit from linked list -> do we still want that?
struct itask_core
{
        virtual void reschedule( _itask_op& ) = 0;
};

struct task_core : itask_core
{
        bool run_once()
        {
                if ( ready_tasks.empty() )
                        return false;
                auto& t = ready_tasks.front();
                ready_tasks.detach_front();
                t.resume();
                return true;
        }

        void run_n( std::size_t n )
        {
                for ( std::size_t i = 0; i < n; ++i )
                        if ( !run_once() )
                                break;
        }

        void reschedule( _itask_op& op ) override
        {
                ready_tasks.link_back( op );
        }

private:
        zll::ll_list< _itask_op > ready_tasks;
};

template < typename T >
struct _expected
{
        _expected() noexcept
        {
        }

        _awaitable_state_e state = _awaitable_state_e::empty;
        union
        {
                T val;
        };

        template < typename... Args >
        void set_value( Args&&... args ) noexcept
        {
                new ( (void*) &val ) T( (Args&&) args... );
                state = _awaitable_state_e::value;
        }

        ~_expected() noexcept
        {
                if ( state == _awaitable_state_e::value )
                        val.~T();
        }
};

template <>
struct _expected< void >
{
        _expected() noexcept     = default;
        _awaitable_state_e state = _awaitable_state_e::empty;

        void set_value() noexcept
        {
                state = _awaitable_state_e::value;
        }
};


template < typename PromiseType, typename S >
struct _awaitable
{
        _awaitable( S sender, std::coroutine_handle< PromiseType > h )
          : _exp()
          , _op( std::move( sender ).connect( _receiver{ ._exp = &_exp, ._cont = h } ) )
        {
        }

        [[nodiscard]] bool await_ready() const noexcept
        {
                return false;
        }

        void await_suspend( std::coroutine_handle< PromiseType > ) noexcept
        {
                _op.start();
        }

        auto await_resume() noexcept
        {
                if constexpr ( std::same_as< value_type, void > )
                        switch ( _exp.state ) {
                        case _awaitable_state_e::empty:
                                ECOR_ASSERT( false );
                        case _awaitable_state_e::value:
                                return;
                        }
                else
                        switch ( _exp.state ) {
                        case _awaitable_state_e::empty:
                                ECOR_ASSERT( false );
                        case _awaitable_state_e::value:

                                return std::move( _exp.val );
                        }
        }

        using _env         = decltype( std::declval< PromiseType >().get_env() );
        using _completions = _sender_completions_t< S, _env >;
        using _values      = _map_values_of_t< _completions >;
        using _errors      = _map_errors_of_t< _completions >;
        static_assert(
            std::variant_size_v< _values > <= 1,
            "Multiple set_value completions not supported" );

        template < typename T >
        struct _extract_type;

        template < typename... U >
        struct _extract_type< std::variant< std::tuple< U... > > >
        {
                using type = std::tuple< U... >;
        };

        template < typename U >
        struct _extract_type< std::variant< std::tuple< U > > >
        {
                using type = U;
        };

        template <>
        struct _extract_type< std::variant< std::tuple<> > >
        {
                using type = void;
        };

        template <>
        struct _extract_type< std::variant<> >
        {
                using type = void;
        };

        using value_type = typename _extract_type< _values >::type;

        using _aw_tp = _expected< value_type >;

        struct _receiver
        {
                using receiver_concept = receiver_t;

                _aw_tp*                              _exp;
                std::coroutine_handle< PromiseType > _cont;

                template < typename... Ts >
                void set_value( Ts&&... vals ) noexcept
                {
                        _exp->set_value( (Ts&&) vals... );
                        _cont.promise().reschedule();
                }

                template < typename... Es >
                void set_error( Es&&... errs ) noexcept
                {
                        _cont.promise()._g.set_error( (Es&&) errs... );
                }

                void set_stopped() noexcept
                {
                        _cont.promise()._g.set_stopped();
                }

                auto get_env() const noexcept -> decltype( std::declval< PromiseType >().get_env() )
                {
                        return _cont.promise().get_env();
                }
        };

        using _op_t = connect_type< S, _receiver >;

        _aw_tp _exp;
        _op_t  _op;
};


struct task_allocator
{
        template < typename M >
        task_allocator( M& m )
          : mem( &m )
        {
                alloc = +[]( void* mem, std::size_t const sz, std::size_t const align ) {
                        return allocate( *( (M*) mem ), sz, align );
                };
                dealloc = +[]( void* mem, void* p, std::size_t const sz, std::size_t const align ) {
                        deallocate( *( (M*) mem ), p, sz, align );
                };
        }

        void* ( *alloc )( void*, std::size_t const, std::size_t const )         = nullptr;
        void ( *dealloc )( void*, void*, std::size_t const, std::size_t const ) = nullptr;
        void* mem                                                               = nullptr;
};

inline void* allocate( task_allocator& t, std::size_t const sz, std::size_t const align )
{
        return t.alloc( t.mem, sz, align );
}

inline void deallocate( task_allocator& t, void* p, std::size_t const sz, std::size_t const align )
{
        return t.dealloc( t.mem, p, sz, align );
}

struct task_ctx
{
        task_allocator alloc;
        task_core      core;

        task_ctx( auto& mem )
          : alloc( mem )
        {
        }
};

inline itask_core& get_task_core( task_ctx& t )
{
        return t.core;
}

inline task_allocator& get_task_alloc( task_ctx& t )
{
        return t.alloc;
}

struct task_default_cfg
{
        using extra_error_signatures = completion_signatures<>;
};

template < typename T >
concept task_config = requires() { typename T::extra_error_signatures; };

template < typename T, task_config CFG = task_default_cfg >
struct task;

enum class task_error : uint8_t
{
        none,
        task_unfinished,
        task_allocation_failure,
        task_already_done,
};

template < typename Task >
struct _task_finish_guard
{
        template < typename U >
        void set_error( U&& err ) noexcept
        {
                ECOR_ASSERT( this->_recv );
                if ( this->_recv ) {
                        auto* r = std::exchange( this->_recv, nullptr );
                        r->set( set_error_t{}, _obj, (U&&) err );
                }
        }

        template < typename... U >
        void set_value( U&&... v ) noexcept
        {
                ECOR_ASSERT( this->_recv );
                if ( this->_recv ) {
                        auto* r = std::exchange( this->_recv, nullptr );
                        r->set( set_value_t{}, this->_obj, (U&&) v... );
                }
        }

        void set_stopped() noexcept
        {
                ECOR_ASSERT( this->_recv );
                if ( this->_recv ) {
                        auto* r = std::exchange( this->_recv, nullptr );
                        r->set( set_stopped_t{}, this->_obj );
                }
        }

        void on_final_suspend() noexcept
        {
                if ( _recv ) {
                        auto* r = std::exchange( this->_recv, nullptr );
                        r->set( set_error_t{}, _obj, task_error::task_unfinished );
                }
        }

        template < typename U >
        void setup_continuable( U& p )
        {
                this->_recv = &_vtable_of< U, vtable >;
                this->_obj  = static_cast< void* >( &p );
        }

protected:
        using sigs          = typename Task::_completions;
        using vtable        = typename _vtable_of_sigs< sigs >::type;
        vtable const* _recv = nullptr;
        void*         _obj  = nullptr;
};

struct _promise_base : _itask_op
{
        static constexpr std::size_t align = alignof( std::max_align_t );
        struct vtable
        {
                task_allocator* mem = nullptr;
        };
        static constexpr std::size_t spacing = align > sizeof( vtable ) ? align : sizeof( vtable );

        // XXX: check the noexcept thing

        static void* alloc( std::size_t sz, task_allocator& mem ) noexcept
        {
                sz += spacing;
                void* const vp = allocate( mem, sz, align );
                if ( !vp )
                        return nullptr;
                vtable vt{
                    .mem = &mem,
                };
                std::memcpy( vp, (void const*) &vt, sizeof( vt ) );
                return ( (char*) vp ) + spacing;
        }

        static void dealloc( void* const ptr, std::size_t const sz ) noexcept
        {
                void*  beg = ( (char*) ptr ) - spacing;
                vtable vt;
                std::memcpy( (void*) &vt, beg, sizeof( vt ) );
                deallocate( *vt.mem, beg, sz + spacing, align );
        }

        void* operator new( std::size_t const sz, auto&& ctx, auto&&... ) noexcept
        {
                task_allocator& a = get_task_alloc( ctx );
                return alloc( sz, a );
        }

        void operator delete( void* const ptr, std::size_t const sz ) noexcept
        {
                dealloc( ptr, sz );
        }

        _promise_base( auto&& ctx, auto&&... )
          : core( get_task_core( ctx ) )
        {
        }

        std::suspend_always initial_suspend() noexcept
        {
                return {};
        }

        itask_core& core;

        void unhandled_exception() noexcept
        {
        }

        struct _env
        {
        };

        _env get_env() noexcept
        {
                return {};
        }

        void reschedule()
        {
                core.reschedule( *this );
        }
};


template < typename T >
struct _promise_type_value
{
        _task_finish_guard< T > _g;

        void return_value( typename T::value_type v ) noexcept
        {
                _g.set_value( std::move( v ) );
        }

        std::suspend_always final_suspend() noexcept
        {
                _g.on_final_suspend();
                return {};
        }
};

template < typename T >
        requires( std::same_as< typename T::value_type, void > )
struct _promise_type_value< T >
{
        _task_finish_guard< T > _g;

        void return_void() noexcept
        {
                _g.set_value();
        }

        std::suspend_always final_suspend() noexcept
        {
                _g.on_final_suspend();
                return {};
        }
};

template < typename Task >
struct _promise_type : _promise_base, _promise_type_value< Task >
{
        using value_type = typename Task::value_type;
        using _promise_base::_promise_base;

        static Task get_return_object_on_allocation_failure()
        {
                // write test cases for this
                return {
                    std::coroutine_handle< _promise_type >{}, task_error::task_allocation_failure };
        }

        Task get_return_object()
        {
                return { std::coroutine_handle< _promise_type >::from_promise( *this ) };
        }

        template < sender S >
        auto await_transform( S sender ) noexcept
        {
                // XXX: leaky implementation detail
                using compls = _sender_completions_t< S, typename _promise_base::_env >;
                using errs   = _filter_errors_of_t< compls >;
                _check_compatible_sigs< errs, typename Task::_error_completions > _{};
                using vals = _map_values_of_t< compls >;
                static_assert(
                    std::variant_size_v< vals > <= 1,
                    "Sender used in co_await must only complete with a single set_value signature" );
                return _awaitable< _promise_type, S >{
                    std::move( sender ),
                    std::coroutine_handle< _promise_type >::from_promise( *this ) };
        }

        template < typename Err >
                requires( _sigs_contains_type<
                          set_error_t( Err ),
                          typename Task::_error_completions >::value )
        auto await_transform( Err e ) noexcept
        {
                return await_transform( just_error( std::move( e ) ) );
        }


        void resume() override
        {
                auto h = std::coroutine_handle< _promise_type >::from_promise( *this );
                h.resume();
        }
};
template < typename T, task_config CFG, typename R >
struct _task_op
{
        using operation_state_concept = operation_state_t;

        _task_op( task_error err, std::coroutine_handle< _promise_type< task< T, CFG > > > h, R r )
          : _h( h )
          , _recv( std::move( r ) )
          , _error( err )
        {
        }

        _task_op( _task_op const& )            = delete;
        _task_op& operator=( _task_op const& ) = delete;
        _task_op( _task_op&& other ) noexcept
          : _h( std::exchange( other._h, nullptr ) )
          , _recv( std::move( other._recv ) )
          , _error( other._error )
        {
        }

        void start()
        {
                if ( _error != task_error::none ) {
                        _recv.set_error( _error );
                        return;
                }
                if ( !_h || _h.done() ) {
                        _recv.set_error( task_error::task_already_done );
                        return;
                }
                _h.promise()._g.setup_continuable( _recv );
                _h.resume();
        }

        ~_task_op()
        {
                if ( this->_h && !this->_h.done() )
                        this->_h.destroy();
        }

        std::coroutine_handle< _promise_type< task< T, CFG > > > _h;
        R                                                        _recv;
        task_error                                               _error;
};

template < typename T, task_config CFG >
struct task
{
        using sender_concept = sender_t;
        using value_type     = T;
        using promise_type   = _promise_type< task >;

        // XXX: append?
        using _error_completions = _type_merge_t<
            completion_signatures< set_error_t( task_error ) >,
            typename CFG::extra_error_signatures >;

        // XXX: convert to simple concat instead of merge
        using _completions = _type_merge_t<
            completion_signatures< _value_setter_t< T >, set_stopped_t() >,
            _error_completions >;
        static_assert(
            alignof( promise_type ) <= alignof( std::max_align_t ),
            "Unsupported alignment" );

        task(
            std::coroutine_handle< promise_type > handle,
            task_error                            error = task_error::none ) noexcept
          : _h( handle )
          , _error( error )
        {
        }

        task( task const& )            = delete;
        task& operator=( task const& ) = delete;
        task( task&& other ) noexcept
          : _h( std::exchange( other._h, nullptr ) )
          , _error( other._error )
        {
        }

        task& operator=( task&& other ) noexcept
        {
                if ( this != &other ) {
                        task tmp = std::move( other );
                        std::swap( _h, tmp._h );
                }
                return *this;
        }

        template < typename Env >
        _completions get_completion_signatures( Env&& )
        {
                return {};
        }

        struct _env
        {
        };

        _env get_env() const noexcept
        {
                return {};
        }

        template < receiver R >
        auto connect( R receiver ) &&
        {
                auto tmp = _h;
                _h       = nullptr;
                return _task_op< T, CFG, R >{ _error, std::move( tmp ), std::move( receiver ) };
        }

        ~task()
        {
                if ( _h && !_h.done() )
                        _h.destroy();
        }

private:
        std::coroutine_handle< promise_type > _h;
        task_error                            _error = task_error::none;
};

template < typename S1, typename S2, typename R >
struct _or_op
{
        using operation_state_concept = operation_state_t;

        _or_op( S1 s1, S2 s2, R r )
          : _recv( std::move( r ) )
          , _op1( std::move( s1 ).connect( _r{ ._fired = _fired, ._recv = _recv } ) )
          , _op2( std::move( s2 ).connect( _r{ ._fired = _fired, ._recv = _recv } ) )
        {
        }

        // XXX: cancel of the other one shall be used isntead
        void start()
        {
                _op1.start();
                _op2.start();
        }

        struct _r
        {
                using receiver_concept = receiver_t;

                bool& _fired;
                R&    _recv;

                template < typename... Args >
                void set_value( Args&&... v )
                {
                        if ( _fired )
                                return;
                        _fired = true;
                        _recv.set_value( (Args&&) v... );
                }

                template < typename T >
                void set_error( T&& e )
                {
                        if ( _fired )
                                return;
                        _fired = true;
                        _recv.set_error( (T&&) e );
                }

                // XXX: investigate whenever this should cancel the other operation
                void set_stopped()
                {
                        if ( _fired )
                                return;
                        _fired = true;
                        _recv.set_stopped();
                }

                auto&& get_env() noexcept
                {
                        return _recv.get_env();
                }
        };

        R    _recv;
        bool _fired = false;

        connect_type< S1, _r > _op1{};
        connect_type< S2, _r > _op2{};
};

template < typename S1, typename S2 >
struct _or_sender
{
        using sender_concept = sender_t;

        _or_sender( S1 s1, S2 s2 )
          : _s1( std::move( s1 ) )
          , _s2( std::move( s2 ) )
        {
        }

        template < typename Env >
        using _completions = typename _type_merge<
            _sender_completions_t< S1, Env >,
            _sender_completions_t< S2, Env > >::type;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                return {};
        }

        struct _env
        {
        };

        _env get_env() const noexcept
        {
                return {};
        }

        template < receiver R >
        _or_op< S1, S2, R > connect( R receiver )
        {
                return { std::move( _s1 ), std::move( _s2 ), std::move( receiver ) };
        }

private:
        S1 _s1;
        S2 _s2;
};

template < typename S1, typename S2 >
_or_sender< S1, S2 > operator||( S1 s1, S2 s2 )
{
        return { std::move( s1 ), std::move( s2 ) };
}

template < sender S >
struct _as_variant
{
        using sender_concept = sender_t;

        template < typename Env >
        using _s_completions = _sender_completions_t< S, Env >;

        template < typename Env >
        using _values = typename _filter_map_tag<
            std::variant<>,
            set_value_t,
            _s_completions< Env >,
            _type_identity_t >::type;

        template < typename Env >
        using _error = _filter_errors_of_t< _s_completions< Env > >;

        template < typename Env >
        using _stopped = _filter_stopped_of_t< _s_completions< Env > >;

        template < typename Env >
        using _completions = _type_merge_t<
            completion_signatures< set_value_t( _values< Env > ) >,
            _error< Env >,
            _stopped< Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                return {};
        }

        struct _env
        {
        };

        _env get_env() const noexcept
        {
                return {};
        }

        template < typename R >
        struct _recv : R
        {
                using receiver_concept = receiver_t;

                _recv( R r )
                  : R( std::move( r ) )
                {
                }

                using value_t = _values< decltype( std::declval< R >().get_env() ) >;

                template < typename T >
                void set_value( T&& val ) noexcept
                {
                        R::set_value( value_t{ (T&&) val } );
                }
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                return std::move( _s ).connect( _recv< R >{ std::move( receiver ) } );
        }

        _as_variant( S s )
          : _s( std::move( s ) )
        {
        }

        S _s;
};

static inline struct as_variant_t
{
        template < sender S >
        auto operator()( S s ) const noexcept
        {
                return _as_variant< S >{ std::move( s ) };
        }

} as_variant;

auto operator|( auto s, as_variant_t ) noexcept
{
        return as_variant( std::move( s ) );
}


template < sender S >
struct _err_to_val
{
        using sender_concept = sender_t;

        template < typename Env >
        using _s_completions = _sender_completions_t< S, Env >;

        template < typename Env >
        using _s_values = _filter_values_of_t< _s_completions< Env > >;

        template < typename Env >
        using _stopped = _filter_stopped_of_t< _s_completions< Env > >;

        template < typename Env >
        using _s_errors_as_val = typename _filter_map_tag<
            completion_signatures<>,
            set_error_t,
            _s_completions< Env >,
            _tag_identity_t< set_value_t >::type >::type;

        template < typename Env >
        using _completions =
            _type_merge_t< _s_values< Env >, _s_errors_as_val< Env >, _stopped< Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                return {};
        }

        struct _env
        {
        };

        _env get_env() const noexcept
        {
                return {};
        }

        template < typename R >
        struct _recv : R
        {
                using receiver_concept = receiver_t;

                _recv( R r )
                  : R( std::move( r ) )
                {
                }

                template < typename... Ts >
                void set_error( Ts&&... errs ) noexcept
                {
                        R::set_value( (Ts&&) errs... );
                }
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                return std::move( _s ).connect( _recv< R >{ std::move( receiver ) } );
        }

        S _s;
};

static inline struct err_to_val_t
{
        template < sender S >
        auto operator()( S s ) const noexcept
        {
                return _err_to_val< S >{ std::move( s ) };
        }

} err_to_val;

auto operator|( auto s, err_to_val_t ) noexcept
{
        return err_to_val( std::move( s ) );
}

template < sender S >
struct _sink_err
{
        using sender_concept = sender_t;

        template < typename Env >
        using _s_completions = _sender_completions_t< S, Env >;

        template < typename Env >
        using _s_errors_as_val = std::optional< typename _filter_map_tag<
            std::variant<>,
            set_error_t,
            _s_completions< Env >,
            _type_identity_t >::type >;

        template < typename Env >
        using _s_values = _filter_values_of_t< _s_completions< Env > >;

        template < typename Env >
        using _s_stopped = _filter_stopped_of_t< _s_completions< Env > >;

        template < typename Env >
        using _completions = _type_merge_t<
            completion_signatures< set_value_t( _s_errors_as_val< Env > ) >,
            _s_stopped< Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                static_assert(
                    std::same_as< _s_values< Env >, completion_signatures<> > ||
                        std::same_as< _s_values< Env >, completion_signatures< set_value_t() > >,
                    "Sender used with sink_err must not complete with set_value, or there has to be only one set_value of shape set_value_t()" );
                return {};
        }

        struct _env
        {
        };

        _env get_env() const noexcept
        {
                return {};
        }

        template < typename R >
        struct _recv : R
        {
                using receiver_concept = receiver_t;

                _recv( R r )
                  : R( std::move( r ) )
                {
                }

                using value_t = _s_errors_as_val< decltype( std::declval< R >().get_env() ) >;

                template < typename T >
                void set_error( T&& err ) noexcept
                {
                        R::set_value( value_t{ (T&&) err } );
                }
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                return std::move( _s ).connect( _recv< R >{ std::move( receiver ) } );
        }

        S _s;
};

static inline struct sink_err_t
{

        auto operator()( sender auto s ) const noexcept
        {
                return _sink_err{ std::move( s ) };
        }

} sink_err;

auto operator|( sender auto s, sink_err_t ) noexcept
{
        return _sink_err{ std::move( s ) };
}

template < sender S, template < typename R > class CB, typename DataType >
struct _then_cb
{
        // XXX: Missing sender_concept tag and get_completion_signatures()
        // These are needed for P2300 compliance, but get_completion_signatures() must
        // compute the transformed signatures based on CB's transformation of values/errors.
        // Currently we lack the API to extract signature transformation from CB.

        S         s;
        DataType& cb;

        template < receiver R >
        auto connect( R receiver )
        {
                return s.connect( CB{ std::move( receiver ), cb } );
        }
};

template < template < typename R > class CB, typename DataType >
struct _then_t
{
        DataType& data;
};

template < sender S, template < typename R > class CB, typename DataType >
auto operator|( S s, _then_t< CB, DataType > t ) noexcept
{
        return _then_cb< S, CB, DataType >{ std::move( s ), t.data };
}

template < template < typename R > class CB, typename DataType >
auto then( DataType& data ) noexcept
{
        return _then_t< CB, DataType >{ data };
}

// XXX: replace this iwth actually sender: repeat_until
template < sender S >
struct repeater
{
        repeater( S s )
          : _s( std::move( s ) )
        {
        }


        // XXX: this is wrong, lifetimes will get broken
        struct _recv
        {
                using receiver_concept = receiver_t;

                repeater* r;

                template < typename... Ts >
                void set_value( Ts&&... ) noexcept
                {
                        r->start();
                }

                template < typename... Es >
                void set_error( Es&&... ) noexcept
                {
                        r->start();
                }

                void set_stopped() noexcept
                {
                        r->start();
                }

                struct _env
                {
                };

                _env get_env() const noexcept
                {
                        return {};
                }
        };

        void start()
        {
                _opop.emplace( _s.connect( _recv{ .r = this } ) );
                _opop->start();
        }

        S _s;

        std::optional< connect_type< S, _recv > > _opop;
};

}  // namespace ecor
