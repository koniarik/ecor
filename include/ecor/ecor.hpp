/// MIT License
///
/// Copyright (c) 2025 koniarik
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

#pragma once

#include <atomic>
#include <concepts>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <iterator>
#include <limits>
#include <memory>
#include <new>
#include <optional>
#include <span>
#include <type_traits>
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

template < typename T >
struct _tag
{
};

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

/// Sender concept tag, used for marking types as senders.
struct sender_t
{
};

/// Receiver concept tag, used for marking types as receivers.
struct receiver_t
{
};

/// Operation state concept tag, used for marking types as operation states.
struct operation_state_t
{
};

/// Type tag for set_value completion signal.
struct set_value_t
{
};

/// Type tag for set_error completion signal.
struct set_error_t
{
};

/// Type tag for set_stopped completion signal.
struct set_stopped_t
{
};

/// Type tag for get_stopped query, private for ecor. Not intended for public use.
struct _get_stopped_t
{
};

/// Type container for completion signatures, used to specify the set of possible completion
/// signals.
template < typename... S >
struct completion_signatures
{
};

/// Standard empty environment.
struct empty_env
{
};

/// Empty base class for base-class customization point.
struct noop_base
{
};

/// Operation state type of a sender-receiver connection. The type returned by connecting a sender
/// to a receiver.
template < typename S, typename R >
using connect_type = decltype( std::move( std::declval< S >() ).connect( std::declval< R >() ) );

// ------------------------------------------------------------------------------

template < typename T >
constexpr T _align_idx( uint8_t const* buff, T idx, std::size_t align ) noexcept
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

/// Helper to select the smallest unsigned integer type that can index a buffer of size N.
template < std::size_t N >
using smallest_index_type = decltype( _pick_index_type< N >() );


struct _allocate_t
{
        template < typename T >
                requires( requires( T x ) { x.allocate( std::size_t{}, std::size_t{} ); } )
        void* operator()( T& x, std::size_t bytes, std::size_t align ) const
            noexcept( noexcept( x.allocate( bytes, align ) ) )
        {
                return x.allocate( bytes, align );
        }
};
/// Allocate CPO for memory resources.
static constexpr _allocate_t allocate{};


struct _deallocate_t
{
        template < typename T >
                requires( requires( T x, void* p, std::size_t b, std::size_t a ) {
                        x.deallocate( p, b, a );
                } )
        void operator()( T& x, void* p, std::size_t bytes, std::size_t align ) const
            noexcept( noexcept( x.deallocate( p, bytes, align ) ) )
        {
                x.deallocate( p, bytes, align );
        }
};
/// Deallocate CPO for memory resources.
static constexpr _deallocate_t deallocate{};

/// Type is memory resource if it can be used with allocate and deallocate CPOs.
template < typename T >
concept memory_resource = requires( T a, std::size_t bytes, std::size_t align, void* p ) {
        { allocate( a, bytes, align ) } -> std::same_as< void* >;
        { deallocate( a, p, bytes, align ) } -> std::same_as< void >;
};


/// Unique span with custom deleter, useful for managing memory from custom memory resources.
/// Combines the semantics of std::unique_ptr and std::span.
///
template <
    typename T,
    std::size_t Extent = std::dynamic_extent,
    typename Deleter   = std::default_delete< T[] > >
struct unique_span : std::span< T, Extent >
{
        using span_type = std::span< T, Extent >;

        unique_span() noexcept = default;

        /// Construct a unique_span with given data, size, and deleter.
        unique_span( T* data, std::size_t size, Deleter deleter = Deleter{} ) noexcept(
            std::is_nothrow_move_constructible_v< Deleter > )
          : span_type( data, size )
          , _deleter( std::move( deleter ) )
        {
        }

        unique_span( unique_span const& )            = delete;
        unique_span& operator=( unique_span const& ) = delete;

        /// Move constructor - transfers ownership from other to this unique_span.
        unique_span( unique_span&& other ) noexcept
          : span_type( other.data(), other.size() )
          , _deleter( std::move( other._deleter ) )
        {
                *(span_type*) &other = span_type{};
        }

        /// Move assignment operator - transfers ownership from other to this unique_span.
        unique_span& operator=( unique_span&& other ) noexcept
        {
                if ( this != &other ) {
                        if ( this->data() )
                                _deleter( this->data() );
                        *(span_type*) this   = span_type( other.data(), other.size() );
                        _deleter             = std::move( other._deleter );
                        *(span_type*) &other = span_type{};
                }
                return *this;
        }

        /// Release ownership of the managed span and return it. After this call, the unique_span is
        /// empty.
        std::span< T, Extent > release() noexcept
        {
                T*          data   = this->data();
                std::size_t size   = this->size();
                *(span_type*) this = span_type{};
                return span_type( data, size );
        }

        /// Destructor - calls the deleter on the managed span if it is not empty.
        ~unique_span()
        {
                if ( this->data() )
                        _deleter( this->data() );
        }

private:
        [[no_unique_address]] Deleter _deleter;
};

/// Circular buffer memory resource, manages a provided memory block as a circular buffer for
/// dynamic allocations. Uses an index-based linked list to track allocated blocks.
///
/// The buffer is treated as a circular space, and allocations are made by finding a suitable space
/// after the last allocated block. Deallocations mark blocks as free, and the allocator can reuse
/// freed space for future allocations. The internal linked list structure allows for efficient
/// tracking of allocated blocks and free space.
///
/// Note that if first block is allocated and is not freed, the buffer is considered full, even if
/// there is free space between first block and the last block.
///
/// The template parameters are:
/// - `IndexType`: An unsigned integer type used for indexing into the buffer. The maximum
///   capacity of the buffer is limited by the maximum value of this type. The implementation
///   provides a helper to automatically select an appropriate index type based on the buffer size.
/// - `Base`: An optional base class that can be used for customization. This allows the
///    user to pick an interface used for the memory resource.
///
template < typename IndexType, typename Base = noop_base >
struct circular_buffer_memory : Base
{
        static_assert(
            std::is_unsigned< IndexType >::value,
            "IndexType must be an unsigned integer type" );

        /// Type of the index used for tracking allocations.
        using index_type = IndexType;

        template < std::size_t N >
                requires( std::numeric_limits< IndexType >::max() >= N )
        constexpr circular_buffer_memory( std::span< uint8_t, N > b ) noexcept
          : _buff( std::move( b ) )
        {
        }
        template < std::size_t N >
                requires( std::numeric_limits< IndexType >::max() >= N )
        constexpr circular_buffer_memory( uint8_t ( &b )[N] ) noexcept
          : _buff( b )
        {
        }

        struct _deleter
        {
                circular_buffer_memory< IndexType, Base >* mem;

                template < typename T >
                void operator()( T* p ) noexcept
                {
                        if ( p ) {
                                p->~T();
                                mem->deallocate( p );
                        }
                }
        };

        /// Unique pointer type for objects allocated from this memory resource, with automatic
        /// deallocation.
        template < typename T >
        using uptr = std::unique_ptr< T, _deleter >;

        /// Allocate and construct an object of type T with given arguments, returning a unique
        /// pointer that manages the allocated object. The memory for the object is allocated from
        /// this memory resource, and will be automatically deallocated when the unique pointer is
        /// destroyed.
        ///
        /// Undefined behavior if circular_buffer_memory ends lifetime before the unique pointer.
        ///
        template < typename T, typename... Args >
        uptr< T > make( Args&&... args )
        {
                void* p = allocate( sizeof( T ), alignof( T ) );
                if ( !p )
                        return { nullptr, _deleter{ this } };
                T* t = new ( p ) T( (Args&&) args... );
                return { t, _deleter{ this } };
        }

        /// Unique span type for arrays allocated from this memory resource, with automatic
        /// deallocation.
        template < typename T, std::size_t Extent = std::dynamic_extent >
        using uspan = unique_span< T, Extent, _deleter >;

        /// Allocate and construct an array of type T with given size, returning a unique span that
        /// manages the allocated array. The memory for the array is allocated from this memory
        /// resource, and will be automatically deallocated when the unique span is destroyed.
        ///
        /// Undefined behavior if circular_buffer_memory ends lifetime before the unique span.
        ///
        template < typename T >
        uspan< T > make_span( std::size_t n )
        {
                void* p = allocate( sizeof( T ) * n, alignof( T ) );
                if ( !p )
                        return uspan< T >{ nullptr, 0, _deleter{ this } };
                auto* pp = new ( p ) T[n]();
                return uspan< T >{ pp, n, _deleter{ this } };
        }

        /// Overload for fixed-size arrays, deduces the size from the template parameter. Rest is
        /// same as for the other make_span.
        template < typename T, std::size_t Extent >
        uspan< T, Extent > make_span()
        {
                constexpr std::size_t n  = Extent;
                auto                  sp = make_span< T >( n );
                return sp;
        }

        /// Allocate `bytes` with `align`, returns nullptr if no space is available.
        /// The returned pointer is valid until the memory resource is destroyed or the memory is
        /// deallocated.
        ///
        /// User is expected to call deallocate with the same size and alignment when the memory is
        /// no longer needed.
        [[nodiscard]] void* allocate( std::size_t bytes, std::size_t align ) noexcept
        {
                auto* p = _allocate( _buff, bytes, align );
                return p;
        }

        /// Deallocate pointer previously allocated by allocate()
        void deallocate( void* p, std::size_t bytes, std::size_t align ) noexcept
        {
                std::ignore = bytes;
                std::ignore = align;
                _deallocate( _buff.data(), (uint8_t*) p );
        }

        /// Deallocate pointer previously allocated by allocate(), overload without size and
        /// alignment. This is provided for convenience.
        void deallocate( void* p ) noexcept
        {
                _deallocate( _buff.data(), (uint8_t*) p );
        }

        /// Get total capacity of the buffer in bytes.
        [[nodiscard]] std::size_t capacity() const noexcept
        {
                return _buff.size();
        }

        /// Get total used bytes in the buffer.
        [[nodiscard]] std::size_t used_bytes() const noexcept
        {
                if ( _first == npos )
                        return 0;
                if ( _first <= _last )
                        return ( _next - _first );
                return ( _buff.size() - _first ) + _next;
        }

        /// Special value indicating no position.
        static constexpr index_type npos = std::numeric_limits< index_type >::max();

        /// Internal node structure for tracking allocated blocks. Each node represents an allocated
        /// block and contains indices to the next and previous nodes in the linked list. The actual
        /// data for the block starts immediately after the node header in the buffer.
        ///
        /// Public for testing purposes, but not intended for external use.
        struct _node
        {
                index_type next_idx = npos;
                index_type prev_idx = npos;
        };

private:
        /// XXX: verify that test case scenarion in which we use this as a "stack" exists, it is
        /// expected to behave well in that case

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
                        idx      = _align_idx( buff.data(), sizeof( _node ), align );
                        capacity = buff.size() - idx;
                } else if ( _first <= _next ) {
                        // Non-overflow state: [ ][x][x][x][ ][ ]
                        {
                                idx = _align_idx( buff.data(), _next + sizeof( _node ), align );
                                capacity = buff.size() - idx;
                                if ( idx < buff.size() && capacity >= (int) bytes )
                                        return idx - sizeof( _node );
                        }
                        // Non-overflow state, but overflow triggered: [ ][ ][ ][x][x][x]
                        {
                                idx      = _align_idx( buff.data(), sizeof( _node ), align );
                                capacity = idx - _first;
                        }

                } else {  // _first > _next
                          // Overflow state: [x][x][ ][ ][x][x]
                        idx      = _align_idx( buff.data(), _next + sizeof( _node ), align );
                        capacity = _first - idx;
                }
                if ( idx < buff.size() && capacity >= (int) bytes )
                        return idx - sizeof( _node );
                return npos;
        }

        void _set_node( uint8_t* buff, index_type idx, _node const& n ) noexcept
        {
                std::memcpy( buff + idx, &n, sizeof( _node ) );
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
                _node const n{
                    .next_idx = npos,
                    .prev_idx = _last,
                };
                _last = idx;
                _next = idx + sizeof( _node ) + bytes;
                _set_node( buff.data(), idx, n );
                return p + sizeof( _node );
        }

        void _deallocate( uint8_t* buff, void* p ) noexcept
        {
                auto*            pp  = (uint8_t*) p - sizeof( _node );
                index_type const idx = pp - buff;
                _node const      n   = _get_node( buff, idx );

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

                _set_node( buff, idx, _node{ .next_idx = 0, .prev_idx = 0 } );
        }

public:
        /// Get the node header for the block at given index. The returned node contains the next
        /// and previous indices for the linked list. The actual data for the block starts
        /// immediately after the node header in the buffer. This is provided for testing purposes
        /// to verify the internal structure of the buffer, but is not intended for external use.
        _node _get_node( uint8_t* buff, index_type idx ) const noexcept
        {
                _node n;
                std::memcpy( &n, buff + idx, sizeof( _node ) );
                return n;
        }

        /// Index of first node in the list, npos if empty. Public for testing purposes, but not
        /// intended for external use.
        index_type _first = npos;
        /// Index of last node in the list, npos if empty. Public for testing purposes, but not
        /// intended for external use.
        index_type _last = npos;
        /// Index of first free byte after _last, npos if empty. Public for testing purposes, but
        /// not intended for external use.
        index_type _next = npos;

private:
        std::span< uint8_t > _buff;
};

/// Allocator that uses reference to circular_buffer_memory for allocating memory. This can be used
/// to create standard library containers that use circular_buffer_memory for their allocations.
///
/// The template parameters are:
/// - `T`: The type of objects to allocate.
/// - `IndexType`: The index type used by the circular_buffer_memory.
/// - `Base`: The base class used by the circular_buffer_memory, included here for compatibility
///   with circular_buffer_memory's template parameters.
///
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

        /// Construct an allocator that uses the given circular_buffer_memory for allocations.
        explicit circular_buffer_allocator(
            circular_buffer_memory< IndexType, Base >& buffer ) noexcept
          : _buffer( &buffer )
        {
        }

        /// Copy constructor, allows copying the allocator. The copied allocator will refer to the
        /// same circular_buffer_memory.
        circular_buffer_allocator( circular_buffer_allocator const& ) noexcept = default;

        /// Template copy constructor, allows copying from an allocator of a different type. The
        /// copied allocator will refer to the same circular_buffer_memory.
        template < typename U >
        circular_buffer_allocator(
            circular_buffer_allocator< U, IndexType, Base > const& other ) noexcept
          : _buffer( other._buffer )
        {
        }

        /// Allocate memory for n objects of type T, returns pointer to the allocated memory. The
        /// memory is allocated from the circular_buffer_memory associated with this allocator. The
        /// returned pointer is valid until the memory resource is destroyed or the memory is
        /// deallocated.
        ///
        /// Throws std::bad_alloc if allocation fails.
        ///
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

        /// Deallocate memory previously allocated by allocate(). The pointer must have been
        /// returned by a previous call to allocate() on this allocator, and the size n must match
        /// the size used in that call. The memory is deallocated back to the circular_buffer_memory
        /// associated with this allocator.
        void deallocate( T* ptr, std::size_t n )
        {
                if ( ptr )
                        _buffer->deallocate( ptr, n * sizeof( T ), alignof( T ) );
        }

        /// Equality comparison operators, allocators are equal if they refer to the same
        /// circular_buffer_memory.
        bool operator==( circular_buffer_allocator const& other ) const noexcept
        {
                return _buffer == other._buffer;
        }

        /// Inequality comparison operator, defined in terms of operator==.
        bool operator!=( circular_buffer_allocator const& other ) const noexcept
        {
                return !( *this == other );
        }

        /// Grant access to other allocator types for copying. This allows allocators of different
        /// types to access each other's internal buffer pointer when copying.
        template < typename U, typename IdxType, typename Base2 >
        friend struct circular_buffer_allocator;

private:
        circular_buffer_memory< IndexType, Base >* _buffer;
};

// ------------------------------------------------------------------------------

struct get_env_t
{
        template < typename T >
        decltype( auto ) operator()( T const& o ) const noexcept
        {
                static constexpr bool v = requires( get_env_t t ) {
                        { o.get_env() };
                };
                if constexpr ( v )
                        return o.get_env();
                else
                        return empty_env{};
        }
};
/// CPO for getting the environment from senders and receivers. If the type does not have a get_env
/// member function, returns empty_env.
inline constexpr get_env_t get_env{};

struct get_completion_signatures_t
{
        template < typename S, typename E >
        decltype( auto ) operator()( S& s, E& e ) const noexcept
        {
                static constexpr bool has_member = requires( S& s, E& e ) {
                        { s.get_completion_signatures( e ) };
                };
                if constexpr ( has_member )
                        return s.get_completion_signatures( e );
                else
                        return typename S::completion_signatures{};
        }
};
/// CPO for getting the completion signatures from a sender. If the sender has a
/// get_completion_signatures member function, it is called with the environment to get the
/// completion signatures. Otherwise, it returns the sender's completion_signatures member type.
///
inline constexpr get_completion_signatures_t get_completion_signatures{};

/// Helper to get the completion signatures of a sender (or similar) with a given environment.
template < typename S, typename Env >
using _sender_completions_t =
    decltype( get_completion_signatures( std::declval< S& >(), std::declval< Env& >() ) );

/// Concept for types that can be used as queryable objects in the environment. A type is queryable
/// if it is an object type (i.e., not a reference, function, or void type) and contains query
/// member functions for appropriate query types.
template < typename T >
concept queryable = std::is_object_v< T >;

/// Concept for senders, requires that the member type sneder_concept derives from sender_t, has a
/// get_env member function that returns a queryable type, and is move constructible and
/// constructible from itself.
template < typename T >
concept sender = std::derived_from< typename std::remove_cvref_t< T >::sender_concept, sender_t > &&
                 requires( T const& s ) {
                         { get_env( s ) } -> queryable;
                 } && std::move_constructible< std::remove_cvref_t< T > > &&
                 std::constructible_from< std::remove_cvref_t< T >, T >;

/// Concept for receivers, requires that the member type receiver_concept derives from receiver_t,
/// has a get_env member function that returns a queryable type, and is move constructible and
/// constructible from itself.
template < typename T >
concept receiver =
    std::derived_from< typename std::remove_cvref_t< T >::receiver_concept, receiver_t > &&
    requires( T const& r ) {
            { get_env( r ) } -> queryable;
    } && std::move_constructible< std::remove_cvref_t< T > > &&
    std::constructible_from< std::remove_cvref_t< T >, T >;


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
template <>
struct _is_signature< _get_stopped_t() > : std::true_type
{
};

/// Type is isgnature if it is one of the valid completion signatures (set_value, set_error,
/// set_stopped, or _get_stopped). This is used to validate the types used in
/// completion_signatures.
template < typename T >
concept signature = _is_signature< T >::value;

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

/// Helper to get the set_value signature for a given type T. If T is void, the signature is
/// set_value_t(), otherwise it is set_value_t( T ).
template < typename T >
using _value_setter_t = typename _value_setter< T >::type;

/// Single row of vtable for a specific completion signature. This contains the function pointer for
/// invoking the corresponding completion signal on derived class, and a constructor that
/// initializes the function pointer based on the derived and base types.
///
/// Any row shall have an invoke function that takes the signature tag, a pointer to the derived
/// object, and the arguments for the completion signal, and invokes the corresponding member
/// function on the derived object.
template < signature S >
struct _sig_vtable_row;

template < typename... Args >
struct _sig_vtable_row< set_value_t( Args... ) >
{

        using f_t = void ( * )( void*, Args... );
        f_t _set_value;

        template < typename D, typename B >
        constexpr _sig_vtable_row( _tag< D >, _tag< B > ) noexcept
          : _set_value{ +[]( void* self, Args... args ) {
                  static_cast< D* >( static_cast< B* >( self ) )->set_value( (Args&&) args... );
          } }
        {
        }

        void invoke( set_value_t, void* self, Args... args ) const
        {
                _set_value( self, (Args&&) args... );
        }
};

template < typename... Args >
struct _sig_vtable_row< set_error_t( Args... ) >
{
        using f_t = void ( * )( void*, Args... );
        f_t _set_error;

        template < typename D, typename B >
        constexpr _sig_vtable_row( _tag< D >, _tag< B > ) noexcept
          : _set_error{ +[]( void* self, Args... args ) {
                  static_cast< D* >( static_cast< B* >( self ) )->set_error( (Args&&) args... );
          } }
        {
        }

        void invoke( set_error_t, void* self, Args... args ) const
        {
                _set_error( self, (Args&&) args... );
        }
};

template <>
struct _sig_vtable_row< set_stopped_t() >
{
        using f_t = void ( * )( void* );
        f_t _set_stopped;

        template < typename D, typename B >
        constexpr _sig_vtable_row( _tag< D >, _tag< B > ) noexcept
          : _set_stopped{ +[]( void* self ) {
                  static_cast< D* >( static_cast< B* >( self ) )->set_stopped();
          } }
        {
        }

        void invoke( set_stopped_t, void* self ) const
        {
                _set_stopped( self );
        }
};

template <>
struct _sig_vtable_row< _get_stopped_t() >
{
        using f_t = bool ( * )( void* );
        f_t _get_stopped;

        template < typename D, typename B >
        constexpr _sig_vtable_row( _tag< D >, _tag< B > ) noexcept
          : _get_stopped{ +[]( void* self ) {
                  return static_cast< D* >( static_cast< B* >( self ) )->get_stopped();
          } }
        {
        }

        bool invoke( _get_stopped_t, void* self ) const
        {
                return _get_stopped( self );
        }
};

/// Vtable containing rows for all completion signatures in the signature list. This is used as a
/// custom implementation of virtual table for the completion signals, allowing for dynamic dispatch
/// of the completion signal handlers based on the actual type of the receiver.
template < signature... S >
struct _sig_vtable : _sig_vtable_row< S >...
{
        template < typename D, typename B >
        constexpr _sig_vtable( _tag< D > dt, _tag< B > bt ) noexcept
          : _sig_vtable_row< S >( dt, bt )...
        {
        }

        using _sig_vtable_row< S >::invoke...;
};

/// Helper to construct a vtable for a given derived type D, with base type B, and signature list
/// VTable.
template < typename D, typename B, typename VTable >
static constexpr VTable _vtable_of = { _tag< D >{}, _tag< B >{} };

/// Concept to check if a vtable can invoke set_value with given arguments.
template < typename VTable, typename... Args >
concept _vtable_can_call_value = requires( VTable v, void* self, Args&&... args ) {
        ( v.invoke( set_value_t{}, self, (Args&&) args... ) );
};

/// Concept to check if a vtable can invoke set_error with given arguments.
template < typename VTable, typename Arg >
concept _vtable_can_call_error = requires( VTable v, void* self, Arg&& arg ) {
        ( v.invoke( set_error_t{}, self, (Arg&&) arg ) );
};

/// Concept to check if a vtable can invoke set_stopped with no arguments.
template < typename VTable >
concept _vtable_can_call_stopped =
    requires( VTable v, void* self ) { ( v.invoke( set_stopped_t{}, self ) ); };

/// Base class that provides the vtable for completion signal dispatch. This mixins a reference to
/// appropiate vtable as a base class of the object.
///
template < signature... S >
struct _vtable_mixin
{
        using _vtable = _sig_vtable< S... >;

        template < typename D >
        _vtable_mixin( _tag< D > ) noexcept
          : vtable( _vtable_of< D, _vtable_mixin< S... >, _vtable > )
        {
                static_assert(
                    std::derived_from< D, _vtable_mixin< S... > >,
                    "Derived type must derive from _vtable_mixin with the same signature list" );
        }

        template < typename... Args >
        void _set_value( Args... args )
        {
                vtable.invoke( set_value_t{}, this, (Args&&) args... );
        }

        template < typename... Args >
        void _set_error( Args... args )
        {
                vtable.invoke( set_error_t{}, this, (Args&&) args... );
        }

        void _set_stopped()
        {
                vtable.invoke( set_stopped_t{}, this );
        }

        bool _get_stopped() noexcept
        {
                return vtable.invoke( _get_stopped_t{}, this );
        }

private:
        _vtable const& vtable;
};

/// Applies the _vtable_mixin to a list of completion signatures, creating a mixin that provides the
/// vtable for those signatures.
template < typename Sigs >
struct _vtable_of_sigs;

template < signature... S >
struct _vtable_of_sigs< completion_signatures< S... > >
{
        using type = _sig_vtable< S... >;
};


/// Helper to filter and map completion signatures based on a given tag (set_value, set_error, or
/// set_stopped). This is used to extract the relevant signatures for a specific completion signal
/// from a list of completion signatures, and to transform them into a different form (e.g., mapping
/// set_value( T ) to T or to set_value( T ) depending on the user's needs).
///
/// The template parameters are:
/// - `TL`: The current list of mapped signatures, accumulated during the recursion. Serves as the
///     output of the metafunction, and is typically initialized as an empty variant.
/// - `Tag`: The tag to filter the signatures by (e.g., set_value_t, set_error_t, or set_stopped_t),
///     ignores any non-matching signatures.
/// - `Sigs`: The list of completion signatures to filter and map.
/// - `Tuple`: A template template parameter that is used to construct the mapped signatures. For
///   example, this could be std::tuple to map set_value( T ) to std::tuple< T >, or
///   _type_identity_t to map set_value( T ) to T
///
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

/// Helper to generate a signature type for a given tag. For example, _sig_generator_t< set_value_t
/// >::type< T, U > will generate the signature type set_value_t( T, U ).
template < typename Tag >
struct _sig_generator_t
{
        template < typename... Args >
        using type = Tag( Args... );
};

template < typename Sigs >
using _filter_values_as_variant_tuple_t =
    typename _filter_map_tag< std::variant<>, set_value_t, Sigs, std::tuple >::type;

template < typename Sigs >
using _filter_erros_as_variant_t =
    typename _filter_map_tag< std::variant<>, set_error_t, Sigs, _type_identity_t >::type;

template < typename Sigs >
using _filter_values_of_t = typename _filter_map_tag<
    completion_signatures<>,
    set_value_t,
    Sigs,
    _sig_generator_t< set_value_t >::type >::type;

template < typename Sigs >
using _filter_errors_of_t = typename _filter_map_tag<
    completion_signatures<>,
    set_error_t,
    Sigs,
    _sig_generator_t< set_error_t >::type >::type;

template < typename Sigs >
using _filter_stopped_of_t = typename _filter_map_tag<
    completion_signatures<>,
    set_stopped_t,
    Sigs,
    _sig_generator_t< set_stopped_t >::type >::type;

template < typename T, typename U >
struct _sigs_contains_type;

template < typename T, typename... Ts >
struct _sigs_contains_type< T, completion_signatures< Ts... > > : _contains_type< T, Ts... >
{
};

/// Concept to check if a list of completion signatures contains a set_stopped signature.
template < typename U >
concept _sigs_contains_set_stopped = _sigs_contains_type< set_stopped_t(), U >::value;

template < typename Sigs1, typename Sigs2 >
struct _sig_is_underset_of_impl;

template < typename... S1, typename... S2 >
struct _sig_is_underset_of_impl< completion_signatures< S1... >, completion_signatures< S2... > >
{
        static constexpr bool value = ( _contains_type< S1, S2... >::value && ... && true );
};

/// Sigs1 is compatible with Sigs2 if every signature in Sigs1 is also present in Sigs2.
template < typename Sigs1, typename Sigs2 >
concept _sig_is_underset_of = _sig_is_underset_of_impl< Sigs1, Sigs2 >::value;


template < typename O, typename... Ts >
struct _sigs_merge_impl;

template < typename... S1, typename S, typename... S2, typename... Ts >
        requires( !_contains_type< S, S1... >::value )
struct _sigs_merge_impl< completion_signatures< S1... >, completion_signatures< S, S2... >, Ts... >
  : _sigs_merge_impl< completion_signatures< S1..., S >, completion_signatures< S2... >, Ts... >
{
};

template < typename... S1, typename S, typename... S2, typename... Ts >
        requires( _contains_type< S, S1... >::value )
struct _sigs_merge_impl< completion_signatures< S1... >, completion_signatures< S, S2... >, Ts... >
  : _sigs_merge_impl< completion_signatures< S1... >, completion_signatures< S2... >, Ts... >
{
};

template < typename... S1, typename... Ts >
struct _sigs_merge_impl< completion_signatures< S1... >, completion_signatures<>, Ts... >
  : _sigs_merge_impl< completion_signatures< S1... >, Ts... >
{
};
template < typename... S1 >
struct _sigs_merge_impl< completion_signatures< S1... > >
{
        using type = completion_signatures< S1... >;
};

/// Helper to merge multiple lists of completion signatures into a single list that contains all
/// unique signatures from the input lists.
template < typename... S >
using _sigs_merge_t = typename _sigs_merge_impl< completion_signatures<>, S... >::type;

/// ------------------------------------------------------------------------------

/// Concept for stoppable tokens, requires that the type has stop_requested and stop_possible member
/// functions that return bool, is copyable, equality comparable, and swappable.
///
/// Stop tokens provide an API for checking if a stop has been requested, and if stopping is
/// possible. They are used in the environment to allow senders and receivers to cooperatively
/// handle cancellation of asynchronous operations.
template < class Token >
concept stoppable_token = requires( Token const tok ) {
        { tok.stop_requested() } noexcept -> std::same_as< bool >;
        { tok.stop_possible() } noexcept -> std::same_as< bool >;
        { Token( tok ) } noexcept;
} && std::copyable< Token > && std::equality_comparable< Token > && std::swappable< Token >;

/// Concept for unstoppable tokens, which are a special case of stoppable tokens that cannot be
/// stopped.
template < class Token >
concept unstoppable_token =
    stoppable_token< Token > && requires { requires( !Token{}.stop_possible() ); };

/// Concept for stoppable sources, requires that the type has get_token, stop_possible,
/// stop_requested, and request_stop member functions with appropriate signatures, and that the
/// token returned by get_token satisfies the stoppable_token concept.
///
/// Stop source provides stop_tokens and the source is used to signal that stop was requested.
template < class Source >
concept stoppable_source = requires( Source& src, Source const csrc ) {
        { csrc.get_token() } -> stoppable_token;
        { csrc.stop_possible() } noexcept -> std::same_as< bool >;
        { csrc.stop_requested() } noexcept -> std::same_as< bool >;
        { src.request_stop() } -> std::same_as< bool >;
};

/// In-place stop token, which is a simple implementation of a stoppable token that is designed to
/// be used in-place without dynamic memory allocation.
struct inplace_stop_token;

/// In-place stop callback, which is a callback that can be registered with an inplace_stop_token to
/// be invoked when a stop is requested. This allows for cooperative cancellation of asynchronous
/// operations without the need for dynamic memory allocation.
template < typename CallbackFn >
struct inplace_stop_callback;

struct _inplace_stop_callback_base : zll::ll_base< _inplace_stop_callback_base >
{
        virtual void _execute() = 0;

        virtual ~_inplace_stop_callback_base() = default;
};

/// In-place stop source, which is a simple implementation of a stoppable source that is designed to
/// be used in-place without dynamic memory allocation. It maintains a list of registered callbacks
/// that are invoked when a stop is requested, allowing for cooperative cancellation of asynchronous
/// operations without the need for dynamic memory allocation.
struct inplace_stop_source
{
        inplace_stop_source() noexcept = default;

        inplace_stop_source( inplace_stop_source const& )            = delete;
        inplace_stop_source& operator=( inplace_stop_source const& ) = delete;

        /// Get the stop token associated with this stop source. The returned token can be used to
        /// check if a stop has been requested, and to register callbacks that will be invoked when
        /// a stop is requested.
        inplace_stop_token get_token() const noexcept;

        /// Check if stopping is possible. For inplace_stop_source, this always returns true since
        /// it can always be stopped.
        bool stop_possible() const noexcept
        {
                return true;
        }

        /// Check if a stop has been requested. This returns true if request_stop() has been called
        /// on this stop source, and false otherwise.
        bool stop_requested() const noexcept
        {
                return _stopped;
        }

        /// Request a stop. This sets the internal state to indicate that a stop has been requested,
        /// and invokes all registered callbacks. Returns true if this call successfully requested a
        /// stop, or false if a stop had already been requested.
        bool request_stop()
        {
                if ( _stopped )
                        return false;
                _stopped = true;

                while ( !_callbacks.empty() ) {
                        auto& cb = _callbacks.front();
                        cb._execute();
                        if ( !_callbacks.empty() && &_callbacks.front() == &cb )
                                zll::detach( cb );
                }

                return true;
        }

private:
        bool _stopped = false;

        // XXX: veve oficially hates that standard requires this to be mutable
        mutable zll::ll_list< _inplace_stop_callback_base > _callbacks;

        template < typename CallbackFn >
        friend struct inplace_stop_callback;
};

struct inplace_stop_token
{
        inplace_stop_token() noexcept = default;

        /// Check if a stop has been requested. This returns true if the associated stop source has
        /// had request_stop() called on it, and false otherwise. If this token is not associated
        /// with a stop source, this returns false.
        [[nodiscard]] bool stop_requested() const noexcept
        {
                return _source && _source->stop_requested();
        }

        /// Check if stopping is possible. This returns true if this token is associated with a stop
        /// source, and false otherwise. If this token is not associated with a stop source, this
        /// returns false since there is no source to request a stop from.
        [[nodiscard]] constexpr bool stop_possible() const noexcept
        {
                return _source != nullptr;
        }

        /// Equality comparison operators, two inplace_stop_tokens are equal if they are associated
        /// with the same stop source (or both are not associated with any stop source).
        friend bool
        operator==( inplace_stop_token const& lhs, inplace_stop_token const& rhs ) noexcept
        {
                return lhs._source == rhs._source;
        }

        /// Inequality comparison operator, defined in terms of operator==.
        friend bool
        operator!=( inplace_stop_token const& lhs, inplace_stop_token const& rhs ) noexcept
        {
                return lhs._source != rhs._source;
        }

        template < typename CallbackFn >
        using callback_type = inplace_stop_callback< CallbackFn >;

private:
        inplace_stop_source const* _source = nullptr;

        inplace_stop_token( inplace_stop_source const* source ) noexcept
          : _source( source )
        {
        }

        friend struct inplace_stop_source;

        template < typename CallbackFn >
        friend struct inplace_stop_callback;
};

inline inplace_stop_token inplace_stop_source::get_token() const noexcept
{
        return inplace_stop_token{ this };
}

/// In-place stop callback, which is a callback that can be registered with an inplace_stop_token to
/// be invoked when a stop is requested. This allows for cooperative cancellation of asynchronous
/// operations without the need for dynamic memory allocation. The callback is constructed with a
/// stop token and an initializer for the callback function. If the stop token is already in a
/// stopped state when the callback is constructed, the callback function is invoked immediately.
///
/// The callback is automatically unlinked from the stop source when it is destroyed, ensuring that
/// it will not be invoked after it has been destroyed. The callback function is stored in-place
/// within the callback object, allowing for efficient storage without dynamic memory allocation.
///
template < typename CallbackFn >
struct inplace_stop_callback : _inplace_stop_callback_base
{
        template < typename Initializer >
        explicit inplace_stop_callback( inplace_stop_token st, Initializer&& init ) noexcept(
            std::is_nothrow_constructible_v< CallbackFn, Initializer > &&
            std::is_nothrow_invocable_v< CallbackFn > )
          : _callback_fn( (Initializer&&) init )
          , _source( st._source )
        {
                if ( _source && _source->stop_possible() ) {
                        if ( _source->stop_requested() )
                                ( (CallbackFn&&) _callback_fn )();
                        else
                                _source->_callbacks.link_back( *this );
                }
        }

        inplace_stop_callback( inplace_stop_callback&& )                 = delete;
        inplace_stop_callback( inplace_stop_callback const& )            = delete;
        inplace_stop_callback& operator=( inplace_stop_callback&& )      = delete;
        inplace_stop_callback& operator=( inplace_stop_callback const& ) = delete;

        void _execute() override
        {
                ( (CallbackFn&&) _callback_fn )();
        }

private:
        CallbackFn                 _callback_fn;
        inplace_stop_source const* _source;
};

template < typename CallbackFn >
inplace_stop_callback( inplace_stop_token, CallbackFn ) -> inplace_stop_callback< CallbackFn >;

/// Never stop token, which is a simple implementation of an unstoppable token that can be used as a
/// default stop token in environments where stopping is not supported or needed. This token always
/// reports that stopping is not possible and that a stop has not been requested, and its callback
/// type is a no-op callback that does nothing when invoked.
struct never_stop_token
{
        struct cb_type
        {
                explicit cb_type( never_stop_token, auto&& ) noexcept
                {
                }
        };

public:
        template < class >
        using callback_type = cb_type;

        static constexpr bool stop_requested() noexcept
        {
                return false;
        }
        static constexpr bool stop_possible() noexcept
        {
                return false;
        }

        bool operator==( never_stop_token const& ) const = default;
};

struct get_stop_token_t
{
        template < typename T >
        decltype( auto ) operator()( T const& env ) const noexcept
        {
                static constexpr bool v = requires( get_stop_token_t t ) {
                        { env.query( t ) };
                };  // namespace ecor
                if constexpr ( v )
                        return env.query( *this );
                else
                        return never_stop_token{};
        }
};
/// CPO for getting the stop token from the environment. If the environment does not have a stop
/// token, returns a never_stop_token that cannot be stopped.
inline constexpr get_stop_token_t get_stop_token{};

/// ------------------------------------------------------------------------------

/// Base class for linked list entries which can be invoked with the completion signals.
template < signature... S >
struct _ll_entry : _vtable_mixin< S... >, zll::ll_base< _ll_entry< S... > >
{
        template < typename D >
        _ll_entry( _tag< D > ) noexcept
          : _vtable_mixin< S... >( _tag< D >{} )
        {
        }
};

/// Operation state for list-based senders. This is the type of the object that is linked into the
/// list of the scheduler, and contains the receiver and the vtable for dispatching the completion
/// signals to the receiver.
template < typename R, signature... S >
struct _ll_op : _ll_entry< S... >, R
{
        using operation_state_concept = operation_state_t;

        _ll_op( auto& list, R receiver ) noexcept( std::is_nothrow_move_constructible_v< R > )
          : _ll_entry< S... >( _tag< _ll_op >{} )
          , R( std::move( receiver ) )
          , _list( list )
        {
        }

        void start() noexcept
        {
                _list.link_back( *this );
        }

private:
        zll::ll_list< _ll_entry< S... > >& _list;
};

/// Base class for skew heap entries which can be invoked with the completion signals.
template < typename K, signature... S >
struct _sh_entry : _vtable_mixin< S... >, zll::sh_base< _sh_entry< K, S... > >
{
        K key;

        template < typename D >
        _sh_entry( K k, _tag< D > ) noexcept( std::is_nothrow_move_constructible_v< K > )
          : _vtable_mixin< S... >{ _tag< D >{} }
          , key( std::move( k ) )
        {
        }

        constexpr bool operator<( _sh_entry const& o ) const
            noexcept( noexcept( std::declval< K const& >() < std::declval< K const& >() ) )
        {
                return key < o.key;
        }
};

/// Operation state for skew heap-based senders. This is the type of the object that is linked into
/// the skew heap of the scheduler, and contains the receiver, the key for ordering in the heap, and
/// the vtable for dispatching the completion signals to the receiver.
template < typename R, typename K, signature... S >
struct _sh_op : _sh_entry< K, S... >, R
{
        using operation_state_concept = operation_state_t;

        _sh_op( auto& heap, K key, R receiver ) noexcept(
            std::is_nothrow_move_constructible_v< R > && std::is_nothrow_move_constructible_v< K > )
          : _sh_entry< K, S... >( key, _tag< _sh_op >{} )
          , R( std::move( receiver ) )
          , _heap( heap )
        {
        }

        void start() noexcept
        {
                _heap.link( *this );
        }

private:
        zll::sh_heap< _sh_entry< K, S... > >& _heap;
};

/// Sender type for linked-list-based schedulers. This is the type returned by the schedule()
/// function of a linked-list-based scheduler, and contains a reference to the list of the
/// scheduler.
template < signature... S >
struct _ll_sender
{
        using sender_concept = sender_t;

        _ll_sender( zll::ll_list< _ll_entry< S... > >& ll ) noexcept
          : _ll( ll )
        {
        }

        using completion_signatures = ecor::completion_signatures< S... >;

        [[nodiscard]] empty_env get_env() const noexcept
        {
                return {};
        }

        template < receiver R >
        _ll_op< R, S... > connect( R receiver ) noexcept(
            noexcept( _ll_op< R, S... >{ _ll, std::move( receiver ) } ) )
        {
                return { _ll, std::move( receiver ) };
        }

private:
        zll::ll_list< _ll_entry< S... > >& _ll;
};

/// Sender type for skew-heap-based schedulers. This is the type returned by the schedule() function
/// of a skew-heap-based scheduler, and contains a reference to the heap of the scheduler and the
/// key for ordering in the heap.
template < typename K, signature... S >
struct _sh_sender
{
        using sender_concept = sender_t;

        _sh_sender( K key, zll::sh_heap< _sh_entry< K, S... > >& sh ) noexcept(
            std::is_nothrow_move_constructible_v< K > )
          : _key( key )
          , _sh( sh )
        {
        }

        using completion_signatures = ecor::completion_signatures< S... >;

        [[nodiscard]] empty_env get_env() const noexcept
        {
                return {};
        }

        template < receiver R >
        _sh_op< R, K, S... > connect( R receiver ) && noexcept(
            noexcept( _sh_op< R, K, S... >{ _sh, std::move( _key ), std::move( receiver ) } ) )
        {
                return { _sh, std::move( _key ), std::move( receiver ) };
        }

        template < receiver R >
        _sh_op< R, K, S... > connect( R receiver ) const& noexcept(
            noexcept( _sh_op< R, K, S... >{ _sh, _key, std::move( receiver ) } ) )
        {
                return { _sh, _key, std::move( receiver ) };
        }

private:
        K                                     _key;
        zll::sh_heap< _sh_entry< K, S... > >& _sh;
};


/// Broadcast source that implements a scheduler that allows multiple receivers to be scheduled with
/// the same completion signatures, and when a completion signal is sent, it is broadcast to all
/// scheduled receivers. These are unregistered before they are completed, so they will only receive
/// the first completion signal sent after they are scheduled.
///
template < signature... S >
struct broadcast_source
{
        using sender_type = _ll_sender< S... >;

        /// Schedule a new receiver with this source.
        _ll_sender< S... > schedule() noexcept
        {
                return ( _list );
        }

        /// Send a set_value signal to all scheduled receivers. These are unregistered before the
        /// signal is sent, so they will only receive the first set_value signal sent after they are
        /// scheduled.
        ///
        /// Passes the arguments as non-const reference.
        ///
        template < typename... Args >
        void set_value( Args&&... args )
        {
                static_assert(
                    _vtable_can_call_value< _sig_vtable< S... >, Args... >,
                    "Completion signatures do not contain set_value_t(Args...)" );
                for_each( [&]( auto& n ) {
                        n._set_value( args... );
                } );
        }

        /// Send a set_error signal to all scheduled receivers. These are unregistered before the
        /// signal is sent, so they will only receive the first set_error signal sent after they are
        /// scheduled.
        ///
        /// Passes the error as non-const reference.
        ///
        template < typename E >
        void set_error( E&& err )
        {
                static_assert(
                    _vtable_can_call_error< _sig_vtable< S... >, E >,
                    "Completion signatures do not contain set_error_t(E)" );
                for_each( [&]( auto& n ) {
                        n._set_error( err );
                } );
        }

        /// Send a set_stopped signal to all scheduled receivers. These are unregistered before the
        /// signal is sent, so they will only receive the first set_stopped signal sent after they
        /// are scheduled.
        ///
        void set_stopped()
        {
                static_assert(
                    _vtable_can_call_stopped< _sig_vtable< S... > >,
                    "Completion signatures do not contain set_stopped_t()" );
                for_each( [&]( auto& n ) {
                        n._set_stopped();
                } );
        }

private:
        void for_each( auto&& f )
        {
                auto l = std::move( _list );
                if ( l.empty() )
                        return;

                for_each_node( l.front(), f );
                auto& n   = l.front();
                using Acc = typename _ll_entry< S... >::access;
                for ( _ll_entry< S... >* m = &n; m; m = _node( Acc::get( *m ).next ) )
                        f( *m );
        }

        zll::ll_list< _ll_entry< S... > > _list;
};


/// FIFO source that implements a scheduler that allows multiple receivers to be scheduled with the
/// same completion signatures, and processes them in FIFO order.
///
/// Any completion signal sent to the source is delivered to the front receiver in the queue, and
/// that receiver is unregistered before the signal is sent, so it will only receive the first
/// completion signal sent after it is scheduled.
///
template < signature... S >
struct fifo_source
{
        using completion_sigs = completion_signatures< S... >;
        using sender_type     = _ll_sender< S... >;

        /// Schedule a new sender with this scheduler.
        _ll_sender< S... > schedule() noexcept
        {
                return ( _ll );
        }

        /// Send a set_value signal to the front scheduled receiver. The receiver is unregistered
        /// before the signal is sent, so it will only receive the first set_value signal sent after
        /// it is scheduled.
        ///
        /// Arguments are perfectly forwarded.
        ///
        template < typename... V >
        void set_value( V&&... value )
        {
                static_assert(
                    _vtable_can_call_value< _sig_vtable< S... >, V... >,
                    "Completion signatures do not contain set_value_t(Args...)" );
                do_f( [&]( auto& n ) {
                        n._set_value( (V&&) value... );
                } );
        }

        /// Send a set_error signal to the front scheduled receiver. The receiver is unregistered
        /// before the signal is sent, so it will only receive the first set_error signal sent after
        /// it is scheduled.
        ///
        /// The error is perfectly forwarded.
        ///
        template < typename E1 >
        void set_error( E1&& err )
        {
                static_assert(
                    _vtable_can_call_error< _sig_vtable< S... >, E1 >,
                    "Completion signatures do not contain set_error_t(E1)" );
                do_f( [&]( auto& n ) {
                        n._set_error( (E1&&) err );
                } );
        }

        /// Send a set_stopped signal to the front scheduled receiver. The receiver is unregistered
        /// before the signal is sent, so it will only receive the first set_stopped signal sent
        /// after it is scheduled.
        ///
        void set_stopped()
        {
                static_assert(
                    _vtable_can_call_stopped< _sig_vtable< S... > >,
                    "Completion signatures do not contain set_stopped_t()" );
                do_f( [&]( auto& n ) {
                        n._set_stopped();
                } );
        }

private:
        void do_f( auto f )
        {
                if ( _ll.empty() )
                        return;

                auto& n = _ll.take_front();
                f( n );
        }


        zll::ll_list< _ll_entry< S... > > _ll;
};

/// Keyed source that implements a scheduler that allows multiple receivers to be scheduled with the
/// same completion signatures, and processes them in order based on a key provided at scheduling
/// time. The key is used to order the scheduled receivers in a heap, and completion signals are
/// delivered to the receiver with the smallest key.
///
template < typename K, signature... S >
struct seq_source
{
        using sender_type = _sh_sender< K, S... >;

        /// Schedule a new sender with this scheduler, using the provided key for ordering. The
        /// sender is returned with the key and a reference to the heap of the scheduler, and when
        /// connected and started, it will be linked into the heap based on the key.
        _sh_sender< K, S... >
        schedule( K key ) noexcept( noexcept( _sh_sender< K, S... >{ key, _sh } ) )
        {
                return { key, _sh };
        }

        /// Send a set_value signal to the scheduled receiver with the smallest key. The receiver is
        /// unregistered before the signal is sent, so it will only receive the first set_value
        /// signal sent after it is scheduled.
        ///
        /// Arguments are perfectly forwarded.
        ///
        template < typename... V >
        void set_value( V&&... value )
        {
                static_assert(
                    _vtable_can_call_value< _sig_vtable< S... >, V... >,
                    "Completion signatures do not contain set_value_t(V)" );
                do_f( [&]( auto& n ) {
                        _sn._set_value( (V&&) value... );
                } );
        }

        /// Send a set_error signal to the scheduled receiver with the smallest key. The receiver is
        /// unregistered before the signal is sent, so it will only receive the first set_error
        /// signal sent after it is scheduled.
        ///
        /// The error is perfectly forwarded.
        ///
        template < typename E1 >
        void set_error( E1&& err )
        {
                static_assert(
                    _vtable_can_call_error< _sig_vtable< S... >, E1 >,
                    "Completion signatures do not contain set_error_t(E1)" );
                // XXX: should this treat all entries or just the front one?
                do_f( [&]( auto& n ) {
                        _sn._set_error( (E1&&) err );
                } );
        }

        /// Send a set_stopped signal to the scheduled receiver with the smallest key. The receiver
        /// is unregistered before the signal is sent, so it will only receive the first set_stopped
        /// signal sent after it is scheduled.
        ///
        void set_stopped()
        {
                static_assert(
                    _vtable_can_call_stopped< _sig_vtable< S... > >,
                    "Completion signatures do not contain set_stopped_t()" );
                // XXX: should this treat all entries or just the front one?
                do_f( [&]( auto& n ) {
                        _sn._set_stopped();
                } );
        }

        /// Check if there are any scheduled receivers. This returns true if there are no scheduled
        /// receivers, and false otherwise.
        ///
        [[nodiscard]] bool empty() const
        {
                return _sh.empty();
        }

        /// Get a reference to the scheduled receiver with the smallest key. This does not modify
        /// the heap, so the receiver remains scheduled and will receive the next completion signal
        /// sent to the source. The behavior is undefined if there are no scheduled receivers.
        ///
        [[nodiscard]] _sh_entry< K, S... > const& front() const
        {
                return *_sh.top;
        }

private:
        void do_f( auto f )
        {
                if ( _sh.empty() )
                        return;

                auto& n = _sh.take();
                f( n );
        }

        zll::sh_heap< _sh_entry< K, S... > > _sh;
};


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
        auto connect( R receiver ) && noexcept(
            std::is_nothrow_move_constructible_v< T > && std::is_nothrow_move_constructible_v< R > )
        {
                return _op{ std::move( err ), std::move( receiver ) };
        }

        using completion_signatures = ecor::completion_signatures< set_error_t( T ) >;

        [[nodiscard]] empty_env get_env() const noexcept
        {
                return {};
        }
};

/// Returns a sender that completes with the given error. The sender returned by this function has a
/// single completion signature of set_error_t( T ), where T is the type of the error passed to the
/// function. When connected to a receiver and started, the sender will immediately complete by
/// calling set_error on the receiver with the error passed to this function.
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

struct task_core
{
        bool run_once()
        {
                if ( _ready_tasks.empty() )
                        return false;
                auto& t = _ready_tasks.front();
                _ready_tasks.detach_front();
                t.resume();
                return true;
        }

        void run_n( std::size_t n )
        {
                for ( std::size_t i = 0; i < n; ++i )
                        if ( !run_once() )
                                break;
        }

        void reschedule( _itask_op& op )
        {
                _ready_tasks.link_back( op );
        }

private:
        zll::ll_list< _itask_op > _ready_tasks;
};

template < typename T >
struct _expected
{
        _expected() noexcept {};

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

template < typename T >
struct _awaitable_extract_type;

template < typename... U >
struct _awaitable_extract_type< std::variant< std::tuple< U... > > >
{
        using type = std::tuple< U... >;
};

template < typename U >
struct _awaitable_extract_type< std::variant< std::tuple< U > > >
{
        using type = U;
};

template <>
struct _awaitable_extract_type< std::variant< std::tuple<> > >
{
        using type = void;
};

template <>
struct _awaitable_extract_type< std::variant<> >
{
        using type = void;
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
                else {
                        switch ( _exp.state ) {
                        case _awaitable_state_e::empty:
                                ECOR_ASSERT( false );
                        case _awaitable_state_e::value:
                                break;
                        }
                        return std::move( _exp.val );
                }
        }

        using _env         = decltype( std::declval< PromiseType >().get_env() );
        using _completions = _sender_completions_t< std::remove_cvref_t< S >, _env >;
        using _values      = _filter_values_as_variant_tuple_t< _completions >;
        using _errors      = _filter_erros_as_variant_t< _completions >;
        static_assert(
            std::variant_size_v< _values > <= 1,
            "Multiple set_value completions not supported" );

        using value_type = typename _awaitable_extract_type< _values >::type;

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

                [[nodiscard]] auto
                get_env() const noexcept -> decltype( std::declval< PromiseType >().get_env() )
                {
                        return _cont.promise().get_env();
                }
        };

        using _op_t = connect_type< S, _receiver >;

        _aw_tp _exp;
        _op_t  _op;
};

template < class Alloc >
concept simple_allocator = requires( Alloc alloc, size_t n ) {
        { *alloc.allocate( n ) } -> std::same_as< typename Alloc::value_type& >;
        { alloc.deallocate( alloc.allocate( n ), n ) };
} && std::copy_constructible< Alloc > && std::equality_comparable< Alloc >;


struct task_memory_resource
{
        template < typename M >
        task_memory_resource( M& m )
          : mem( &m )
        {
                alloc = +[]( void* mem, std::size_t const sz, std::size_t const align ) {
                        return ecor::allocate( *( (M*) mem ), sz, align );
                };
                dealloc = +[]( void* mem, void* p, std::size_t const sz, std::size_t const align ) {
                        ecor::deallocate( *( (M*) mem ), p, sz, align );
                };
        }

        [[nodiscard]] void* allocate( std::size_t const sz, std::size_t const align ) const noexcept
        {
                return alloc( mem, sz, align );
        }

        void deallocate( void* p, std::size_t const sz, std::size_t const align ) const noexcept
        {
                dealloc( mem, p, sz, align );
        }

        void* ( *alloc )( void*, std::size_t const, std::size_t const )         = nullptr;
        void ( *dealloc )( void*, void*, std::size_t const, std::size_t const ) = nullptr;
        void* mem                                                               = nullptr;
};

struct get_task_core_t
{
        template < typename T >
        decltype( auto ) operator()( T& t ) const noexcept
        {
                return t.query( get_task_core_t{} );
        }
};
inline constexpr get_task_core_t get_task_core{};

struct get_memory_resource_t
{
        template < typename T >
        decltype( auto ) operator()( T& t ) const noexcept
        {
                return t.query( get_memory_resource_t{} );
        }
};
inline constexpr get_memory_resource_t get_memory_resource{};

struct task_ctx
{
        task_memory_resource alloc;
        task_core            core;

        task_ctx( auto& mem )
          : alloc( mem )
        {
        }

        auto& query( get_task_core_t ) noexcept
        {
                return core;
        }

        auto& query( get_memory_resource_t ) noexcept
        {
                return alloc;
        }
};

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
                        r->invoke( set_error_t{}, _obj, (U&&) err );
                }
        }

        template < typename... U >
        void set_value( U&&... v ) noexcept
        {
                ECOR_ASSERT( this->_recv );
                if ( this->_recv ) {
                        auto* r = std::exchange( this->_recv, nullptr );
                        r->invoke( set_value_t{}, this->_obj, (U&&) v... );
                }
        }

        void set_stopped() noexcept
        {
                ECOR_ASSERT( this->_recv );
                if ( this->_recv ) {
                        auto* r = std::exchange( this->_recv, nullptr );
                        r->invoke( set_stopped_t{}, this->_obj );
                }
        }

        void on_final_suspend() noexcept
        {
                if ( _recv ) {
                        auto* r = std::exchange( this->_recv, nullptr );
                        r->invoke( set_error_t{}, _obj, task_error::task_unfinished );
                }
        }

        template < typename U >
        void setup_continuable( U& p )
        {
                this->_recv = &_vtable_of< U, U, vtable >;
                this->_obj  = static_cast< void* >( &p );
        }

protected:
        using sigs          = typename Task::completion_signatures;
        using vtable        = typename _vtable_of_sigs< sigs >::type;
        vtable const* _recv = nullptr;
        void*         _obj  = nullptr;
};

struct _promise_base : _itask_op
{
        static constexpr std::size_t align = alignof( std::max_align_t );
        struct vtable
        {
                task_memory_resource* mem = nullptr;
        };
        static constexpr std::size_t spacing = align > sizeof( vtable ) ? align : sizeof( vtable );

        // XXX: check the noexcept thing

        static void* alloc( std::size_t sz, task_memory_resource& mem ) noexcept
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
                task_memory_resource& a = get_memory_resource( ctx );
                return alloc( sz, a );
        }

        void operator delete( void* const ptr, std::size_t const sz ) noexcept
        {
                dealloc( ptr, sz );
        }

        _promise_base( auto&& ctx, auto&&... )
          : core( get_task_core( ctx ) )
          , token()
        {
        }

        void unhandled_exception()
        {
        }

        std::suspend_always initial_suspend() noexcept
        {
                return {};
        }

        task_core&         core;
        inplace_stop_token token;

        struct _env
        {
                inplace_stop_token& token;

                [[nodiscard]]
                auto query( get_stop_token_t ) const noexcept
                {
                        return token;
                }
        };

        _env get_env() noexcept
        {
                return { token };
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

template < typename E >
struct with_error
{
        using type = std::remove_cvref_t< E >;
        E error;
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

        template < typename E >
                requires( _sigs_contains_type<
                          set_error_t( E ),
                          typename Task::_error_completions >::value )
        auto yield_value( with_error< E > error )
        {
                return await_transform( just_error( std::move( error.error ) ) );
        }


        template < typename T >
        decltype( auto ) await_transform( T&& x ) noexcept
        {
                using U = std::remove_cvref_t< T >;
                if constexpr ( sender< U > ) {
                        // XXX: leaky implementation detail
                        using compls = _sender_completions_t< U, typename _promise_base::_env >;
                        using errs   = _filter_errors_of_t< compls >;
                        static_assert(
                            _sig_is_underset_of< errs, typename Task::_error_completions >,
                            "Error signatures are not compatible" );
                        using vals = _filter_values_as_variant_tuple_t< compls >;
                        static_assert(
                            std::variant_size_v< vals > <= 1,
                            "Sender used in co_await must only complete with a single set_value signature" );
                        return _awaitable< _promise_type, T >{
                            (T&&) x,
                            std::coroutine_handle< _promise_type >::from_promise( *this ) };
                } else {
                        return (T&&) x;
                }
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

        _task_op() noexcept = default;
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
        _task_op& operator=( _task_op&& other ) noexcept
        {
                _task_op cpy{ std::move( other ) };
                std::swap( _h, cpy._h );
                std::swap( _recv, cpy._recv );
                std::swap( _error, cpy._error );
                return *this;
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
                if constexpr ( !std::same_as<
                                   decltype( get_stop_token( _recv.get_env() ) ),
                                   never_stop_token > )
                        _h.promise().token = get_stop_token( _recv.get_env() );
                _h.promise()._g.setup_continuable( _recv );
                _h.promise().core.reschedule( _h.promise() );
        }

        void clear()
        {
                if ( _h )
                        _h.destroy();
                _h     = {};
                _recv  = {};
                _error = task_error::none;
        }

        operator bool()
        {
                return this->_h && !this->_h.done();
        }

        ~_task_op()
        {
                if ( this->_h && !this->_h.done() )
                        this->_h.destroy();
        }

        std::coroutine_handle< _promise_type< task< T, CFG > > > _h;
        R                                                        _recv;
        task_error                                               _error = task_error::none;
};

template < typename T, task_config CFG >
struct task
{
        using sender_concept = sender_t;
        using value_type     = T;
        using promise_type   = _promise_type< task >;

        // XXX: append?
        using _error_completions = _sigs_merge_t<
            ecor::completion_signatures< set_error_t( task_error ) >,
            typename CFG::extra_error_signatures >;

        // XXX: convert to simple concat instead of merge
        using completion_signatures = _sigs_merge_t<
            ecor::completion_signatures< _value_setter_t< T >, set_stopped_t() >,
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


        [[nodiscard]] empty_env get_env() const noexcept
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

                decltype( auto ) get_env() noexcept
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
        using _completions =
            _sigs_merge_t< _sender_completions_t< S1, Env >, _sender_completions_t< S2, Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                return {};
        }

        [[nodiscard]] empty_env get_env() const noexcept
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
        using _completions = _sigs_merge_t<
            completion_signatures< set_value_t( _values< Env > ) >,
            _error< Env >,
            _stopped< Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                return {};
        }

        [[nodiscard]] empty_env get_env() const noexcept
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

[[maybe_unused]] static inline struct as_variant_t
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
            _sig_generator_t< set_value_t >::type >::type;

        template < typename Env >
        using _completions =
            _sigs_merge_t< _s_values< Env >, _s_errors_as_val< Env >, _stopped< Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& )
        {
                return {};
        }

        [[nodiscard]] empty_env get_env() const noexcept
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

[[maybe_unused]] static inline struct err_to_val_t
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
        using _completions = _sigs_merge_t<
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

        [[nodiscard]] empty_env get_env() const noexcept
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

[[maybe_unused]] static inline struct sink_err_t
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

        using sender_concept = sender_t;

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
template < typename S >
struct repeater
{
        static_assert( sender< S >, "S must be a sender" );
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


                [[nodiscard]] empty_env get_env() const noexcept
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

struct _await_until_stopped
{
        using sender_concept = sender_t;


        using completion_signatures = ecor::completion_signatures< set_value_t() >;


        [[nodiscard]] empty_env get_env() const noexcept
        {
                return {};
        }

        template < typename R >
        struct _op
        {
                using operation_state_concept = operation_state_t;

                _op( R r )
                  : _r( std::move( r ) )
                {
                }

                void start()
                {
                        auto st = get_stop_token( ecor::get_env( _r ) );
                        static_assert(
                            !std::same_as< decltype( st ), never_stop_token >,
                            "Receiver must be stoppable" );
                        _cb.emplace( st, cb_t{ .op = this } );
                }

                R _r;

                struct cb_t
                {
                        void operator()() noexcept
                        {
                                op->_r.set_value();
                        }
                        _op* op;
                };

                std::optional< inplace_stop_callback< cb_t > > _cb;
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                return _op{ std::move( receiver ) };
        }
};

[[maybe_unused]]
static inline _await_until_stopped wait_until_stopped;

template < typename T >
struct _transaction_vtable_mixin;

template < signature... S >
        requires( !_contains_type< set_stopped_t(), S... >::value )
struct _transaction_vtable_mixin< completion_signatures< S... > >
{
        using type = _vtable_mixin< S... >;
};

template < signature... S >
        requires( _contains_type< set_stopped_t(), S... >::value )
struct _transaction_vtable_mixin< completion_signatures< S... > >
{
        using type = _vtable_mixin< S..., _get_stopped_t() >;
};

template < typename T >
using _transaction_vtable_mixin_t =
    typename _transaction_vtable_mixin< _sender_completions_t< T, empty_env > >::type;

template < typename T >
struct transaction_entry : _transaction_vtable_mixin_t< T >, zll::ll_base< transaction_entry< T > >
{
        using base    = _transaction_vtable_mixin_t< T >;
        using _vtable = typename base::_vtable;
        static constexpr bool _has_stop_sig =
            _sigs_contains_set_stopped< _sender_completions_t< T, empty_env > >;

        T data;

        template < typename D >
        transaction_entry( _tag< D >, T data )
          : base( _tag< D >{} )
          , data( std::move( data ) )
        {
        }
};

template < typename T, typename R >
struct transaction_op : transaction_entry< T >, R
{
        using _vtable                       = typename transaction_entry< T >::_vtable;
        static constexpr bool _has_stop_sig = transaction_entry< T >::_has_stop_sig;

        transaction_op( T data, R r, zll::ll_list< transaction_entry< T > >& pending )
          : transaction_entry< T >{ _tag< transaction_op >{}, std::move( data ) }
          , R( std::move( r ) )
          , _pending( pending )
        {
        }

        void start()
        {
                _pending.link_front( *this );
        }

        [[nodiscard]] bool get_stopped() const noexcept
        {
                if constexpr ( _has_stop_sig ) {
                        bool res = get_stop_token( get_env( (R&) *this ) ).stop_requested();
                        return res;
                } else
                        return false;
        }

private:
        zll::ll_list< transaction_entry< T > >& _pending;
};


template < typename T >
struct transaction_sender
{
        using sender_concept = sender_t;

        template < typename Env >
        constexpr auto get_completion_signatures( Env&& e ) noexcept
        {
                return val.get_completion_signatures( (Env&&) e );
        }

        [[nodiscard]] empty_env get_env() const noexcept
        {
                return {};
        }

        template < receiver R >
        auto connect( R receiver ) &&
        {
                return transaction_op< T, R >{ std::move( val ), std::move( receiver ), _pending };
        }

        T                                       val;
        zll::ll_list< transaction_entry< T > >& _pending;
};

template < typename T, size_t N >
struct transaction_circular_buffer
{
        static_assert( ( N & ( N - 1 ) ) == 0, "Size of circular buffer must be a power of 2" );
        static_assert(
            N < std::numeric_limits< uint16_t >::max(),
            "Size of circular buffer must be less than 65536" );
        static_assert( std::is_trivial_v< T > );

        T& operator[]( uint16_t x )
        {
                return _data[x % N];
        }

        bool full()
        {
                return ( last - first ) == N;
        }

        bool empty()
        {
                return first == last;
        }

        std::atomic< uint16_t > first = 0;
        std::atomic< uint16_t > mid1  = 0;
        std::atomic< uint16_t > mid2  = 0;
        std::atomic< uint16_t > last  = 0;

private:
        T _data[N];
};

/// XXX: example usage: request reply over UART - we can wait a while for reply
/// XXX: write explicit tests about proper semantics of set stopped if used: active transaction
/// shall not be stoppable

/// Expected behavior:
///  - Used for request-reply protocols, that allow multiple in-flight transactions, but the order
///  of replies is guaranteed.
///  - Transaction is user-defined type T
///  - User can specify signatures S... that the transaction can complete with, and provide handlers
///  for those signatures when scheduling a transaction.
///  - When transaction is started, it is added to the list of pending transactions. When a reply is
///  received, user can call tick() with the reply, and transaction source will find the matching
///  transaction and complete it with the appropriate signature.
template < typename T >
struct transaction_source
{
        using sender_type                   = transaction_sender< T >;
        static constexpr bool _has_stop_sig = transaction_entry< T >::_has_stop_sig;

        sender_type schedule( T val )
        {
                return { std::move( val ), _pending_tx };
        }

        transaction_entry< T >* query_next_transaction()
        {
                // XXX: test the stop behavior
                if constexpr ( _has_stop_sig ) {
                        _pending_tx.remove_if( [this]( transaction_entry< T >& tx ) {
                                if ( !tx._get_stopped() )
                                        return false;

                                tx._set_stopped();
                                return true;
                        } );
                }
                if ( _pending_tx.empty() )
                        return nullptr;
                return &_pending_tx.take_back();
        }

private:
        zll::ll_list< transaction_entry< T > > _pending_tx;
};

}  // namespace ecor
