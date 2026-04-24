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

#pragma once

#include <atomic>
#include <bit>
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
#define ECOR_ASSERT( expr ) ( (void) sizeof( expr ) )
#endif

#endif

/// Portable wrapper for the [[no_unique_address]] attribute. MSVC ignores the standard form
/// and instead recognizes the vendor-specific [[msvc::no_unique_address]] (since VS 2019 16.9
/// with /Zc:__cplusplus or /std:c++20). All other major compilers honor the standard form.
#ifndef ECOR_NO_UNIQUE_ADDRESS
#if defined( _MSC_VER ) && !defined( __clang__ )
#define ECOR_NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#else
#define ECOR_NO_UNIQUE_ADDRESS [[no_unique_address]]
#endif
#endif

/// Portable force-inline hint. Expands to a vendor-specific attribute on Clang, GCC, and MSVC;
/// expands to nothing on other compilers (the function is still implicitly inline because it is
/// defined in a class body or marked `inline` by the user).
#ifndef ECOR_FORCE_INLINE
#if defined( __clang__ ) || defined( __GNUC__ )
#define ECOR_FORCE_INLINE __attribute__( ( always_inline ) ) inline
#elif defined( _MSC_VER )
#define ECOR_FORCE_INLINE __forceinline
#else
#define ECOR_FORCE_INLINE inline
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

/// Helper to apply a template C to the signatures in a completion_signatures type.
template < template < typename... > class C, typename T >
struct _apply_to_sigs;

template < template < typename... > class C, typename... Ts >
struct _apply_to_sigs< C, completion_signatures< Ts... > >
{
        using type = C< Ts... >;
};

template < template < typename... > class C, typename T >
using _apply_to_sigs_t = typename _apply_to_sigs< C, T >::type;

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


/// Thin wrapper over error value, used to tag value as an error for ecor library.
template < typename E >
struct with_error
{
        using type = std::remove_cvref_t< E >;
        E error;

        explicit with_error( E e )
          : error( std::move( e ) )
        {
        }
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

template < typename S >
struct _multivalue_value_signature : std::false_type
{
};

template < typename T, typename U, typename... Ts >
struct _multivalue_value_signature< set_value_t( T, U, Ts... ) > : std::true_type
{
};

template < typename S >
struct _no_value_signature : std::false_type
{
};

template <>
struct _no_value_signature< set_value_t() > : std::true_type
{
};

/// Helper concept to check if all set_value signatures in a sender's completion signatures are
/// singular, i.e. they have at exactly one argument.
template < typename... S >
concept all_value_signatures_singular =
    ( ... && ( !_multivalue_value_signature< S >::value && !_no_value_signature< S >::value ) );

template < typename T >
struct _is_all_singular;

template < typename... S >
struct _is_all_singular< completion_signatures< S... > >
{
        static constexpr bool value = all_value_signatures_singular< S... >;
};

/// Helper variable template for checking if all set_value signatures in completion signatures are
/// singular.
template < typename Sigs >
static constexpr bool is_all_singular_v = _is_all_singular< Sigs >::value;


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
        ECOR_NO_UNIQUE_ADDRESS Deleter _deleter;
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

struct _get_env_t
{
        template < typename T >
        decltype( auto ) operator()( T const& o ) const noexcept
        {
                static constexpr bool v = requires( T const& o ) {
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
inline constexpr _get_env_t get_env{};

/// Helper to get the environment type of a sender or receiver. This is the type returned by get_env
/// when called with an object of type T.
template < typename T >
using env_type = decltype( get_env( std::declval< T const& >() ) );

struct _get_completion_signatures_t
{
        template < typename S, typename E >
        decltype( auto ) operator()( S& s, E& e ) const
        {
                static constexpr bool has_member = requires( S& s, E& e ) {
                        { s.get_completion_signatures( e ) };
                };
                if constexpr ( has_member )
                        return s.get_completion_signatures( e );
                else {
                        using sigs = typename S::completion_signatures;
                        return sigs{};
                }
        }
};
/// CPO for getting the completion signatures from a sender. If the sender has a
/// get_completion_signatures member function, it is called with the environment to get the
/// completion signatures. Otherwise, it returns the sender's completion_signatures member type.
///
inline constexpr _get_completion_signatures_t get_completion_signatures{};

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

template < typename S, typename R >
concept _connectable = sender< S > && receiver< R > && requires( S&& s, R&& r ) {
        { std::move( s ).connect( std::move( r ) ) };
};

struct _connect_t
{
        template < typename S, typename R >
                requires _connectable< S, R >
        auto operator()( S&& s, R&& r ) const
            noexcept( noexcept( std::move( s ).connect( std::move( r ) ) ) )
        {
                return std::move( s ).connect( std::move( r ) );
        }
};
/// Connect CPO for connecting senders to receivers.
inline constexpr _connect_t connect{};

template < typename R, typename T >
struct _receiver_sig_callable;

template < typename R, typename... Args >
struct _receiver_sig_callable< R, set_value_t( Args... ) >
{
        static constexpr bool value =
            requires( R& r, Args&&... args ) { r.set_value( (Args&&) args... ); };
};

template < typename R, typename Err >
struct _receiver_sig_callable< R, set_error_t( Err ) >
{
        static constexpr bool value = requires( R& r, Err&& err ) { r.set_error( (Err&&) err ); };
};

template < typename R >
struct _receiver_sig_callable< R, set_stopped_t() >
{
        static constexpr bool value = requires( R& r ) { r.set_stopped(); };
};

template < typename R, typename Sigs >
struct _receiver_for_impl;

template < typename R, typename... S >
struct _receiver_for_impl< R, completion_signatures< S... > >
{
        static constexpr bool value = ( ... && _receiver_sig_callable< R, S >::value );
};

/// Concept for receivers that can handle a specific sender's completion signatures. Requires that R
/// satisfies the receiver concept and has the callable member functions required by every signature
/// in the completion signatures of sender S (queried with R's own environment). Use this to
/// constrain connect() overloads to receivers that are compatible with a given sender type.
template < typename R, typename S >
concept receiver_for =
    receiver< R > && _receiver_for_impl< R, _sender_completions_t< S, env_type< R > > >::value;

/// Concept for receivers that can handle an explicitly listed set of completion signatures.
/// Requires that R satisfies the receiver concept and has callable member functions for each of the
/// provided signatures Sig. Unlike receiver_for, the signatures are supplied directly rather than
/// derived from a sender type, which creates more human-friendly error messages when the constraint
/// is not satisfied, as the signatures are explicitly listed in the error rather than being buried
/// inside the sender's type.
template < typename R, typename... Sig >
concept receiver_for_sigs = receiver< R > && ( ... && _receiver_sig_callable< R, Sig >::value );

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

/// ------------------------------------------------------------------------------


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

template < typename S, typename... Ts >
struct _sigs_append_impl;

template < typename... S, typename... Ts >
struct _sigs_append_impl< completion_signatures< S... >, Ts... >
{
        using type = completion_signatures< S..., Ts... >;
};

/// Helper to append a list of signatures to an existing list of completion signatures.
template < typename Sigs, typename... Ts >
using _sigs_append_t = typename _sigs_append_impl< Sigs, Ts... >::type;

template < typename S, typename... Ts >
struct _sigs_concat_impl;

template < typename... S >
struct _sigs_concat_impl< completion_signatures< S... > >
{
        using type = completion_signatures< S... >;
};

template < typename... S1, typename... S2, typename... Ts >
struct _sigs_concat_impl< completion_signatures< S1... >, completion_signatures< S2... >, Ts... >
  : _sigs_concat_impl< completion_signatures< S1..., S2... >, Ts... >
{
};

/// Helper to concatenate multiple lists of completion signatures into a single list that contains
/// all signatures from the input lists.
template < typename... Sigs >
using _sigs_concat_t = typename _sigs_concat_impl< completion_signatures<>, Sigs... >::type;

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
        [[nodiscard]] inplace_stop_token get_token() const noexcept;

        /// Check if stopping is possible. For inplace_stop_source, this always returns true since
        /// it can always be stopped.
        [[nodiscard]] bool stop_possible() const noexcept
        {
                return true;
        }

        /// Check if a stop has been requested. This returns true if request_stop() has been called
        /// on this stop source, and false otherwise.
        [[nodiscard]] bool stop_requested() const noexcept
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

/// A minimal environment that carries a stop token and satisfies `get_stop_token` queries.
/// `Token` may be a value type or a reference type — e.g. `inplace_stop_token` or
/// `inplace_stop_token&`. Using a reference avoids copying and reflects live token state.
template < typename Token = inplace_stop_token >
struct stop_token_env
{
        Token _token;

        [[nodiscard]] decltype( auto ) query( get_stop_token_t ) const noexcept
        {
                return _token;
        }
};

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

        template < receiver R >
        _ll_op< R, S... > connect( R receiver ) noexcept(
            noexcept( _ll_op< R, S... >{ _ll, std::move( receiver ) } ) )
        {
                static_assert(
                    receiver_for_sigs< R, S... >,
                    "Receiver does not satisfy the requirements for the sender's completion signatures" );
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

        template < receiver R >
        _sh_op< R, K, S... > connect( R receiver ) && noexcept(
            noexcept( _sh_op< R, K, S... >{ _sh, std::move( _key ), std::move( receiver ) } ) )
        {
                static_assert(
                    receiver_for_sigs< R, S... >,
                    "Receiver does not satisfy the requirements for the sender's completion signatures" );
                return { _sh, std::move( _key ), std::move( receiver ) };
        }

        template < receiver R >
        _sh_op< R, K, S... > connect( R receiver ) const& noexcept(
            noexcept( _sh_op< R, K, S... >{ _sh, _key, std::move( receiver ) } ) )
        {
                static_assert(
                    receiver_for_sigs< R, S... >,
                    "Receiver does not satisfy the requirements for the sender's completion signatures" );
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
                        n._set_value( (V&&) value... );
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
                        n._set_error( (E1&&) err );
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
                        n._set_stopped();
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

// ------------------------------------------------------------------------------


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
                static_assert(
                    receiver_for< R, _just_error< T > >,
                    "Receiver does not satisfy the requirements for the sender's completion signatures" );
                return _op{ std::move( err ), std::move( receiver ) };
        }

        using completion_signatures = ecor::completion_signatures< set_error_t( T ) >;
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

// ------------------------------------------------------------------------------

enum class _awaitable_state_e : uint8_t
{
        empty,
        value
};

/// Base class for any item that can be placed in the `task_core` ready queue and resumed.
///
/// Derive from `schedulable` and override `resume()` to create custom schedulable items that
/// can be enqueued into `task_core` via `reschedule()`.
///
struct schedulable : zll::ll_base< schedulable >
{
        virtual void resume() = 0;
};

/// Maintains a list of ready tasks that can be resumed. Used by the library to re-schedule woken-up
/// tasks. Has to be periodically polled by the user to run the ready tasks, either by calling
/// run_once() to run a single task, or run_n() to run multiple tasks.
///
struct task_core
{
        /// Run a single ready task if there is one. This removes the task from the list of ready
        /// tasks and resumes it. Returns true if a task was run, or false if there were no ready
        /// tasks to run.
        bool run_once()
        {
                if ( _ready_tasks.empty() )
                        return false;
                auto& t = _ready_tasks.front();
                _ready_tasks.detach_front();
                t.resume();
                return true;
        }

        /// Run up to n ready tasks. This repeatedly calls run_once() until either n tasks have been
        /// run or there are no more ready tasks to run.
        void run_n( std::size_t n )
        {
                for ( std::size_t i = 0; i < n; ++i )
                        if ( !run_once() )
                                break;
        }

        /// Reschedule a task by adding it's promise to the list of ready tasks. The task will be
        /// resumed the next time run_once() or run_n() is called.
        void reschedule( schedulable& op ) noexcept
        {
                _ready_tasks.link_back( op );
        }

private:
        zll::ll_list< schedulable > _ready_tasks;
};

template < typename T >
struct _awaitable_expected
{
        _awaitable_expected() noexcept
        {
        }

        _awaitable_state_e state = _awaitable_state_e::empty;
        union
        {
                T val;
        };

        template < typename... Args >
        void set_value( Args&&... args ) noexcept( std::is_nothrow_constructible_v< T, Args... > )
        {
                new ( (void*) &val ) T( (Args&&) args... );
                state = _awaitable_state_e::value;
        }

        ~_awaitable_expected() noexcept
        {
                if ( state == _awaitable_state_e::value )
                        val.~T();
        }
};

template <>
struct _awaitable_expected< void >
{
        _awaitable_expected() noexcept = default;

        _awaitable_state_e state = _awaitable_state_e::empty;

        void set_value() noexcept
        {
                state = _awaitable_state_e::value;
        }
};

/// Helper to extract the value type from the completion signatures of a sender, for use in the
/// awaitable implementation. This is used to determine the type that the awaitable will return when
/// co_awaited.
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

/// Awaitable type for co_awaiting on senders. This is the type returned by the operator co_await of
/// senders, and contains the state for the awaitable operation, including the expected result of
/// the operation and the operation state for the sender.
///
/// Awaitable resumes coroutine only on on_value signal.
///
template < typename PromiseType, typename S >
struct _task_awaitable
{
        _task_awaitable( S sender )
          : _exp()
          , _op( std::move( sender ).connect(
                _receiver{ ._awaitable = this, ._promise = &_promise, ._exp = &_exp } ) )
        {
        }

        /// Awaitable is never ready
        [[nodiscard]] bool await_ready() const noexcept
        {
                return false;
        }

        /// Starts the operation state on awaiter suspension.
        void await_suspend( std::coroutine_handle< PromiseType > ch ) noexcept
        {
                _promise = &ch.promise();
                _promise->trace.on_await_suspend( *this );
                _op.start();
        }

        /// Resumes the coroutine when the operation completes. This assumes that the operation
        /// completed with a value, and returns the value if there is one. Undefined behavior if the
        /// operation completed with an error or was stopped.
        decltype( auto ) await_resume() noexcept
        {
                _promise->trace.on_await_resume( *this );
                ECOR_ASSERT( _exp.state == _awaitable_state_e::value );
                if constexpr ( std::same_as< value_type, void > )
                        return;
                else
                        return std::move( _exp.val );
        }

        using _env         = env_type< PromiseType >;
        using _completions = _sender_completions_t< std::remove_cvref_t< S >, _env >;
        using _values      = _filter_values_as_variant_tuple_t< _completions >;
        static_assert(
            std::variant_size_v< _values > <= 1,
            "Multiple set_value completions in awaitable not supported" );

        /// Type of value returned by the awaitable when co_awaited.
        using value_type = typename _awaitable_extract_type< _values >::type;

        struct _receiver
        {
                using receiver_concept = receiver_t;

                _task_awaitable*                   _awaitable;
                PromiseType**                      _promise;
                _awaitable_expected< value_type >* _exp;

                template < typename... Ts >
                void set_value( Ts&&... vals ) noexcept(
                    std::is_nothrow_constructible_v< value_type, Ts... > ||
                    std::same_as< value_type, void > )
                {
                        ( *_promise )->trace.on_await_set_value( *_awaitable, vals... );
                        _exp->set_value( (Ts&&) vals... );
                        ( *_promise )->core.reschedule( **_promise );
                }

                template < typename E >
                void set_error( E&& err )
                {
                        ( *_promise )->trace.on_await_set_error( *_awaitable, err );
                        ( *_promise )->invoke_set_error( (E&&) err );
                }

                void set_stopped()
                {
                        ( *_promise )->trace.on_await_set_stopped( *_awaitable );
                        ( *_promise )->invoke_set_stopped();
                }

                [[nodiscard]] decltype( auto ) get_env() const noexcept
                {
                        return ( *_promise )->get_env();
                }
        };


private:
        PromiseType*                      _promise = nullptr;
        _awaitable_expected< value_type > _exp;

        using _op_t = connect_type< S, _receiver >;
        _op_t _op;
};

template < class Alloc >
concept simple_allocator = requires( Alloc alloc, size_t n ) {
        { *alloc.allocate( n ) } -> std::same_as< typename Alloc::value_type& >;
        { alloc.deallocate( alloc.allocate( n ), n ) };
} && std::copy_constructible< Alloc > && std::equality_comparable< Alloc >;


/// Task memory resource that provides allocation and deallocation functions for tasks. This is used
/// by the task implementation to allocate memory for the task state, and can be customized by the
/// user to use different allocation strategies.
///
/// Type erasure is used to allow the task implementation to use the memory resource without knowing
/// its type, and the memory resource is stored as a pointer to the user-provided allocator along
/// with function pointers for allocation and deallocation that operate on the user-provided
/// allocator.
///
struct task_memory_resource
{
        template < typename M >
        task_memory_resource( M& m ) noexcept
          : mem( &m )
        {
                alloc = +[]( void* mem, std::size_t const sz, std::size_t const align ) {
                        return ecor::allocate( *( (M*) mem ), sz, align );
                };
                dealloc = +[]( void* mem, void* p, std::size_t const sz, std::size_t const align ) {
                        ecor::deallocate( *( (M*) mem ), p, sz, align );
                };
        }

        [[nodiscard]] void* allocate( std::size_t const sz, std::size_t const align ) const
        {
                return alloc( mem, sz, align );
        }

        void deallocate( void* p, std::size_t const sz, std::size_t const align ) const
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
/// CPO for getting the task core from the context argument.
inline constexpr get_task_core_t get_task_core{};

struct get_memory_resource_t
{
        template < typename T >
        decltype( auto ) operator()( T& t ) const noexcept
        {
                return t.query( get_memory_resource_t{} );
        }
};
/// CPO for getting the memory resource from the context argument.
inline constexpr get_memory_resource_t get_memory_resource{};

/// Basic task context that provides the task with necessary resources: task_core and
/// task_memory_resource.
///
/// Users can customize the context by providing their own type that contains these resources and
/// implements the query functions.
///
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

/// Concept for the task context. A type satisfies this concept if it provides a task core and a
/// task memory resource that can be accessed through the get_task_core and get_memory_resource
/// CPOs.
template < typename T >
concept task_context = requires( T t ) {
        { get_task_core( t ) } -> std::same_as< task_core& >;
        { get_memory_resource( t ) } -> std::same_as< task_memory_resource& >;
};

/// EXPERIMENTAL: the task tracing API (this type, the `trace_type` slot in `task_config`, and
/// every `on_*` hook below) is unstable and may change drastically between releases. Use at
/// your own risk; pin a version if you depend on the exact shape of these hooks.
///
/// User trace types must define every hook on this default type — the promise calls them
/// unconditionally. A user trace must not outlive the promise that owns it; hooks may store
/// the address of the promise but must not access it after the promise is destroyed.
///
/// Lifecycle of a single task call, in roughly the order the hooks fire:
///   on_alloc → on_promise_construct → on_op_start → on_resume → (await cycles) →
///   on_return → (on_set_value | on_set_error | on_set_stopped) → on_final_suspend →
///   on_dealloc.
///
/// Every hook receives the owning `promise` (or `op` / `awaiter`) by reference so a trace
/// can correlate events belonging to the same task instance by address.
struct task_default_trace
{
        /// Called from `_promise_type::operator new` right after the coroutine frame has been
        /// allocated from the task memory resource. `sz` is the frame size in bytes, `p` is the
        /// pointer that was just allocated (or null on failure). Use this to account for memory
        /// usage per task.
        ECOR_FORCE_INLINE static void
        on_alloc( auto& /*ctx*/, std::size_t const /*sz*/, void* /*p*/ ) noexcept
        {
        }

        /// Called from `_promise_type::operator delete` just before the coroutine frame is
        /// returned to the task memory resource. Pairs 1:1 with `on_alloc`.
        ECOR_FORCE_INLINE static void on_dealloc( void* /*p*/, std::size_t const /*sz*/ ) noexcept
        {
        }

        /// Called from the promise constructor, after the task core and the trace member itself
        /// have been initialized but before the coroutine body has started running. The variadic
        /// pack contains the original arguments passed to the coroutine function (the same ones
        /// the coroutine body receives), giving you a chance to capture identifying parameters.
        ECOR_FORCE_INLINE void
        on_promise_construct( auto& /*promise*/, auto& /*ctx*/, auto&... ) noexcept
        {
        }

        /// Called from `_task_op::start()` — i.e. when a connected sender is started by its
        /// receiver. This marks the moment the task is first scheduled on the task core; it
        /// fires exactly once per operation state.
        ECOR_FORCE_INLINE void on_op_start( auto& /*op*/ ) noexcept
        {
        }

        /// Called every time the scheduler picks this task and is about to resume the coroutine
        /// handle. Fires once per scheduling tick — i.e. once after `on_op_start`, then again
        /// after each `co_await` that suspended the task.
        ECOR_FORCE_INLINE void on_resume( auto& /*promise*/ ) noexcept
        {
        }

        /// Called when the task's own completion is being delivered to its continuation
        /// (the receiver that connected this task) with `set_value`. Fires after `on_return`
        /// and just before the receiver's `set_value` is invoked. The pack holds the value(s).
        ECOR_FORCE_INLINE void on_set_value( auto& /*promise*/, auto&... /*value*/ ) noexcept
        {
        }

        /// Called when the task is completing its continuation with `set_error`. This happens
        /// either because an awaited child sender errored and the error was propagated, or
        /// because the coroutine body threw (see `on_unhandled_exception`).
        ECOR_FORCE_INLINE void on_set_error( auto& /*promise*/, auto& /*error*/ ) noexcept
        {
        }

        /// Called when the task is completing its continuation with `set_stopped`, i.e. the
        /// task was cancelled (typically because an awaited child sender propagated stop).
        ECOR_FORCE_INLINE void on_set_stopped( auto& /*promise*/ ) noexcept
        {
        }

        /// Called from `return_value` — the coroutine body executed `co_return <expr>`. Fires
        /// before `on_set_value` so a trace can observe the produced value at the point the
        /// coroutine emits it.
        ECOR_FORCE_INLINE void on_return( auto& /*promise*/, auto& /*value*/ ) noexcept
        {
        }

        /// Called from `return_void` — the `task<void>` coroutine body reached `co_return;` or
        /// fell off the end. Same role as the value overload, for void tasks.
        ECOR_FORCE_INLINE void on_return( auto& /*promise*/ ) noexcept
        {
        }

        /// Called from the promise destructor when the coroutine frame is being torn down while
        /// a continuation was still attached — meaning the task never delivered a completion
        /// to it. The promise will synthesize `set_stopped` for the continuation right after
        /// this hook returns. Useful for spotting tasks abandoned by their op_state.
        ECOR_FORCE_INLINE void on_destroy_with_continuation( auto& /*promise*/ ) noexcept
        {
        }

        /// Called when an exception escapes the coroutine body. The promise will translate it
        /// into `set_error(task_error::task_unhandled_exception)` immediately after this hook.
        ECOR_FORCE_INLINE void on_unhandled_exception( auto& /*promise*/ ) noexcept
        {
        }

        /// Called from `final_suspend()` — the coroutine has finished executing and is about
        /// to suspend for the last time. After this point the frame is inert and waiting to
        /// be destroyed (followed eventually by `on_dealloc`).
        ECOR_FORCE_INLINE void on_final_suspend( auto& /*promise*/ ) noexcept
        {
        }

        /// Called inside `_task_awaitable::await_resume` — the task is being resumed after a
        /// `co_await` on a child sender, and the child completed with a value. Fires just
        /// before the value is handed back to the coroutine body.
        ECOR_FORCE_INLINE void on_await_resume( auto& /*awaiter*/ ) noexcept
        {
        }

        /// Called inside `_task_awaitable::await_suspend` — the coroutine reached a
        /// `co_await` on a child sender and is about to suspend itself; the child sender's
        /// op_state will be started immediately after.
        ECOR_FORCE_INLINE void on_await_suspend( auto& /*awaiter*/ ) noexcept
        {
        }

        /// Called when the awaited child sender completed with `set_value`. Fires before the
        /// task is rescheduled to be resumed. The pack contains the value(s) produced by the
        /// child sender.
        ECOR_FORCE_INLINE void on_await_set_value( auto& /*awaiter*/, auto&... /*value*/ ) noexcept
        {
        }

        /// Called when the awaited child sender completed with `set_error`. The task will
        /// propagate this error to its own continuation (triggering `on_set_error`) instead of
        /// being resumed.
        ECOR_FORCE_INLINE void on_await_set_error( auto& /*awaiter*/, auto& /*error*/ ) noexcept
        {
        }

        /// Called when the awaited child sender completed with `set_stopped`. The task will
        /// propagate stop to its own continuation (triggering `on_set_stopped`) instead of
        /// being resumed.
        ECOR_FORCE_INLINE void on_await_set_stopped( auto& /*awaiter*/ ) noexcept
        {
        }
};

/// Task can be configured by passing type with configuration information. This is default value.
struct task_default_cfg
{
        using extra_error_signatures = completion_signatures<>;
        using trace_type             = task_default_trace;
};

/// Concept for the task configuration. A type is task configuration if:
/// - It has a nested type `extra_error_signatures` that is a `completion_signatures` type, which
///   specifies the additional error signatures that the task can complete with, in addition to the
///   default set of error signatures defined by the library. This allows the user to customize the
///   error handling of the task.
/// - It has a nested type `trace_type` (EXPERIMENTAL — see `task_default_trace`) used by the
///   promise to dispatch trace events. Use `task_default_trace` if you do not need tracing.
template < typename T >
concept task_config = requires() {
        typename T::extra_error_signatures;
        typename T::trace_type;
};

template < typename T, task_config CFG = task_default_cfg >
struct task;

/// Errors that can be emitted by the task implementation.
enum class task_error : uint8_t
{
        none                     = 1,  ///< No error
        task_unfinished          = 2,  ///< Task was destructed without signal.
        task_allocation_failure  = 3,  ///< Task memory allocation failed.
        task_already_started     = 4,  ///< Task was already started.
        task_unhandled_exception = 5,  ///< Task threw an exception that was not caught.
        task_missing             = 6,  ///< Coroutine task handle is empty.
};

/// Base class for task promises. This implements the common functionality for all task promises,
/// such as memory allocation and deallocation, and provides the interface for rescheduling the task
/// when it is resumed by the task scheduler.
struct _promise_base : schedulable
{
        static constexpr std::size_t align   = alignof( std::max_align_t );
        static constexpr std::size_t spacing = align > sizeof( void* ) ? align : sizeof( void* );

        /// Allocate memory for the task promise using the task memory resource, and store a pointer
        /// to the memory resource in the allocated memory for later use in deallocation.
        static void* _alloc( std::size_t sz, task_memory_resource& mem )
        {
                sz += spacing;
                void* const vp = allocate( mem, sz, align );
                if ( !vp )
                        return nullptr;
                auto* pmem = &mem;
                std::memcpy( vp, (void const*) &pmem, sizeof( void* ) );
                return ( (char*) vp ) + spacing;
        }

        /// Deallocate memory for the task promise using the task memory resource. This retrieves
        /// the pointer to the memory resource that was stored in the allocated memory during
        /// allocation, and uses it to deallocate the memory for the task promise.
        static void _dealloc( void* const ptr, std::size_t const sz )
        {
                void*                 beg = ( (char*) ptr ) - spacing;
                task_memory_resource* mem = nullptr;
                std::memcpy( (void*) &mem, beg, sizeof( void* ) );
                deallocate( *mem, beg, sz + spacing, align );
        }

        _promise_base( _promise_base const& )            = delete;
        _promise_base& operator=( _promise_base const& ) = delete;

        _promise_base( task_core& c )
          : core( c )
          , token()
        {
        }

        std::suspend_always initial_suspend() noexcept
        {
                return {};
        }

        task_core&         core;
        inplace_stop_token token;

        using _env = stop_token_env< inplace_stop_token& >;

        _env get_env() noexcept
        {
                return { token };
        }
};

template < typename Task >
struct _promise_type;

/// Promise type subset that implements value_type specific behavior of the task.
template < typename Task, typename Val >
struct _promise_return_mixin
{
        void return_value( Val v )
        {
                auto* p = static_cast< _promise_type< Task >* >( this );
                p->trace.on_return( *p, v );
                p->invoke_set_value( std::move( v ) );
        }
};

template < typename Task >
struct _promise_return_mixin< Task, void >
{
        void return_void()
        {
                auto* p = static_cast< _promise_type< Task >* >( this );
                p->trace.on_return( *p );
                p->invoke_set_value();
        }
};

/// Promise type for the task. This implements the promise interface required for C++ coroutines,
/// and inherits from the promise base and the value mixin to provide the necessary functionality
/// for the task promise.
///
/// - Suspends on initial_suspend, is expected to run after start() is called on the operation state
///   returned by connect().
/// - Provides API for library to signal sender completions to the task's receiver, by invoking the
///   appropriate set_value, set_error, or set_stopped signals on the stored continuable object.
/// - Allows yielding errors that are compatible with the task's error signatures, by implementing
///   yield_value that forwards to the awaitable transformation of just_error sender.
/// - Provides the await_transform function that wraps any sender into _task_awaitable, which allows
///   co_awaiting on senders from the task coroutine.
///
template < typename Task >
struct _promise_type : _promise_base, _promise_return_mixin< Task, typename Task::value_type >
{
        using value_type = typename Task::value_type;
        using cfg_type   = typename Task::config_type;
        using trace_type = typename cfg_type::trace_type;
        using _promise_base::_promise_base;
        using vtable = _apply_to_sigs_t< _sig_vtable, typename Task::completion_signatures >;

        _promise_type( _promise_type const& )            = delete;
        _promise_type& operator=( _promise_type const& ) = delete;

        void* operator new( std::size_t const sz, task_context auto&& ctx, auto&&... ) noexcept
        {
                /// XXX: we can't guarantee noexcept of the subcalls - try/catch?
                task_memory_resource& a = get_memory_resource( ctx );
                void*                 p = _alloc( sz, a );
                trace_type::on_alloc( ctx, sz, p );
                return p;
        }

        void operator delete( void* const ptr, std::size_t const sz ) noexcept
        {
                /// XXX: we can't guarantee noexcept of the subcalls - try/catch?
                trace_type::on_dealloc( ptr, sz );
                _dealloc( ptr, sz );
        }

        /// Construct the promise base by extracting the task core from the context argument and
        /// initializing the token.
        _promise_type( task_context auto&& ctx, auto&&... s )
          : _promise_base( get_task_core( ctx ) )
          , trace()
        {
                trace.on_promise_construct( *this, ctx, s... );
        }

        static Task get_return_object_on_allocation_failure()
        {
                return {
                    std::coroutine_handle< _promise_type >{}, task_error::task_allocation_failure };
        }

        Task get_return_object()
        {
                return { std::coroutine_handle< _promise_type >::from_promise( *this ) };
        }

        template < typename E >
                requires( _vtable_can_call_error< vtable, E > )
        auto yield_value( with_error< E > error )
        {
                return await_transform( just_error( std::move( error.error ) ) );
        }

        template < typename T >
        decltype( auto ) await_transform( T&& x ) noexcept
        {
                using U = std::remove_cvref_t< T >;
                if constexpr ( sender< U > ) {
                        using compls = _sender_completions_t< U, typename _promise_base::_env >;
                        using errs   = _filter_errors_of_t< compls >;
                        static_assert(
                            _sig_is_underset_of< errs, typename Task::_error_completions >,
                            "Error signatures are not compatible" );
                        using vals = _filter_values_as_variant_tuple_t< compls >;
                        static_assert(
                            std::variant_size_v< vals > <= 1,
                            "Sender used in co_await must only complete with a single set_value signature" );
                        return _task_awaitable< _promise_type, T >{ (T&&) x };
                } else {
                        return (T&&) x;
                }
        }

        void unhandled_exception() noexcept
        {
                trace.on_unhandled_exception( *this );
                this->invoke_set_error( task_error::task_unhandled_exception );
        }

        void resume() override
        {
                trace.on_resume( *this );
                auto h = std::coroutine_handle< _promise_type >::from_promise( *this );
                h.resume();
        }

        template < typename U >
        void invoke_set_error( U&& err )
        {
                trace.on_set_error( *this, err );
                ECOR_ASSERT( this->_cont_vtable );
                if ( this->_cont_vtable ) {
                        auto* r = std::exchange( this->_cont_vtable, nullptr );
                        r->invoke( set_error_t{}, _cont_obj, (U&&) err );
                }
        }

        template < typename... U >
        void invoke_set_value( U&&... v )
        {
                trace.on_set_value( *this, v... );
                ECOR_ASSERT( this->_cont_vtable );
                if ( this->_cont_vtable ) {
                        auto* r = std::exchange( this->_cont_vtable, nullptr );
                        r->invoke( set_value_t{}, this->_cont_obj, (U&&) v... );
                }
        }

        void invoke_set_stopped()
        {
                trace.on_set_stopped( *this );
                ECOR_ASSERT( this->_cont_vtable );
                if ( this->_cont_vtable ) {
                        auto* r = std::exchange( this->_cont_vtable, nullptr );
                        r->invoke( set_stopped_t{}, this->_cont_obj );
                }
        }

        std::suspend_always final_suspend() noexcept
        {
                trace.on_final_suspend( *this );
                if ( _cont_vtable ) {
                        auto* r = std::exchange( this->_cont_vtable, nullptr );
                        r->invoke( set_error_t{}, _cont_obj, task_error::task_unfinished );
                }
                return {};
        }

        template < typename U >
        void setup_continuable( U& p )
        {
                this->_cont_vtable = &_vtable_of< U, U, vtable >;
                this->_cont_obj    = static_cast< void* >( &p );
        }

        ~_promise_type() noexcept
        {
                if ( _cont_vtable ) {
                        trace.on_destroy_with_continuation( *this );
                        this->_cont_vtable->invoke( set_stopped_t{}, _cont_obj );
                }
        }


        ECOR_NO_UNIQUE_ADDRESS trace_type trace;

private:
        vtable const* _cont_vtable = nullptr;
        void*         _cont_obj    = nullptr;
};

/// Operation state for the task sender. This is the type returned by the connect() function of the
/// task sender, and it contains the state for the operation of starting the task, including the
/// coroutine handle for the task promise, the receiver to which the task will send its completion
/// signals, and any error that occurred during the setup of the operation. The start() function of
/// this type is called to start the task, which will resume the task coroutine and begin its
/// execution.
template < typename T, task_config CFG, typename R >
struct _task_op
{
        using promise_type            = _promise_type< task< T, CFG > >;
        using operation_state_concept = operation_state_t;

        _task_op() noexcept = default;
        _task_op( task_error err, std::coroutine_handle< promise_type > h, R r ) noexcept(
            std::is_nothrow_move_constructible_v< R > )
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
          , _error( std::exchange( other._error, task_error::none ) )
        {
        }

        _task_op& operator=( _task_op&& other ) noexcept
        {
                if ( this == &other )
                        return *this;
                _task_op cpy{ std::move( other ) };
                std::swap( _h, cpy._h );
                std::swap( _recv, cpy._recv );
                std::swap( _error, cpy._error );
                return *this;
        }

        /// Start the task by resuming the task coroutine. If there was an error during the setup of
        /// the operation, such as an allocation failure or the task being already started, then the
        /// error is sent to the receiver instead of starting the task.
        ///
        /// If the task was successfully started, then the task coroutine will be resumed and will
        /// begin its execution, and the completion signals from the task will be sent to the
        /// receiver when the task completes.
        ///
        /// Coroutine is not resumed in this call, but rather scheduled to be resumed by the task
        /// core.
        ///
        void start()
        {
                if ( _error != task_error::none ) {
                        _recv.set_error( _error );
                        return;
                }
                if ( !_h ) {
                        _recv.set_error( task_error::task_missing );
                        return;
                }
                if ( _h.done() ) {
                        _recv.set_error( task_error::task_already_started );
                        return;
                }
                if constexpr ( !std::same_as<
                                   decltype( ecor::get_stop_token( ecor::get_env( _recv ) ) ),
                                   never_stop_token > )
                        _h.promise().token = ecor::get_stop_token( ecor::get_env( _recv ) );
                _h.promise().setup_continuable( _recv );
                _h.promise().trace.on_op_start( *this );
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

        /// Destroy the task operation. Destroys the coroutine handle if valid, covering both
        /// coroutines still suspended mid-execution and coroutines parked at final_suspend
        /// (done() == true). Because final_suspend returns suspend_always the runtime does not
        /// free the frame automatically — the holder of the handle is responsible in all cases.
        ///
        ~_task_op()
        {
                if ( this->_h )
                        this->_h.destroy();
        }

        std::coroutine_handle< promise_type > _h;
        R                                     _recv;
        task_error                            _error = task_error::none;
};

/// Task type that represents an asynchronous operation that is implemented as a coroutine and is a
/// sender.
///
/// The task type is parameterized by the value type it provides, and an optional configuration type
/// that specifies additional behavior during compilation.
///
/// Task expectes argument satisfying task_context concept to be passed to the coroutine as first
/// argument. This is used to provide the task with necessary runtime resources, such as the task
/// core for scheduling and the task memory resource for dynamic memory allocation.
///
/// Task can be scheduled and processed as normal sender in the sender/receiver framework. The task
/// has capability to co_await any sender as long as:
///  - The sender has at most one set_value completion_signature.
///  - The sender has only set_error completions that are compatible with the current task.
///  - Note: The sender can have set_stopped completion signature.
/// As a consequence of task being a sender and being able to await sender - tasks can await other
/// tasks.
///
/// Task always starts suspended and only starts execution after start() was called on the operation
/// state returned by connect().
///
/// When co_awaiting on a sender, the task will suspend until the sender completes, value signal
/// of sender does not immediately resume the task, but instead the task is scheduled to be resumed
/// by the task core. In case of an error or stop signal from the sender, the task will not be
/// resumed, but instead the error or stop signal will be immediately propagated to the task's
/// receiver, and the task will be considered completed and will be destructed.
///
/// Task completion signature is always fixed and independent of the content of the coroutine
/// itself, or the receiver environment:
///   - set_value_t( value_type ) where value_type is the type specified by the user as the first
///     template parameter of the task. If the value type is void, then the set_value signature is
///     set_value_t().
///   - set_error_t( task_error ) - This is the default error signature that all tasks have, and it
///     is used to report errors that occur during the execution of the task, such as allocation
///     failures or unfinished
///   - Additional error signatures specified by the user in the task configuration.
///   - set_stopped_t() - This is used to report that the task was stopped before it could complete,
///     either by the user or by the library.
///
template < typename T, task_config CFG >
struct task
{
        using sender_concept = sender_t;
        using value_type     = T;
        using config_type    = CFG;
        using promise_type   = _promise_type< task >;

        using _error_completions =
            _sigs_append_t< typename CFG::extra_error_signatures, set_error_t( task_error ) >;

        using completion_signatures =
            _sigs_append_t< _error_completions, _value_setter_t< T >, set_stopped_t() >;

        static_assert(
            alignof( promise_type ) <= alignof( std::max_align_t ),
            "Unsupported alignment" );

        /// Constructor used by promise_type for coroutine creation.
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
          , _error( std::exchange( other._error, task_error::none ) )
        {
        }

        task& operator=( task&& other ) noexcept
        {
                if ( this != &other ) {
                        task tmp = std::move( other );
                        std::swap( _h, tmp._h );
                        std::swap( _error, tmp._error );
                }
                return *this;
        }

        /// Connect the task to a receiver. This returns an operation state that can be started to
        /// begin the execution of the task.
        ///
        /// The operation is expected to be destructive, the task can't be connected again after
        /// connect call.
        ///
        template < receiver R >
        auto connect( R receiver ) &&
        {
                static_assert(
                    receiver_for< R, task< T, CFG > >,
                    "Receiver does not satisfy the requirements for the task's completion signatures" );
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

/// -------------------------------------------------------------------------------

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

        // XXX: cancel of the other one shall be used instead
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
                        return ecor::get_env( _recv );
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
        _completions< Env > get_completion_signatures( Env&& ) noexcept
        {
                return {};
        }


        template < receiver R >
        _or_op< S1, S2, R > connect( R receiver )
        {
                static_assert(
                    receiver_for< R, _or_sender >,
                    "Receiver does not satisfy the requirements for the combined completion signatures of the two senders" );
                return { std::move( _s1 ), std::move( _s2 ), std::move( receiver ) };
        }

private:
        S1 _s1;
        S2 _s2;
};

/// XXX: not recommended for production use, is not safe - lifetime of the senders is not managed in
/// any way, and it's on the user to ensure that they are valid until the completion of the
/// operation. This is mostly intended for testing and demonstration purposes, and should be used
/// with caution in production code.
template < typename S1, typename S2 >
_or_sender< S1, S2 > operator||( S1 s1, S2 s2 )
{
        return { std::move( s1 ), std::move( s2 ) };
}

/// -------------------------------------------------------------------------------

template < sender S >
struct _as_variant
{
        using sender_concept = sender_t;

        template < typename Env >
        using _s_completions = _sender_completions_t< S, Env >;

        /// All set_value completions of the sender, transformed to have their value types wrapped
        /// in a std::variant.
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
        using _completions = _sigs_concat_t<
            completion_signatures< set_value_t( _values< Env > ) >,
            _error< Env >,
            _stopped< Env > >;

        _as_variant( S s )
          : _s( std::move( s ) )
        {
        }

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& ) noexcept
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

                using _env       = env_type< R >;
                using value_type = _values< _env >;

                template < typename T >
                void set_value( T&& val ) noexcept
                {
                        R::set_value( value_type{ (T&&) val } );
                }
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                static_assert(
                    is_all_singular_v< _s_completions< env_type< R > > >,
                    "All set_value signatures of sender must be singular (exactly one argument) to use as_variant" );
                return std::move( _s ).connect( _recv< R >{ std::move( receiver ) } );
        }

private:
        S _s;
};

/// CPO for transforming a sender to have its set_value completions wrapped in a std::variant. This
/// is useful for cases where a sender has multiple set_value completions with different value
/// types, and the user wants to unify them into a single set_value completion with a std::variant
/// of the value types.
///
/// Expects that all set_value completions of the sender have exactly one argument, and the
/// resulting set_value completion of the transformed sender will have a single argument of type
/// std::variant of the original value types.
///
[[maybe_unused]] static inline struct as_variant_t
{
        template < sender S >
        auto operator()( S s ) const
        {
                return _as_variant< S >{ std::move( s ) };
        }

} as_variant;

/// Operator| overload for as_variant CPO. This allows using the pipe syntax to apply the as_variant
/// transformation to a sender.
auto operator|( auto s, as_variant_t )
{
        return as_variant( std::move( s ) );
}

/// -------------------------------------------------------------------------------

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
            _sigs_concat_t< _s_values< Env >, _s_errors_as_val< Env >, _stopped< Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& ) noexcept
        {
                return {};
        }

        _err_to_val( S s )
          : _s( std::move( s ) )
        {
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
                void set_error( Ts&&... errs )
                {
                        R::set_value( (Ts&&) errs... );
                }
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                static_assert(
                    receiver_for< R, _err_to_val >,
                    "Receiver does not satisfy the requirements for the err_to_val transformation" );
                return std::move( _s ).connect( _recv< R >{ std::move( receiver ) } );
        }

private:
        S _s;
};

/// CPO for transforming a sender to convert all its set_error completions into set_value
/// completions.
[[maybe_unused]] static inline struct err_to_val_t
{
        template < sender S >
        auto operator()( S s ) const
        {
                return _err_to_val< S >{ std::move( s ) };
        }

} err_to_val;

/// Operator| overload for err_to_val CPO. This allows using the pipe syntax to apply the err_to_val
/// transformation to a sender.
auto operator|( auto s, err_to_val_t )
{
        return err_to_val( std::move( s ) );
}

/// -------------------------------------------------------------------------------

template < sender S >
struct _sink_err
{
        using sender_concept = sender_t;

        template < typename Env >
        using _s_completions = _sender_completions_t< S, Env >;

        template < typename Env >
        using _s_errors = typename _filter_map_tag<
            std::variant<>,
            set_error_t,
            _s_completions< Env >,
            _type_identity_t >::type;

        template < typename Env >
        using _s_errors_as_val = std::optional< _s_errors< Env > >;

        template < typename Env >
        using _s_values = _filter_values_of_t< _s_completions< Env > >;

        template < typename Env >
        using _s_stopped = _filter_stopped_of_t< _s_completions< Env > >;

        template < typename Env >
        using _completions = _sigs_merge_t<
            completion_signatures< set_value_t( _s_errors_as_val< Env > ) >,
            _s_stopped< Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& ) noexcept
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

                using _env    = env_type< R >;
                using value_t = _s_errors_as_val< _env >;

                void set_value() noexcept
                {
                        R::set_value( value_t{} );
                }

                template < typename T >
                void set_error( T&& err ) noexcept
                {
                        R::set_value( value_t{ (T&&) err } );
                }
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                using _env = env_type< R >;
                static_assert(
                    std::variant_size_v< _s_errors< _env > > > 0,
                    "Sender used with sink_err must have at least one error type" );
                static_assert(
                    std::same_as< _s_values< _env >, completion_signatures<> > ||
                        std::same_as< _s_values< _env >, completion_signatures< set_value_t() > >,
                    "Sender used with sink_err must not complete with set_value, or there has to be only one set_value of shape set_value_t()" );
                static_assert(
                    receiver_for< R, _sink_err< S > >,
                    "Receiver does not satisfy the requirements for the sink_err sender's completion signatures" );
                return std::move( _s ).connect( _recv< R >{ std::move( receiver ) } );
        }

        S _s;
};

/// CPO for transforming a sender to convert all its set_error completions into set_value
/// completion, that returns std::optional of std::variant of errors. It expects that the sender
/// does not have any set_value completions, or it has only one set_value completion with no
/// arguments.
///
[[maybe_unused]] static inline struct sink_err_t
{

        auto operator()( sender auto s ) const noexcept
        {
                return _sink_err{ std::move( s ) };
        }

} sink_err;

/// Operator| overload for sink_err CPO. This allows using the pipe syntax to apply the sink_err
/// transformation to a sender.
auto operator|( sender auto s, sink_err_t ) noexcept
{
        return _sink_err{ std::move( s ) };
}

/// -------------------------------------------------------------------------------

/// Helper for then: maps a set_value_t(Args...) signature through callable F.
/// Produces set_value_t() if F returns void, otherwise set_value_t(invoke_result_t<F, Args...>).
template < typename Ret >
struct _then_value_sig_ret
{
        using type = set_value_t( Ret );
};

template <>
struct _then_value_sig_ret< void >
{
        using type = set_value_t();
};

template < typename F >
struct _then_value_sig
{
        template < typename... Args >
        using type = typename _then_value_sig_ret< std::invoke_result_t< F, Args... > >::type;
};

template < sender S, typename F >
struct _then
{
        using sender_concept = sender_t;

        template < typename Env >
        using _s_completions = _sender_completions_t< S, Env >;

        template < typename Env >
        using _s_errors = _filter_errors_of_t< _s_completions< Env > >;

        template < typename Env >
        using _s_stopped = _filter_stopped_of_t< _s_completions< Env > >;

        template < typename Env >
        using _s_mapped_values = typename _filter_map_tag<
            completion_signatures<>,
            set_value_t,
            _s_completions< Env >,
            _then_value_sig< F >::template type >::type;

        template < typename Env >
        using _completions =
            _sigs_merge_t< _s_mapped_values< Env >, _s_errors< Env >, _s_stopped< Env > >;

        template < typename Env >
        _completions< Env > get_completion_signatures( Env&& ) noexcept
        {
                return {};
        }

        template < typename R >
        struct _recv : R
        {
                using receiver_concept = receiver_t;

                F _f;

                _recv( R r, F f )
                  : R( std::move( r ) )
                  , _f( std::move( f ) )
                {
                }

                template < typename... Args >
                void set_value( Args&&... args )
                {
                        if constexpr ( std::is_void_v< std::invoke_result_t< F, Args... > > ) {
                                std::move( _f )( (Args&&) args... );
                                R::set_value();
                        } else {
                                R::set_value( std::move( _f )( (Args&&) args... ) );
                        }
                }
        };

        template < receiver R >
        auto connect( R receiver ) &&
        {
                static_assert(
                    receiver_for< R, _then >,
                    "Receiver does not satisfy the requirements for the then sender's completion signatures" );
                return std::move( _s ).connect(
                    _recv< R >{ std::move( receiver ), std::move( _f ) } );
        }

        S _s;
        F _f;
};

/// Closure type returned by then(f) — can be used with the pipe operator.
template < typename F >
struct _then_closure
{
        F _f;
};

template < sender S, typename F >
auto operator|( S s, _then_closure< F > c )
{
        return _then< S, F >{ std::move( s ), std::move( c._f ) };
}

/// CPO for applying a callable transformation to each set_value signal. Errors and stopped
/// signals pass through unchanged. The callable is invoked with the value arguments of each
/// set_value completion. If the callable returns void, the output sender emits set_value_t().
/// Otherwise it emits set_value_t( result ).
///
[[maybe_unused]] static inline struct then_t
{
        template < typename F >
        auto operator()( F f ) const
        {
                return _then_closure< F >{ std::move( f ) };
        }

        template < sender S, typename F >
        auto operator()( S s, F f ) const
        {
                return _then< S, F >{ std::move( s ), std::move( f ) };
        }

} then;

/// -------------------------------------------------------------------------------

/// Implementation base for `task_holder`. Templated only on `CFG` so all restart/stop logic is
/// instantiated once per configuration, regardless of how many different factory or context types
/// are in use. Not intended to be used directly — use `task_holder` instead.
///
template < task_config CFG = task_default_cfg >
struct _task_holder_base : schedulable
{
        using _task_t = task< void, CFG >;

        _task_holder_base( _task_holder_base const& )            = delete;
        _task_holder_base& operator=( _task_holder_base const& ) = delete;
        _task_holder_base( _task_holder_base&& )                 = delete;
        _task_holder_base& operator=( _task_holder_base&& )      = delete;

        explicit _task_holder_base( task_core& core )
          : _core( core )
        {
        }

        /// Schedule the first task run. The task does not start immediately; it runs on the next
        /// `task_core` tick.
        void start()
        {
                _core.reschedule( *this );
        }

        /// Signal the running task to stop and return a sender that completes with
        /// `set_value_t()` once the loop exits.
        _ll_sender< set_value_t() > stop()
        {
                _stop_src.request_stop();
                return _done_src.schedule();
        }

protected:
        /// Override in the concrete subclass to produce the next task instance.
        virtual _task_t _make_task() = 0;

private:
        struct _recv
        {
                using receiver_concept = receiver_t;

                _task_holder_base* _holder;

                template < typename... Args >
                void set_value( Args&&... ) noexcept
                {
                        _holder->_core.reschedule( *_holder );
                }

                template < typename E >
                void set_error( E&& ) noexcept
                {
                        _holder->_core.reschedule( *_holder );
                }

                void set_stopped() noexcept
                {
                        _holder->_core.reschedule( *_holder );
                }

                [[nodiscard]] stop_token_env< inplace_stop_token > get_env() const noexcept
                {
                        return { _holder->_stop_src.get_token() };
                }
        };

        using _op_t = connect_type< _task_t, _recv >;

        void resume() override
        {
                if ( _op.has_value() ) {
                        _op->clear();  // explicitly free the done coroutine frame
                        _op.reset();
                }
                if ( _stop_src.stop_requested() ) {
                        _done_src.set_value();
                } else {
                        _op.emplace( _make_task().connect( _recv{ this } ) );
                        _op->start();
                }
        }

        task_core&                        _core;
        inplace_stop_source               _stop_src;
        broadcast_source< set_value_t() > _done_src;
        std::optional< _op_t >            _op;
};

/// Owns a `task<void, CFG>` that restarts automatically, it is provided with a factory to create
/// new task instances, and a context that is passed to the factory on each run.
///
/// Restart policy:
/// - Restarts on `set_value`, `set_error`, or `set_stopped` from the inner task.
/// - Exits the loop only when `stop()` has been called AND the next completion arrives
///   (regardless of which signal it is).
///
/// Template parameters:
/// - `CFG`     — task configuration, default `task_default_cfg`
/// - `Ctx`     — task context type satisfying `task_context`, default `task_ctx`
/// - `Factory` — callable `(Ctx&) -> task<void, CFG>`; deduced via CTAD
///
/// Precondition: must not be destructed before the sender returned by `stop()` completes.
///
template <
    task_config  CFG = task_default_cfg,
    task_context Ctx = task_ctx,
    typename Factory = void >
struct task_holder : _task_holder_base< CFG >
{
        using base = _task_holder_base< CFG >;

        task_holder( Ctx& ctx, Factory factory )
          : _task_holder_base< CFG >( get_task_core( ctx ) )
          , _ctx( ctx )
          , _factory( std::move( factory ) )
        {
        }

        using base::start;
        using base::stop;

private:
        task< void, CFG > _make_task() override
        {
                return _factory( _ctx );
        }

        Ctx&    _ctx;
        Factory _factory;
};

/// Deduction guide: `task_holder h{ ctx, factory }` deduces `task_holder<task_default_cfg, Ctx,
/// F>`.
template < task_context Ctx, typename Factory >
task_holder( Ctx&, Factory ) -> task_holder< task_default_cfg, Ctx, Factory >;

/// -------------------------------------------------------------------------------

struct _wait_until_stopped
{
        using sender_concept = sender_t;

        using completion_signatures = ecor::completion_signatures< set_value_t() >;

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
                static_assert(
                    receiver_for< R, _wait_until_stopped >,
                    "Receiver does not satisfy the requirements for wait_until_stopped's completion signatures" );
                return _op{ std::move( receiver ) };
        }
};

/// CPO for creating a sender that completes when the stop token in the receiver's environment is
/// triggered. The sender completes with set_value_t() when the stop token is triggered. If the
/// stop token is never triggered, the sender will never complete.
[[maybe_unused]]
static inline _wait_until_stopped wait_until_stopped;

/// -------------------------------------------------------------------------------

/// Awaiter used by ecor::suspend. Suspends the current task coroutine and re-queues it at the back
/// of the task_core's ready list, allowing other ready tasks to run first.
///
/// If the stop token in the task promise is already triggered at suspension time, the task
/// completes with set_stopped() and does not resume. No callbacks or allocations are involved;
/// code that never uses this type incurs zero overhead.
struct _suspend_awaiter
{
        [[nodiscard]] bool await_ready() const noexcept
        {
                return false;
        }

        template < typename Promise >
        void await_suspend( std::coroutine_handle< Promise > h ) noexcept
        {
                auto& p = h.promise();
                if ( p.token.stop_requested() ) {
                        p.invoke_set_stopped();
                        return;
                }
                p.core.reschedule( p );
        }

        void await_resume() noexcept
        {
        }
};

/// Yields the current task to the scheduler, placing it at the back of the ready queue so
/// that other tasks get a chance to run before this one resumes.
///
/// If a stop has already been requested (via the stop token propagated from the receiver)
/// at the point of suspension, the task completes with set_stopped() and does not resume.
///
/// Usage inside a task coroutine:
///   co_await ecor::suspend;
///
/// Zero overhead when not used: no callbacks, no allocations, no extra per-task state.
[[maybe_unused]]
static inline _suspend_awaiter suspend;

/// -------------------------------------------------------------------------------
/// async_arena — asynchronous reference-counted lifetime management

template < typename T >
concept _has_member_async_destroy = requires( T& obj ) {
        { obj.async_destroy() } -> sender;
};

template < typename Ctx, typename T >
concept _has_adl_async_destroy = requires( Ctx& ctx, T& obj ) {
        { async_destroy( ctx, obj ) } -> sender;
};

/// CPO for asynchronous destruction of managed objects. Used by the library to destroy objects with
/// asynchronous destruction.
///
/// Users specialize this by providing either:
///  - A member function: `auto async_destroy(task_context auto& ctx)`
///  - An ADL free function: `auto async_destroy(task_context auto& ctx, T& obj)`
///
/// The returned type must be a sender.
///
struct _async_destroy_t
{
        template < typename Ctx, typename T >
        ecor::sender auto operator()( Ctx& ctx, T& obj ) const
        {
                if constexpr ( _has_adl_async_destroy< Ctx, T > )
                        return async_destroy( ctx, obj );
                else {
                        static_assert( _has_member_async_destroy< T > );
                        return obj.async_destroy();
                }
        }
};
inline constexpr _async_destroy_t async_destroy{};

enum class _cb_state : uint8_t
{
        alive,
        queued,
        destroying,
};

struct _async_arena_cb_base : zll::ll_base< _async_arena_cb_base >
{
        virtual void start_destroy()  = 0;
        virtual void finish_cleanup() = 0;

protected:
        ~_async_arena_cb_base() = default;
};

struct _async_arena_core_base : schedulable
{
        explicit _async_arena_core_base( task_core& core ) noexcept
          : _core( core )
        {
        }

        void _enqueue_destroy( _async_arena_cb_base& cb ) noexcept
        {
                _destroy_queue.link_back( cb );
                if ( !_active && zll::detached( static_cast< schedulable& >( *this ) ) )
                        _core.reschedule( *this );
        }

        void _on_destroy_complete() noexcept
        {
                ECOR_ASSERT( _active );
                ECOR_ASSERT( zll::detached( static_cast< schedulable& >( *this ) ) );
                _core.reschedule( *this );
        }

        void resume() override
        {
                if ( _active ) {
                        _active->finish_cleanup();
                        _active = nullptr;
                }

                if ( !_destroy_queue.empty() ) {
                        _active = &_destroy_queue.take_front();
                        _active->start_destroy();
                } else if ( _stopped && _alive_list.empty() ) {
                        _done_src.set_value();
                }
        }

        task_core&                           _core;
        zll::ll_list< _async_arena_cb_base > _alive_list;
        zll::ll_list< _async_arena_cb_base > _destroy_queue;
        _async_arena_cb_base*                _active  = nullptr;
        bool                                 _stopped = false;
        broadcast_source< set_value_t() >    _done_src;

        struct _destroy_receiver
        {
                using receiver_concept = receiver_t;

                _async_arena_core_base* _core;

                void set_value( auto&&... ) noexcept
                {
                        _core->_on_destroy_complete();
                }

                void set_error( auto&& ) noexcept
                {
                        _core->_on_destroy_complete();
                }

                void set_stopped() noexcept
                {
                        _core->_on_destroy_complete();
                }
        };
};

struct _async_arena_cb_counted : _async_arena_cb_base
{
        explicit _async_arena_cb_counted( _async_arena_core_base& arena ) noexcept
          : _arena( arena )
        {
        }

        void add_ref() noexcept
        {
                ECOR_ASSERT( _refs > 0 );
                ECOR_ASSERT( _state == _cb_state::alive );
                ++_refs;
        }

        void release() noexcept
        {
                ECOR_ASSERT( _refs > 0 );
                if ( --_refs == 0 ) {
                        _state = _cb_state::queued;
                        _arena._enqueue_destroy( *this );
                }
        }

        _async_arena_core_base& _arena;
        uint32_t                _refs  = 1;
        _cb_state               _state = _cb_state::alive;

protected:
        ~_async_arena_cb_counted() = default;
};

template < typename Ctx, typename Mem >
struct async_arena;

template < typename Ctx, typename Mem >
struct _async_arena_core : _async_arena_core_base
{
        _async_arena_core( Ctx& ctx, Mem& mem ) noexcept
          : _async_arena_core_base( get_task_core( ctx ) )
          , _ctx( ctx )
          , _mem( mem )
        {
        }

        Ctx& _ctx;
        Mem& _mem;
};

template < typename T, typename Ctx, typename Mem >
struct _async_arena_control_block final : _async_arena_cb_counted
{
        using _destroy_sender_t =
            decltype( ecor::async_destroy( std::declval< Ctx& >(), std::declval< T& >() ) );

        using _destroy_receiver = _async_arena_core_base::_destroy_receiver;
        using _destroy_op_t     = connect_type< _destroy_sender_t, _destroy_receiver >;

        _destroy_op_t* _destroy_op = nullptr;
        T              _object;

        template < typename... Args >
        _async_arena_control_block( _async_arena_core< Ctx, Mem >& arena, Args&&... a )
          : _async_arena_cb_counted( arena )
          , _object( (Args&&) a... )
        {
        }

        void start_destroy() override
        {
                ECOR_ASSERT( _state == _cb_state::queued );
                _state = _cb_state::destroying;

                auto& arena = static_cast< _async_arena_core< Ctx, Mem >& >( _arena );

                using op_t = _destroy_op_t;

                auto  s = ecor::async_destroy( arena._ctx, _object );
                void* p = ecor::allocate( arena._mem, sizeof( op_t ), alignof( op_t ) );
                ECOR_ASSERT( p );
                _destroy_op =
                    ::new ( p ) op_t( std::move( s ).connect( _destroy_receiver{ &_arena } ) );
                _destroy_op->start();
        }

        void finish_cleanup() override
        {
                auto& arena = static_cast< _async_arena_core< Ctx, Mem >& >( _arena );

                using op_t = _destroy_op_t;
                _destroy_op->~op_t();
                ecor::deallocate( arena._mem, _destroy_op, sizeof( op_t ), alignof( op_t ) );
                _destroy_op = nullptr;
                auto& mem   = arena._mem;
                this->~_async_arena_control_block();
                ecor::deallocate(
                    mem,
                    this,
                    sizeof( _async_arena_control_block ),
                    alignof( _async_arena_control_block ) );
        }
};

/// Reference-counted smart pointer to an object managed by an `async_arena`. When the last
/// `async_ptr` is dropped, the async destroy protocol is initiated — the object is not immediately
/// destructed but instead queued for asynchronous cleanup via the arena.
///
/// Copyable, movable, nullable. Dereferenceable via `*` and `->`.
///
template < typename T, typename Ctx, typename Mem >
struct async_ptr
{
        using _cb_t = _async_arena_control_block< T, Ctx, Mem >;

        /// Default constructor creates a null `async_ptr`.
        async_ptr() noexcept = default;

        /// Constructing from nullptr creates a null `async_ptr`.
        async_ptr( std::nullptr_t ) noexcept
        {
        }

        /// Copy constructor and copy assignment operator increment the reference count.
        async_ptr( async_ptr const& o ) noexcept
          : _cb( o._cb )
        {
                if ( _cb )
                        _cb->add_ref();
        }

        /// Move constructor and move assignment operator transfer ownership without modifying the
        /// reference count.
        async_ptr( async_ptr&& o ) noexcept
          : _cb( o._cb )
        {
                o._cb = nullptr;
        }

        /// Copy assignment operator: release the current object (if any), copy the control block
        /// pointer from the other `async_ptr`, and increment the reference count if the new control
        /// block is not null.
        async_ptr& operator=( async_ptr const& o ) noexcept
        {
                if ( this != &o ) {
                        _reset();
                        _cb = o._cb;
                        if ( _cb )
                                _cb->add_ref();
                }
                return *this;
        }

        /// Move assignment operator: release the current object (if any), transfer the control
        /// block pointer from the other `async_ptr`, and set the other `async_ptr` to null without
        /// modifying the reference count.
        async_ptr& operator=( async_ptr&& o ) noexcept
        {
                if ( this != &o ) {
                        _reset();
                        _cb   = o._cb;
                        o._cb = nullptr;
                }
                return *this;
        }

        /// Destructor releases the current object (if any) by decrementing the reference count and
        /// initiating async destruction if the count reaches zero.
        ~async_ptr()
        {
                _reset();
        }

        /// Explicit bool conversion operator returns true if the `async_ptr` is non-null.
        explicit operator bool() const noexcept
        {
                return _cb != nullptr;
        }

        /// Dereference operators return the managed object. Precondition: the `async_ptr` must be
        /// non-null.
        T& operator*() const noexcept
        {
                ECOR_ASSERT( _cb );
                return _cb->_object;
        }

        /// Arrow operator returns a pointer to the managed object. Precondition: the `async_ptr`
        /// must be non-null.
        T* operator->() const noexcept
        {
                ECOR_ASSERT( _cb );
                return &_cb->_object;
        }

        /// Get the raw pointer to the managed object, or nullptr if the `async_ptr` is null.
        T* get() const noexcept
        {
                return _cb ? &_cb->_object : nullptr;
        }

        /// Reset the `async_ptr` to null, releasing the current object (if any) by decrementing the
        /// reference count and initiating async destruction if the count reaches zero.
        void reset() noexcept
        {
                _reset();
        }

        /// Comparison operators compare the control block pointers for equality. Two `async_ptr`s
        /// are equal if they point to the same control block (i.e., manage the same object), and
        /// not equal otherwise.
        friend bool operator==( async_ptr const& a, async_ptr const& b ) noexcept
        {
                return a._cb == b._cb;
        }

        /// Comparison operators compare the control block pointers for inequality. Two `async_ptr`s
        /// are not equal if they point to different control blocks (i.e., manage different
        /// objects), and equal otherwise.
        friend bool operator!=( async_ptr const& a, async_ptr const& b ) noexcept
        {
                return a._cb != b._cb;
        }

        /// Comparison operators with nullptr compare the control block pointer to null. An
        /// `async_ptr` is equal to nullptr if its control block pointer is null.
        friend bool operator==( async_ptr const& a, std::nullptr_t ) noexcept
        {
                return a._cb == nullptr;
        }

        /// Comparison operators with nullptr compare the control block pointer to null. An
        /// `async_ptr` is not equal to nullptr if its control block pointer is not null.
        friend bool operator!=( async_ptr const& a, std::nullptr_t ) noexcept
        {
                return a._cb != nullptr;
        }

private:
        template < typename, typename >
        friend struct async_arena;

        explicit async_ptr( _cb_t* cb ) noexcept
          : _cb( cb )
        {
        }

        void _reset() noexcept
        {
                if ( _cb ) {
                        _cb->release();
                        _cb = nullptr;
                }
        }

        _cb_t* _cb = nullptr;
};

/// Async arena. Creates `async_ptr` instances and provides graceful shutdown via `async_destroy()`.
/// This allows construction of datasets of type with asynchronous destruction requirements, and
/// ensures that all pending destructions complete before shutdown.
///
/// For any type managed by the arena, we assume that async_destroy CPO can be used and guaranteee
/// that it's get called before standard destructor of the object.
///
/// Arena uses reference-counted pointers to handle lifetime of the objects. When the last
/// `async_ptr` to an object is destroyed, the arena initiates asynchronous destruction of the
/// object by enqueuing it for cleanup in provided task context object. The arena keeps track of all
/// alive objects and pending destructions, and provides a sender that completes when all pending
/// destructions have finished after shutdown is initiated.
///
/// Any memory is allocated via `Mem` memory resource provided to the arena. Arena allocates newly
/// created objects and all internal bookkeeping structures (control blocks, destroy operations)
/// from the provided memory resource, and deallocates them when they are destroyed.
///
/// The resulting operation state from sender returned by objects `async_destroy()` is stored in
/// memory blocked allocated from the arena's memory resource, and is properly destroyed and
/// deallocated when the destruction completes.
///
/// The arena itself is not thread-safe and must be used from a single task context.
///
/// Precondition: `async_destroy()` sender must complete before the arena is destroyed.
///
template < typename Ctx, typename Mem >
struct async_arena
{
        /// Construct an async arena with the provided context and memory resource.
        /// `async_ptr` type for an object of type `T` managed by this arena.
        template < typename T >
        using ptr_type = async_ptr< T, Ctx, Mem >;

        /// Construct an async arena with the provided context and memory resource.
        async_arena( Ctx& ctx, Mem& mem ) noexcept
          : _core( ctx, mem )
        {
        }

        async_arena( async_arena const& )            = delete;
        async_arena& operator=( async_arena const& ) = delete;
        async_arena( async_arena&& )                 = delete;
        async_arena& operator=( async_arena&& )      = delete;

        /// Create a new managed object of type T. Returns an `async_ptr<T,Ctx,Mem>` with a
        /// reference count of 1. Returns a null `async_ptr` if allocation fails.
        template < typename T, typename... Args >
        async_ptr< T, Ctx, Mem > make( Args&&... args )
        {
                ECOR_ASSERT( !_core._stopped );
                using cb_t = _async_arena_control_block< T, Ctx, Mem >;
                void* p    = ecor::allocate( _core._mem, sizeof( cb_t ), alignof( cb_t ) );
                if ( !p )
                        return { nullptr };
                auto* cb = ::new ( p ) cb_t( _core, (Args&&) args... );
                _core._alive_list.link_back( *cb );
                return async_ptr< T, Ctx, Mem >{ cb };
        }

        /// Return a reference to the context provided at construction.
        Ctx& ctx() noexcept
        {
                return _core._ctx;
        }

        /// Signal that no new objects will be created and return a sender that completes with
        /// `set_value_t()` when all pending async destructions have finished.
        _ll_sender< set_value_t() > async_destroy() noexcept
        {
                _core._stopped = true;
                if ( _core._alive_list.empty() && _core._destroy_queue.empty() &&
                     zll::detached( static_cast< schedulable& >( _core ) ) && !_core._active )
                        _core._core.reschedule( _core );
                return _core._done_src.schedule();
        }

private:
        _async_arena_core< Ctx, Mem > _core;
};

/// -------------------------------------------------------------------------------
/// Transaction controller

template < typename T >
struct _trnx_vtable_mixin;

template < signature... S >
        requires( !_contains_type< set_stopped_t(), S... >::value )
struct _trnx_vtable_mixin< completion_signatures< S... > >
{
        using type = _vtable_mixin< S... >;
};

template < signature... S >
        requires( _contains_type< set_stopped_t(), S... >::value )
struct _trnx_vtable_mixin< completion_signatures< S... > >
{
        using type = _vtable_mixin< S..., _get_stopped_t() >;
};

template < typename T >
using _trnx_vtable_mixin_t =
    typename _trnx_vtable_mixin< _sender_completions_t< T, empty_env > >::type;

/// Type-erased linked-list node for a pending transaction. Holds user data of type T and a vtable
/// for dispatching completion signals (set_value, set_error, set_stopped) to the concrete receiver
/// without knowing its type. Automatically unlinks from its list on destruction.
///
/// - `T`: User-defined transaction data type. Must provide completion signatures via
/// `get_completion_signatures` CPO.
template < typename T >
struct trnx_entry : _trnx_vtable_mixin_t< T >, zll::ll_base< trnx_entry< T > >
{
        using base    = _trnx_vtable_mixin_t< T >;
        using _vtable = typename base::_vtable;
        static constexpr bool _has_stop_sig =
            _sigs_contains_set_stopped< _sender_completions_t< T, empty_env > >;

        T data;

        /// Construct a transaction entry with the given data. The _tag parameter is used to setup
        /// vtable correctly.
        template < typename Derived >
        trnx_entry( _tag< Derived >, T data )
          : base( _tag< Derived >{} )
          , data( std::move( data ) )
        {
        }

        template < typename... Args >
        void set_value( Args&&... args )
        {
                this->_set_value( (Args&&) args... );
        }

        template < typename E >
        void set_error( E&& e )
        {
                this->_set_error( (E&&) e );
        }

        void set_stopped()
        {
                this->_set_stopped();
        }

        bool get_stopped() const noexcept
        {
                return this->get_stopped();
        }
};
template < typename T, typename R >
struct _trnx_controller_op : trnx_entry< T >, R
{
        using _vtable                       = typename trnx_entry< T >::_vtable;
        static constexpr bool _has_stop_sig = trnx_entry< T >::_has_stop_sig;

        using R::set_error;
        using R::set_stopped;
        using R::set_value;

        _trnx_controller_op( T data, R r, zll::ll_list< trnx_entry< T > >& pending )
          : trnx_entry< T >{ _tag< _trnx_controller_op >{}, std::move( data ) }
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
        zll::ll_list< trnx_entry< T > >& _pending;
};

template < typename T >
struct _trnx_controller_sender
{
        using sender_concept = sender_t;

        template < typename Env >
        constexpr auto get_completion_signatures( Env&& e ) noexcept
        {
                return ecor::get_completion_signatures( val, (Env&&) e );
        }

        template < receiver R >
        auto connect( R receiver ) &&
        {
                static_assert(
                    receiver_for< R, _trnx_controller_sender >,
                    "Receiver does not satisfy the requirements for the transaction sender's completion signatures" );

                return _trnx_controller_op< T, R >{
                    std::move( val ), std::move( receiver ), _pending };
        }

        T                                val;
        zll::ll_list< trnx_entry< T > >& _pending;
};

/// ISR-safe 4-cursor circular buffer for managing in-flight transactions.
/// Cursors are: enqueue -> tx -> rx -> deliver
///   - enqueue - tx: Tasks ready to be transmitted (main loop pushes new entries here).
///   - tx - rx: Transactions that have been transmitted and are awaiting a reply.
///   - rx - deliver: Transactions that have received a reply and are awaiting to be taken out (main
///     loop delivers completion to the receiver).
///
/// Cursors use std::atomic for safe cross-context access (main loop vs ISR).
///
/// - `T`: Element type (must be trivial).
/// - `N`: Buffer capacity (must be a power of 2, < 65536).
template < typename T, size_t N >
struct trnx_circular_buffer
{
        static_assert( std::has_single_bit( N ), "Size of circular buffer must be a power of 2" );
        static_assert(
            N < std::numeric_limits< uint16_t >::max(),
            "Size of circular buffer must be less than 65536" );
        static_assert( std::is_trivial_v< T > );

        bool full() const noexcept
        {
                return ( enqueue - deliver ) == N;
        }

        void push( T val ) noexcept
        {
                _data[enqueue++ % N] = val;
        }

        bool empty() const noexcept
        {
                return deliver == enqueue;
        }

        bool has_tx() const noexcept
        {
                return tx != enqueue;
        }

        T& tx_front() noexcept
        {
                return _data[tx % N];
        }

        void tx_done() noexcept
        {
                tx++;
        }

        bool has_rx() const noexcept
        {
                return rx != tx;
        }

        T& rx_front() noexcept
        {
                return _data[rx % N];
        }

        void rx_done() noexcept
        {
                rx++;
        }

        bool has_deliver() const noexcept
        {
                return deliver != rx;
        }

        T& deliver_front() noexcept
        {
                return _data[deliver % N];
        }

        void pop() noexcept
        {
                deliver++;
        }

        std::atomic< uint16_t > deliver = 0;
        std::atomic< uint16_t > rx      = 0;
        std::atomic< uint16_t > tx      = 0;
        std::atomic< uint16_t > enqueue = 0;

private:
        T _data[N];
};

/// Source for scheduling transactions in a request-reply protocol (e.g. UART, SPI, I²C)
/// where multiple transactions can be in flight simultaneously.
/// The source will get user-defined type T as transaction payload and will return senders that
/// complete when the transaction is completed by the driver.
///
/// The source maintains a linked list of pending transactions. Once a transaction is taken by the
/// driver, it is removed from the list and handed off to the driver, which is responsible for
/// managing it until completion.
///
/// On completion, the driver calls set_value/set_error/set_stopped on the transaction entry. The
/// exact API is specified by the user-provided transaction data type T.
///
/// Architecture of the transaction abstraction:
///   - trnx_controller_source<T>  — schedules transactions, returns senders (this type)
///   - trnx_entry<T>              — type-erased linked-list node holding user data T
///   - trnx_circular_buffer<T, N> — ISR-safe 4-cursor ring buffer for in-flight management
///
/// Usage:
///   1. Call schedule(data) to get a sender representing the transaction.
///   2. Connect the sender to a receiver and start().
///   3. In the driver's tick loop, call query_next_trnx() to retrieve pending entries.
///   4. Move entries into a circular buffer or process them directly.
///   5. When the reply arrives, call set_value/set_error/set_stopped on the entry to complete the
///   sender.
///
/// Stop semantics: stop tokens are checked only for *pending* transactions (still in the source's
/// linked list). Once a transaction is moved outside, whenever it can be cancelled or not depends
/// solely on the driver.
///
/// - `T`: User-defined transaction data type providing get_completion_signatures.
template < typename T >
struct trnx_controller_source
{
        using sender_type                   = _trnx_controller_sender< T >;
        static constexpr bool _has_stop_sig = trnx_entry< T >::_has_stop_sig;

        /// Schedule a new transaction with the given user data. Returns a sender that completes
        /// when the transaction is completed by the driver.
        sender_type schedule( T val )
        {
                return { std::move( val ), _pending_tx };
        }

        /// Retrieve the next pending transaction entry for processing by the driver. Returns
        /// nullptr if there are no pending transactions. Any transaction has been cancelled
        /// (stop token triggered) while it was pending, it will be removed from the list,
        /// set_stopped() will be called on it, and it will not be returned.
        trnx_entry< T >* query_next_trnx()
        {
                if constexpr ( _has_stop_sig ) {
                        _pending_tx.remove_if( []( trnx_entry< T >& tx ) {
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
        zll::ll_list< trnx_entry< T > > _pending_tx;
};

}  // namespace ecor
