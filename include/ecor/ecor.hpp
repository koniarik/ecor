#pragma once

#include <coroutine>
#include <optional>

namespace ecor
{

template < typename A, typename B >
struct varptr
{
        varptr( std::nullptr_t ) noexcept
        {
        }

        varptr( A* ptr ) noexcept
          : ptr_( 0x01 + ( reinterpret_cast< std::intptr_t >( ptr ) << 1 ) )
        {
        }

        varptr( B* ptr ) noexcept
          : ptr_( 0x00 + ( reinterpret_cast< std::intptr_t >( ptr ) << 1 ) )
        {
        }

        varptr( varptr const& other ) noexcept            = default;
        varptr& operator=( varptr const& other ) noexcept = default;
        varptr( varptr&& other ) noexcept                 = default;
        varptr& operator=( varptr&& other ) noexcept      = default;
        ~varptr() noexcept                                = default;

        operator bool() const noexcept
        {
                return ptr_ != 0x00;
        }

        bool operator==( varptr const& other ) const noexcept
        {
                return ptr_ == other.ptr_;
        }

        std::intptr_t get() const noexcept
        {
                return ptr_;
        }

private:
        std::intptr_t ptr_ = 0x00;
};

template < typename U, typename A, typename B >
U* get_if( varptr< A, B > const& ptr ) noexcept
{
        if constexpr ( std::is_same_v< U, A > ) {
                if ( ptr.get() & 0x01 )
                        return reinterpret_cast< U* >( ( ptr.get() >> 1 ) );
        } else {
                static_assert( std::is_same_v< U, B >, "U must be either A or B" );
                if ( !( ptr.get() & 0x00 ) )
                        return reinterpret_cast< U* >( ( ptr.get() >> 1 ) );
        }
        return nullptr;
}

template < typename T >
struct event;

template < typename T >
struct event_core;

template < typename T >
using event_ptr = varptr< event< T >, event_core< T > >;

template < typename T >
struct event_core
{
        void link_event( event< T >& e )
        {
                if ( auto* f = get_if< event< T > >( first ) )
                        f->prev_ = &e;
                e.next_ = first;
                first   = &e;
        }

        void raise( T& value )
        {
                auto* e = get_if< event< T > >( first );
                while ( e ) {
                        event< T >* next = get_if< event< T > >( e->next_ );
                        e->raise( value );
                        e = next;
                }
        }

        event_ptr< T > first = nullptr;
};

template < typename T >
struct event_source
{
        using value_type = T;

        event< T > get()
        {
                return event< T >( core_ );
        }

        void raise( T& value )
        {
                core_.raise( value );
        }

private:
        event_core< T > core_;
};

template < typename T >
struct event
{
        using ptr_t      = varptr< event< T >, event_core< T > >;
        using value_type = T;

        event( event_core< T >& src )
          : src_( src )
        {
        }

        event( event const& )            = delete;
        event& operator=( event const& ) = delete;

        [[nodiscard]] bool await_ready() const noexcept
        {
                return false;
        }

        void await_suspend( std::coroutine_handle<> h ) noexcept
        {
                h_ = h;
                src_.link_event( *this );
        }

        T& await_resume() const
        {
                return *value_;
        }

        ~event()
        {
                unlink();
        }

private:
        friend struct event_core< T >;

        void unlink() noexcept
        {
                if ( auto* pp = get_if< event< T > >( prev_ ) )
                        pp->next_ = next_;
                else if ( auto* np = get_if< event_core< T > >( prev_ ) )
                        np->first = next_;

                if ( auto* pp = get_if< event< T > >( next_ ) )
                        pp->prev_ = prev_;
        }

        void raise( T& value )
        {
                value_ = &value;
                if ( h_ )
                        h_.resume();
        }

        ptr_t next_ = nullptr;
        ptr_t prev_ = nullptr;

        event_core< T >&              src_;
        std::coroutine_handle< void > h_;
        T*                            value_ = nullptr;
};

template < typename T >
struct task
{
        using value_type = T;

        struct promise_type
        {
                using value_type = T;

                task get_return_object()
                {
                        return { std::coroutine_handle< promise_type >::from_promise( *this ) };
                }

                std::suspend_always initial_suspend() noexcept
                {
                        return {};
                }

                std::suspend_never final_suspend() noexcept
                {
                        return {};
                }

                void unhandled_exception() noexcept
                {
                        // XXX: impl
                }
        };

        task( std::coroutine_handle< promise_type > handle )
          : h_( handle )
        {
        }

        task( task const& )            = delete;
        task& operator=( task const& ) = delete;

        void resume()
        {
                if ( h_ )
                        h_.resume();
        }

        [[nodiscard]] bool done() const noexcept
        {
                return !h_ || h_.done();
        }

        ~task()
        {
                if ( h_ )
                        h_.destroy();
        }

private:
        std::coroutine_handle< promise_type > h_;
};

}  // namespace ecor
