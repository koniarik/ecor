#pragma once

#include <concepts>
#include <coroutine>
#include <cstddef>
#include <iterator>
#include <optional>
#include <zll.hpp>

namespace ecor
{

template < typename T >
concept memory_resource = requires( T a, std::size_t bytes, std::size_t align, void* p ) {
        { allocate( a, bytes, align ) } -> std::same_as< void* >;
        { deallocate( a, p, bytes, align ) } -> std::same_as< void >;
};

template < typename T >
void* allocate( T& mem, std::size_t bytes, std::size_t align )
{
        return mem.allocate( bytes, align );
}

template < typename T >
void deallocate( T& mem, void* p, std::size_t bytes, std::size_t align )
{
        return mem.deallocate( p, bytes, align );
}

template < typename T >
struct event_error
{
        virtual void set_error( T error ) = 0;
};

template <>
struct event_error< void >
{
};

template < typename T, typename E >
struct event : event_error< E >, zll::ll_base< event< T, E > >
{
        virtual void set_value( T value ) = 0;
};

template < typename T, typename E >
struct event_core
{
        using unit_type = event< T, E >;

        template < typename V >
        void set_value( V&& value )
        {
                auto iter = list.begin();
                auto e    = list.end();
                while ( iter != e ) {
                        auto n = iter++;
                        n->set_value( (V&&) value );
                }
        }

        template < typename E1 >
        void set_error( E1&& err )
        {
                auto iter = list.begin();
                auto e    = list.end();
                while ( iter != e ) {
                        auto n = iter++;
                        n->set_error( (E1&&) err );
                }
        }


        zll::ll_list< event< T, E > > list;
};

template < typename T, typename E >
struct event_sender;


template < typename T, typename E >
struct event_source
{
        using value_type = T;

        event_sender< T, E > schedule()
        {
                return ( core_ );
        }

        template < typename E1 >
                requires( std::convertible_to< E1, E > )
        void set_error( E1 err )
        {
                core_.set_error( std::move( err ) );
        }

        void set_value( T value )
        {
                core_.set_value( std::move( value ) );
        }


private:
        event_core< T, E > core_;
};

template < typename C >
struct _awaitable_base : C::unit_type
{
        _awaitable_base( C& core )
          : core_( core )
        {
        }

        [[nodiscard]] bool await_ready() const noexcept
        {
                return false;
        }

        void await_suspend( std::coroutine_handle<> h ) noexcept
        {
                core_.list.link_back( *this );
                h_ = h;
        }

        C&                      core_;
        std::coroutine_handle<> h_ = nullptr;
};

template < typename T, typename E >
struct event_awaitable : _awaitable_base< event_core< T, E > >
{
        using base = _awaitable_base< event_core< T, E > >;

        event_awaitable( event_core< T, E >& core )
          : base( core )
        {
        }

        std::variant< T, E > await_resume() const
        {
                return std::move( res_ );
        }

        void set_value( T value ) override
        {
                res_ = std::move( value );
                base::h_.resume();
        }

        void set_error( E error ) override
        {
                res_ = std::move( error );
                base::h_.resume();
        }

private:
        std::variant< T, E > res_;
};

template < typename T >
struct event_awaitable< T, void > : _awaitable_base< event_core< T, void > >
{
        using base = _awaitable_base< event_core< T, void > >;

        event_awaitable( event_core< T, void >& core )
          : base( core )
        {
        }

        T await_resume() const
        {
                return std::move( res_ );
        }

        void set_value( T value ) override
        {
                res_ = std::move( value );
                base::h_.resume();
        }

private:
        T res_;
};

template < typename T, typename E, typename R >
struct _event_op_impl : event< T, E >
{
        using core_type = event_core< T, E >;

        R receiver_;

        _event_op_impl( R receiver )
          : receiver_( std::move( receiver ) )
        {
        }

        void set_value( T value ) override
        {
                receiver_.set_value( std::move( value ) );
        }

        void set_error( E err ) override
        {
                receiver_.set_error( std::move( err ) );
        }
};

template < typename T, typename R >
struct _event_op_impl< T, void, R > : event< T, void >
{
        using core_type = event_core< T, void >;

        R receiver_;

        _event_op_impl( R receiver )
          : receiver_( std::move( receiver ) )
        {
        }

        void set_value( T value ) override
        {
                receiver_.set_value( std::move( value ) );
        }
};

template < typename OpImpl >
struct _op
{
        using core_type = typename OpImpl::core_type;

        _op( core_type& core, auto receiver )
          : impl_( std::move( receiver ) )
          , core_( core )
        {
        }

        void start()
        {
                core_.list.link_back( impl_ );
        }

private:
        OpImpl     impl_;
        core_type& core_;
};

template < typename T, typename E >
struct event_sender
{
        event_sender( event_core< T, E >& core )
          : core_( core )
        {
        }

        template < typename R >
        _op< _event_op_impl< T, E, R > > connect( R receiver )
        {
                return { core_, std::move( receiver ) };
        }

        event_core< T, E >& core_;
};

template < typename E >
struct notif : event_error< E >, zll::ll_base< notif< E > >
{
        virtual void set_value() = 0;
};

template < typename E >
struct notify_core
{
        using unit_type = notif< E >;

        void set_value()
        {
                auto iter = list.begin();
                auto e    = list.end();
                while ( iter != e ) {
                        auto n = iter++;
                        n->set_value();
                }
        }

        template < typename E1 >
        void set_error( E1&& err )
        {
                auto iter = list.begin();
                auto e    = list.end();
                while ( iter != e ) {
                        auto n = iter++;
                        n->set_error( (E1&&) err );
                }
        }

        zll::ll_list< notif< E > > list;
};

template < typename Tag, typename E >
struct notify_sender;

template < typename Tag, typename E = void >
struct notify_source
{
        notify_sender< Tag, E > schedule()
        {
                return ( core_ );
        }

        template < typename E1 >
                requires( std::convertible_to< E1, E > )
        void set_error( E1 err )
        {
                core_.set_error( std::move( err ) );
        }

        void set_value()
        {
                core_.set_value();
        }

private:
        notify_core< E > core_;
};

template < typename Tag, typename E >
struct notify_awaitable : _awaitable_base< notify_core< E > >
{
        using base = _awaitable_base< notify_core< E > >;

        notify_awaitable( notify_core< E >& core )
          : base( core )
        {
        }

        std::optional< E > await_resume() const
        {
                return std::move( res_ );
        }

        void set_value() override
        {
                base::h_.resume();
        }

        void set_error( E error ) override
        {
                res_ = std::move( error );
                base::h_.resume();
        }

private:
        std::optional< E > res_;
};

template < typename Tag >
struct notify_awaitable< Tag, void > : _awaitable_base< notify_core< void > >
{
        using base = _awaitable_base< notify_core< void > >;

        notify_awaitable( notify_core< void >& core )
          : base( core )
        {
        }

        void await_resume() const
        {
        }

        void set_value() override
        {
                base::h_.resume();
        }
};

template < typename R, typename E >
struct _notif_op_impl : notif< E >
{
        R receiver_;

        _notif_op_impl( R receiver )
          : receiver_( std::move( receiver ) )
        {
        }

        void set_value() override
        {
                receiver_.set_value();
        }

        void set_error( E err ) override
        {
                receiver_.set_error( std::move( err ) );
        }
};

template < typename R >
struct _notif_op_impl< R, void > : notif< void >
{
        R receiver_;

        _notif_op_impl( R receiver )
          : receiver_( std::move( receiver ) )
        {
        }

        void set_value() override
        {
                receiver_.set_value();
        }
};

template < typename Tag, typename E >
struct notify_sender
{
        notify_sender( notify_core< E >& core )
          : core_( core )
        {
        }

        template < typename R >
        _op< _notif_op_impl< Tag, E > > connect( R receiver )
        {
                return { core_, std::move( receiver ) };
        }

        notify_core< E >& core_;
};

struct task_allocator
{
        template < memory_resource M >
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

inline task_allocator& task_alloc( task_allocator& t )
{
        return t;
}

inline void* allocate( task_allocator& t, std::size_t const sz, std::size_t const align )
{
        return t.alloc( t.mem, sz, align );
}

inline void deallocate( task_allocator& t, void* p, std::size_t const sz, std::size_t const align )
{
        return t.dealloc( t.mem, p, sz, align );
}

template < typename T >
struct task;

template < typename T >
struct _task_awaiter;

struct _promise_base
{
        static constexpr std::size_t align = alignof( std::max_align_t );
        struct vtable
        {
                task_allocator* mem = nullptr;
        };
        static constexpr std::size_t spacing = align > sizeof( vtable ) ? align : sizeof( vtable );

        static void* alloc( std::size_t sz, task_allocator& mem, vtable& vt )
        {
                sz += spacing;
                void* const vp = allocate( mem, sz, align );
                std::memcpy( vp, (void const*) &vt, sizeof( vt ) );
                return ( (char*) vp ) + spacing;
        }

        static void dealloc( void* const ptr, std::size_t const sz )
        {
                void*  beg = ( (char*) ptr ) - spacing;
                vtable vt;
                std::memcpy( (void*) &vt, beg, sizeof( vt ) );
                deallocate( *vt.mem, beg, sz + spacing, align );
        }

        static void* operator new( std::size_t const sz, auto& mem, auto&&... )
        {
                task_allocator& a = task_alloc( mem );
                vtable          vt{
                             .mem = &a,
                };
                return alloc( sz, a, vt );
        }

        static void operator delete( void* const ptr, std::size_t const sz )
        {
                dealloc( ptr, sz );
        }

        std::suspend_never initial_suspend() noexcept
        {
                return {};
        }

        std::suspend_always final_suspend() noexcept
        {
                if ( next_ )
                        next_.resume();
                // XXX is this bright idea? what about stack size?
                return {};
        }

        std::coroutine_handle<> next_ = nullptr;

        void unhandled_exception() noexcept
        {
        }

        template < typename U, typename E >
        auto await_transform( event_sender< U, E > sender ) noexcept
        {
                return event_awaitable< U, E >( sender.core_ );
        }

        template < typename U, typename E >
        auto await_transform( notify_sender< U, E > sender ) noexcept
        {
                return notify_awaitable< U, E >( sender.core_ );
        }

        template < typename U >
        auto await_transform( task< U >&& other ) noexcept
        {
                return _task_awaiter< U >{ std::move( other ) };
        }
};

template < typename T >
struct _task_awaiter_base
{
        _task_awaiter_base( task< T >&& t )
          : task_( std::move( t ) )
        {
        }

        bool await_ready() const noexcept
        {
                return false;
        }

        bool await_suspend( std::coroutine_handle<> h ) noexcept
        {
                if ( task_.done() )
                        return false;

                task_.promise().next_ = h;

                return true;
        }

protected:
        task< T > task_;
};

template < typename T >
struct _task_awaiter : _task_awaiter_base< T >
{
        using base = _task_awaiter_base< T >;

        T await_resume() const
        {
                auto* p = base::task_.result();
                assert( p );  // TODO: re-think
                return std::move( *p );
        }
};

template <>
struct _task_awaiter< void >;


template < typename T >
struct _promise_type_value
{
        std::optional< T > result_;

        void return_value( T v ) noexcept
        {
                result_ = std::move( v );
        }
};

template <>
struct _promise_type_value< void >
{
        void return_void() noexcept
        {
        }
};

template < typename T >
struct _promise_type : _promise_base, _promise_type_value< T >
{
        using value_type = T;

        task< T > get_return_object()
        {
                return { std::coroutine_handle< _promise_type >::from_promise( *this ) };
        }
};

template < typename T >
struct task
{
        using promise_type = _promise_type< T >;
        using value_type   = T;
        static_assert(
            alignof( promise_type ) <= alignof( std::max_align_t ),
            "Unsupported alignment" );

        task( std::coroutine_handle< promise_type > handle )
          : h_( handle )
        {
        }

        task( task const& )            = delete;
        task& operator=( task const& ) = delete;
        task( task&& other ) noexcept
          : h_( std::exchange( other.h_, nullptr ) )
        {
        }
        task& operator=( task&& other ) noexcept
        {
                if ( this != &other ) {
                        if ( h_ && !h_.done() )
                                h_.destroy();
                        h_ = std::exchange( other.h_, nullptr );
                }
                return *this;
        }

        [[nodiscard]] bool done() const noexcept
        {
                if ( h_ )
                        return h_.done();
                return true;
        }

        T* result() noexcept
        {
                if ( h_ && h_.promise().result_ )
                        return &*h_.promise().result_;
                return nullptr;
        }

        ~task()
        {
                if ( h_ && !h_.done() )
                        h_.destroy();
        }

        // XXX: might be a bad idea to have this public?
        auto& promise() noexcept
        {
                return h_.promise();
        }

private:
        std::coroutine_handle< promise_type > h_;
};

template <>
struct _task_awaiter< void > : _task_awaiter_base< void >
{
        void await_resume() const
        {
        }
};

}  // namespace ecor
