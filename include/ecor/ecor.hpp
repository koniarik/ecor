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
        using value_type = T;
        using error_type = E;

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


template < typename S, typename R >
using connect_type = decltype( std::declval< S >().connect( std::declval< R >() ) );

enum class _awaitable_state_e : uint8_t
{
        empty,
        value,
        error
};

template < typename T, typename E >
struct _awaitable_expected
{
        _awaitable_state_e state = _awaitable_state_e::empty;
        union
        {
                T val;
                E err;
        };

        void set_value( T v ) noexcept
        {
                new ( (void*) &val ) T( std::move( v ) );
                state = _awaitable_state_e::value;
        }

        void set_error( E v ) noexcept
        {
                new ( (void*) &err ) E( std::move( v ) );
                state = _awaitable_state_e::error;
        }
};

template < typename E >
struct _awaitable_expected< void, E >
{
        _awaitable_state_e state = _awaitable_state_e::empty;
        union
        {
                E err;
        };

        void set_value() noexcept
        {
                state = _awaitable_state_e::value;
        }

        void set_error( E v ) noexcept
        {
                new ( (void*) &err ) E( std::move( v ) );
                state = _awaitable_state_e::error;
        }
};

template < typename T >
struct _awaitable_expected< T, void >
{
        _awaitable_state_e state = _awaitable_state_e::empty;
        union
        {
                T val;
        };

        void set_value( T v ) noexcept
        {
                new ( (void*) &val ) T( std::move( v ) );
                state = _awaitable_state_e::value;
        }
};

template <>
struct _awaitable_expected< void, void >
{
        _awaitable_state_e state = _awaitable_state_e::empty;

        void set_value() noexcept
        {
                state = _awaitable_state_e::value;
        }
};


template < typename S >
struct _awaitable
{
        _awaitable( S sender, std::coroutine_handle<> h )
          : op_( std::move( sender ).connect( _receiver{ .exp_ = &exp_, .cont_ = h } ) )
        {
        }

        [[nodiscard]] bool await_ready() const noexcept
        {
                return false;
        }

        void await_suspend( std::coroutine_handle<> ) noexcept
        {
                op_.start();
        }

        using value_type = typename S::value_type;
        using error_type = typename S::error_type;

        auto await_resume() const noexcept
        {
                if constexpr ( std::same_as< error_type, void > ) {
                        if constexpr ( std::same_as< value_type, void > )
                                return;
                        else
                                // if ( exp_.state == _awaitable_state_e::value )
                                return std::move( exp_.val );
                } else {
                        using R = std::variant< value_type, error_type >;
                        if ( exp_.state == _awaitable_state_e::value )
                                return R{ std::move( exp_.val ) };
                        else {
                                //       if ( exp_.state == _awaitable_state_e::error )
                                return R{ std::move( exp_.err ) };
                        }
                }
                // Should not happen
        }

        using _aw_tp = _awaitable_expected< typename S::value_type, typename S::error_type >;

        struct _receiver
        {
                _aw_tp*                 exp_;
                std::coroutine_handle<> cont_;

                template < typename... Ts >
                void set_value( Ts&&... vals ) noexcept
                {
                        exp_->set_value( (Ts&&) vals... );
                        cont_.resume();
                }

                template < typename... Es >
                void set_error( Es&&... errs ) noexcept
                {
                        exp_->set_error( (Es&&) errs... );
                        cont_.resume();
                }
        };

        _aw_tp                       exp_;
        connect_type< S, _receiver > op_;
};

template < typename R, typename E >
struct _notif_op_impl : notif< E >
{
        using core_type = notify_core< E >;

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
        using core_type = notify_core< void >;
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
        using value_type = void;
        using error_type = E;

        notify_sender( notify_core< E >& core )
          : core_( core )
        {
        }

        template < typename R >
        _op< _notif_op_impl< R, E > > connect( R receiver )
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

inline void* allocate( task_allocator& t, std::size_t const sz, std::size_t const align )
{
        return t.alloc( t.mem, sz, align );
}

inline void deallocate( task_allocator& t, void* p, std::size_t const sz, std::size_t const align )
{
        return t.dealloc( t.mem, p, sz, align );
}


struct _task_token : zll::ll_base< _task_token >
{

        _task_token( std::coroutine_handle<> h = nullptr )
          : handle( h )
        {
        }

        void resume()
        {
                if ( handle )
                        handle.resume();
        }

        std::coroutine_handle<> handle = nullptr;
};

struct task_core
{
        zll::ll_list< _task_token > sleepy_tasks;

        void run_once()
        {
                if ( sleepy_tasks.empty() )
                        return;
                auto t = sleepy_tasks.front();
                t.resume();
                sleepy_tasks.detach_front();
        }
};

struct task_ctx
{
        task_allocator alloc;
        task_core      core;

        task_ctx( auto& mem )
          : alloc( mem )
        {
        }
};

inline task_core& get_task_core( task_ctx& t )
{
        return t.core;
}

inline task_allocator& get_task_alloc( task_ctx& t )
{
        return t.alloc;
}

template < typename T >
struct task;

template < typename T >
struct _task_awaiter;

struct _task_resumer : _task_token
{
        _task_resumer( task_core& core )
          : _core( core )
        {
        }

        task_core& _core;

        void reschedule()
        {
                _core.sleepy_tasks.link_back( *this );
        }
};

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

        static void* operator new( std::size_t const sz, auto& ctx, auto&&... )
        {
                task_allocator& a = get_task_alloc( ctx );
                vtable          vt{
                             .mem = &a,
                };
                return alloc( sz, a, vt );
        }

        static void operator delete( void* const ptr, std::size_t const sz )
        {
                dealloc( ptr, sz );
        }

        _promise_base( auto& ctx, auto&&... )
          : core( get_task_core( ctx ) )
        {
        }

        std::suspend_never initial_suspend() noexcept
        {
                return {};
        }

        std::suspend_always final_suspend() noexcept
        {
                if ( resum )
                        resum->reschedule();
                return {};
        }

        task_core&     core;
        _task_resumer* resum = nullptr;

        void unhandled_exception() noexcept
        {
        }
};

template < typename T >
struct _task_awaiter
{
        _task_awaiter( task< T >&& t, task_core& c )
          : resum_( c )
          , task_( std::move( t ) )
        {
        }

        auto await_resume() const
        {
                if constexpr ( std::same_as< T, void > )
                        return;
                else {
                        auto* p = task_.result();
                        assert( p );  // TODO: re-think
                        return std::move( *p );
                }
        }

        bool await_ready() const noexcept
        {
                auto r = task_.done();
                return r;
        }

        bool await_suspend( std::coroutine_handle<> h ) noexcept
        {
                resum_.handle = h;
                if ( !task_.done() )
                        task_.promise().resum = &resum_;
                else
                        resum_.reschedule();
                return true;
        }

private:
        _task_resumer resum_;
        task< T >     task_;
};


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

        template < typename U, typename E >
        auto await_transform( event_sender< U, E > sender ) noexcept
        {
                return _awaitable{
                    std::move( sender ),
                    std::coroutine_handle< _promise_type >::from_promise( *this ) };
        }

        template < typename U, typename E >
        auto await_transform( notify_sender< U, E > sender ) noexcept
        {
                return _awaitable{
                    std::move( sender ),
                    std::coroutine_handle< _promise_type >::from_promise( *this ) };
        }

        template < typename U >
        auto await_transform( task< U >&& other ) noexcept
        {
                return _task_awaiter< U >{ std::move( other ), this->core };
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
                        task tmp = std::move( other );
                        std::swap( h_, tmp.h_ );
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

}  // namespace ecor
