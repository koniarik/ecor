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

template < typename T, typename E >
struct event : zll::ll_base< event< T, E > >
{
        virtual void set_value( T value ) = 0;
        virtual void set_error( E error ) = 0;
        virtual void set_stopped()        = 0;
};

template < typename T, typename E >
struct event_core;

template < typename T, typename E >
struct event_core
{
        void set_value( T value )
        {
                auto iter = list.begin();
                auto e    = list.end();
                while ( iter != e ) {
                        auto n = iter++;
                        n->set_value( std::move( value ) );
                }
        }

        void set_error( E err )
        {
                auto iter = list.begin();
                auto e    = list.end();
                while ( iter != e ) {
                        auto n = iter++;
                        n->set_error( std::move( err ) );
                }
        }

        void set_stopped()
        {
                auto iter = list.begin();
                auto e    = list.end();
                while ( iter != e ) {
                        auto n = iter++;
                        n->set_stopped();
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

        void set_value( T value )
        {
                core_.set_value( std::move( value ) );
        }

        void set_error( E err )
        {
                core_.set_error( std::move( err ) );
        }

        void set_stopped()
        {
                core_.set_stopped();
        }

private:
        event_core< T, E > core_;
};

template < typename T, typename E >
struct event_awaitable : event< T, E >
{
        event_awaitable( event_core< T, E >& core )
          : core_( core )
          , res_()
          , h_( nullptr )
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

        std::variant< T, E > await_resume() const
        {
                return std::move( res_ );
        }

        event_core< T, E >&     core_;
        std::variant< T, E >    res_;
        std::coroutine_handle<> h_;


        void set_value( T value ) override
        {
                res_ = std::move( value );
                h_.resume();
        }

        void set_error( E error ) override
        {
                res_ = std::move( error );
                h_.resume();
        }

        void set_stopped() override
        {
                // XXX: is this wise?
                h_.destroy();
        }
};

template < typename T, typename E, typename R >
struct event_op
{
        event_op( R receiver, event_core< T, E >& core )
          : impl_( std::move( receiver ) )
          , core_( core )
        {
        }

        void start()
        {
                core_.list.link_back( &impl_ );
        }

        struct _event_op : event< T, E >
        {
                R receiver_;

                _event_op( R receiver )
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

                void set_stopped() override
                {
                        receiver_.set_stopped();
                }
        };

private:
        _event_op           impl_;
        event_core< T, E >& core_;
};

template < typename T, typename E >
struct event_sender
{
        event_sender( event_core< T, E >& core )
          : core_( core )
        {
        }

        template < typename R >
        event_op< T, E, R > connect( R receiver )
        {
                return { std::move( receiver ), core_ };
        }

        event_core< T, E >& core_;
};

struct _mem_res_promise
{

        static void* operator new( std::size_t const sz, memory_resource auto& mem, auto&&... )
        {
                return alloc( sz, mem );
        }

        static constexpr std::size_t align = alignof( std::max_align_t );
        struct vtable
        {
                void ( *dealloc )( void*, void*, std::size_t const ) = nullptr;
                void* mem                                            = nullptr;
        };
        static constexpr std::size_t spacing = align > sizeof( vtable ) ? align : sizeof( vtable );

        template < memory_resource M >
        static void* alloc( std::size_t sz, M& mem )
        {
                sz += spacing;
                void* const vp = allocate( mem, sz, align );
                vtable      vt{
                         .dealloc =
                        +[]( void* mem, void* p, std::size_t const sz ) {
                                deallocate( *( (M*) mem ), p, sz, align );
                        },
                         .mem = &mem,
                };
                std::memcpy( vp, (void const*) &vt, sizeof( vtable ) );
                return ( (char*) vp ) + spacing;
        }

        static void operator delete( void* const ptr, std::size_t const sz )
        {
                void*  beg = ( (char*) ptr ) - spacing;
                vtable vt;
                std::memcpy( (void*) &vt, beg, sizeof( vtable ) );
                vt.dealloc( vt.mem, beg, sz + spacing );
        }
};

template < typename T >
struct task
{
        using value_type = T;

        struct promise_type : _mem_res_promise
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
                }

                template < typename U, typename E >
                auto await_transform( event_sender< U, E > sender ) noexcept
                {
                        return event_awaitable< U, E >( sender.core_ );
                }
        };
        static_assert(
            alignof( promise_type ) <= alignof( std::max_align_t ),
            "Unsupported alignment" );

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
                if ( h_ && !h_.done() )
                        h_.destroy();
        }

private:
        std::coroutine_handle< promise_type > h_;
};

}  // namespace ecor
