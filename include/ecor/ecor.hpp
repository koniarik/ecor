#pragma once

#include <concepts>
#include <coroutine>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <limits>
#include <optional>
#include <variant>
#include <zll.hpp>

namespace ecor
{

template < typename T >
concept memory_resource = requires( T a, std::size_t bytes, std::size_t align, void* p ) {
        { allocate( a, bytes, align ) } -> std::same_as< void* >;
        { deallocate( a, p, bytes, align ) } -> std::same_as< void >;
};

struct _dummy_receiver
{
        template < typename... Args >
        void set_value( Args&&... args )
        {
        }
        template < typename... Args >
        void set_error( Args&&... args )
        {
        }
};

template < typename T >
concept sender = requires( T s ) {
        { std::move( s ).connect( std::declval< _dummy_receiver >() ) };
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
T _align_idx( uint8_t const* buff, T idx, std::size_t align )
{
        auto* p = buff + idx;
        auto  a = ( (uintptr_t) p ) % align;
        if ( a )
                p += align - a;
        return (T) ( p - buff );
}

template < std::size_t N >
constexpr auto _pick_index_type()
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

template < typename T >
struct _circular_buffer_memory_base
{
        using index_type = T;

        static constexpr index_type npos = std::numeric_limits< index_type >::max();

        struct node
        {
                index_type next_idx = npos;
                index_type prev_idx = npos;
        };
        index_type _first = npos;
        index_type _last  = npos;
        index_type _next  = npos;

        index_type
        _pick_index( uint8_t* buff, std::size_t bytes, std::size_t align, std::size_t size ) const
        {
                auto idx = _next == npos ? sizeof( node ) : _next + sizeof( node );
                idx      = _align_idx( buff, idx, align );

                auto capacity = size - idx;
                if ( capacity >= bytes )
                        return idx - sizeof( node );

                idx      = _align_idx( buff, sizeof( node ), align );
                capacity = _first != npos ? idx - _first : size - idx;
                if ( capacity >= bytes )
                        return idx - sizeof( node );

                return npos;
        }

        node _get_node( uint8_t* buff, index_type idx ) const
        {
                node n;
                std::memcpy( &n, buff + idx, sizeof( node ) );
                return n;
        }

        void _set_node( uint8_t* buff, index_type idx, node const& n )
        {
                std::memcpy( buff + idx, &n, sizeof( node ) );
        }

        void* _allocate( uint8_t* buff, std::size_t bytes, std::size_t align, std::size_t size )
        {
                auto idx = _pick_index( buff, bytes, align, size );
                if ( idx == npos )
                        return nullptr;
                auto* p = buff + idx;
                if ( _first == npos ) {
                        _first = idx;
                } else {
                        auto nn     = _get_node( buff, _last );
                        nn.next_idx = idx;
                        _set_node( buff, _last, nn );
                }
                node n{
                    .prev_idx = _last,
                    .next_idx = npos,
                };
                _last = idx;
                _next = idx + sizeof( node ) + bytes;
                _set_node( buff, idx, n );
                return p + sizeof( node );
        }

        void _deallocate( uint8_t* buff, void* p )
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
                        _next = npos;
                } else {
                        auto nn     = _get_node( buff, n.next_idx );
                        nn.prev_idx = n.prev_idx;
                        _set_node( buff, n.next_idx, nn );
                }

                _set_node( buff, idx, node{ .next_idx = 0, .prev_idx = 0 } );
        }
};

template < std::size_t N >
struct circular_buffer_memory : private _circular_buffer_memory_base< _index_type< N > >
{
        using base = _circular_buffer_memory_base< _index_type< N > >;
        using base::_first;
        using base::_last;
        using base::_next;
        using node = base::node;
        using base::npos;
        using index_type = base::index_type;

        alignas( std::max_align_t ) uint8_t _buf[N];

        void* allocate( std::size_t bytes, std::size_t align )
        {
                return base::_allocate( _buf, bytes, align, N );
        }

        void deallocate( void* p, std::size_t, std::size_t )
        {
                base::_deallocate( _buf, (uint8_t*) p );
        }
};

template < typename T >
struct _set_error
{
        virtual void set_error( T error ) = 0;
};

template <>
struct _set_error< void >
{
};

template < typename T, typename E >
struct event_entry : _set_error< E >, zll::ll_base< event_entry< T, E > >
{
        using value_type = T;
        using error_type = E;

        virtual void set_value( T value ) = 0;
};

template < typename T, typename E >
struct event_core
{

        template < typename V >
        void set_value( V&& value )
        {
                for_each( [&]( auto& n ) {
                        n.set_value( value );
                } );
        }

        template < typename E1 >
        void set_error( E1&& err )
        {
                for_each( [&]( auto& n ) {
                        n.set_error( err );
                } );
        }

        void for_each( auto f )
        {
                auto l    = std::move( list );
                auto iter = l.begin();
                auto e    = l.end();
                while ( iter != e ) {
                        auto n = iter++;
                        f( *n );
                }
        }

        zll::ll_list< event_entry< T, E > > list;
};

template < typename T, typename E >
struct event_sender;


template < typename T, typename E >
struct event_source
{
        using value_type = T;
        using error_type = E;

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


template < typename B, typename R >
struct _op_impl : B
{
        using base_type  = B;
        using value_type = typename B::value_type;
        using error_type = typename B::error_type;

        R receiver_;

        _op_impl( R receiver )
          : receiver_( std::move( receiver ) )
        {
        }

        void set_value( value_type value ) override
        {
                receiver_.set_value( std::move( value ) );
        }

        void set_error( error_type err ) override
        {
                receiver_.set_error( std::move( err ) );
        }
};

template < typename B, typename R >
        requires( std::same_as< typename B::error_type, void > )
struct _op_impl< B, R > : B
{
        using base_type  = B;
        using value_type = typename B::value_type;
        using error_type = typename B::error_type;

        R receiver_;

        template < typename... Args >
        _op_impl( R receiver, Args&&... args )
          : B( (Args&&) args... )
          , receiver_( std::move( receiver ) )
        {
        }

        void set_value( value_type value ) override
        {
                receiver_.set_value( std::move( value ) );
        }
};

template < typename OpImpl >
struct _list_op
{

        template < typename... Args >
        _list_op( auto& list, auto receiver, Args&&... args )
          : impl_( std::move( receiver ), (Args&&) args... )
          , list_( list )
        {
        }

        void start()
        {
                list_.link_back( impl_ );
        }

private:
        OpImpl                                      impl_;
        zll::ll_list< typename OpImpl::base_type >& list_;
};

template < typename OpImpl >
struct _heap_op
{

        template < typename... Args >
        _heap_op( auto& heap, auto receiver, Args&&... args )
          : impl_( std::move( receiver ), (Args&&) args... )
          , heap_( heap )
        {
        }

        void start()
        {
                heap_.link( impl_ );
        }

private:
        OpImpl                                      impl_;
        zll::sh_heap< typename OpImpl::base_type >& heap_;
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
        _list_op< _op_impl< event_entry< T, E >, R > > connect( R receiver )
        {
                return { core_.list, std::move( receiver ) };
        }

        event_core< T, E >& core_;
};

template < typename K, typename T, typename E >
struct seq_entry : _set_error< E >, zll::sh_base< seq_entry< K, T, E > >
{
        using value_type = T;
        using error_type = E;

        K key;

        seq_entry( K k )
          : key( std::move( k ) )
        {
        }

        virtual void set_value( T value ) = 0;

        constexpr bool operator<( seq_entry const& o ) const
        {
                return key < o.key;
        }
};

template < typename K, typename T, typename E >
struct seq_core
{
        template < typename V >
        void set_value( V&& value )
        {
                do_f( [&]( auto& n ) {
                        n.set_value( (V&&) value );
                } );
        }

        template < typename E1 >
        void set_error( E1&& err )
        {
                do_f( [&]( auto& n ) {
                        n.set_error( (E1&&) err );
                } );
        }

        void do_f( auto f )
        {
                if ( h.empty() )
                        return;

                auto& n = h.take();
                f( n );
        }

        bool empty() const
        {
                return h.empty();
        }

        seq_entry< K, T, E > const& front() const
        {
                return *h.top;
        }

        zll::sh_heap< seq_entry< K, T, E > > h;
};

template < typename K, typename T, typename E >
struct seq_sender;

template < typename K, typename T, typename E >
struct seq_source
{
        using value_type = T;
        using error_type = E;

        // TODO: technically this is not spec-conforming
        seq_sender< K, T, E > schedule( K key )
        {
                return { key, core_ };
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

        bool empty() const
        {
                return core_.empty();
        }

        seq_entry< K, T, E > const& front() const
        {
                return core_.front();
        }

private:
        seq_core< K, T, E > core_;
};

template < typename K, typename T, typename E >
struct seq_sender
{
        using value_type = T;
        using error_type = E;


        seq_sender( K key, seq_core< K, T, E >& core )
          : key( key )
          , core_( core )
        {
        }

        template < typename R >
        _heap_op< _op_impl< seq_entry< K, T, E >, R > > connect( R receiver ) &&
        {
                return { core_.h, std::move( receiver ), std::move( key ) };
        }

        K                    key;
        seq_core< K, T, E >& core_;
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
struct _expected
{
        _expected() noexcept
        {
        }

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

        ~_expected() noexcept
        {
                if ( state == _awaitable_state_e::value )
                        val.~T();
                else if ( state == _awaitable_state_e::error )
                        err.~E();
        }
};

template < typename T >
struct _expected< T, void >
{
        _expected() noexcept
        {
        }

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
        ~_expected() noexcept
        {
                if ( state == _awaitable_state_e::value )
                        val.~T();
        }
};


template < typename S >
struct _awaitable
{
        _awaitable( S sender, std::coroutine_handle<> h )
          : exp_()
          , op_( std::move( sender ).connect( _receiver{ .exp_ = &exp_, .cont_ = h } ) )
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

        using _aw_tp = _expected< typename S::value_type, typename S::error_type >;

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


struct _task_cont_token : zll::ll_base< _task_cont_token >
{

        _task_cont_token( std::coroutine_handle<> h = nullptr )
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
        zll::ll_list< _task_cont_token > idle_tasks;

        void run_once()
        {
                if ( idle_tasks.empty() )
                        return;
                auto t = idle_tasks.front();
                t.resume();
                idle_tasks.detach_front();
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

struct _task_resumer : _task_cont_token
{
        _task_resumer( task_core& core )
          : _core( core )
        {
        }

        task_core& _core;

        void reschedule()
        {
                _core.idle_tasks.link_back( *this );
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

        // XXX: check the noexcept thing

        static void* alloc( std::size_t sz, task_allocator& mem, vtable& vt ) noexcept
        {
                sz += spacing;
                void* const vp = allocate( mem, sz, align );
                if ( !vp )
                        return nullptr;
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
                vtable          vt{
                             .mem = &a,
                };
                return alloc( sz, a, vt );
        }

        void operator delete( void* const ptr, std::size_t const sz ) noexcept
        {
                dealloc( ptr, sz );
        }

        _promise_base( auto&& ctx, auto&&... )
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
        using _promise_base::_promise_base;

        static task< T > get_return_object_on_allocation_failure()
        {
                return { std::coroutine_handle< _promise_type >{} };
        }

        task< T > get_return_object()
        {
                return { std::coroutine_handle< _promise_type >::from_promise( *this ) };
        }

        template < sender S >
        auto await_transform( S sender ) noexcept
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

template < typename S1, typename S2, typename R >
struct _or_op
{
        _or_op( S1 s1, S2 s2, R r )
          : recv_( std::move( r ) )
          , op1_( std::move( s1 ).connect( _r{ .fired_ = fired_, .recv_ = recv_ } ) )
          , op2_( std::move( s2 ).connect( _r{ .fired_ = fired_, .recv_ = recv_ } ) )
        {
        }

        // XXX: cancel of the other one shall be used isntead
        void start()
        {
                op1_.start();
                op2_.start();
        }

        struct _r
        {
                bool& fired_;
                R&    recv_;

                template < typename T >
                void set_value( T&& v )
                {
                        if ( fired_ )
                                return;
                        fired_ = true;
                        recv_.set_value( (T&&) v );
                }

                template < typename T >
                void set_error( T&& e )
                {
                        if ( fired_ )
                                return;
                        fired_ = true;
                        recv_.set_error( (T&&) e );
                }
        };

        R recv_;

        bool fired_ = false;

        connect_type< S1, _r > op1_{};
        connect_type< S2, _r > op2_{};
};

template < typename T1, typename T2 >
struct _variant_of
{
        using type = std::variant< T1, T2 >;
};

// This might be brave
template < typename T >
struct _variant_of< T, T >
{
        using type = T;
};

template < typename T1 >
struct _variant_of< T1, void >
{
        using type = T1;
};

template < typename T1 >
struct _variant_of< void, T1 >
{
        using type = T1;
};

template <>
struct _variant_of< void, void >
{
        using type = void;
};

template < typename S1, typename S2 >
struct _or_sender
{
        _or_sender( S1 s1, S2 s2 )
          : s1_( std::move( s1 ) )
          , s2_( std::move( s2 ) )
        {
        }

        using value_type = _variant_of< typename S1::value_type, typename S2::value_type >::type;
        using error_type = _variant_of< typename S1::error_type, typename S2::error_type >::type;

        template < typename R >
        _or_op< S1, S2, R > connect( R receiver )
        {
                return { std::move( s1_ ), std::move( s2_ ), std::move( receiver ) };
        }

private:
        S1 s1_;
        S2 s2_;
};

template < typename S1, typename S2 >
_or_sender< S1, S2 > operator||( S1 s1, S2 s2 )
{
        return { std::move( s1 ), std::move( s2 ) };
}

}  // namespace ecor
