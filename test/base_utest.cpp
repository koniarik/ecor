
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
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest.h"
#include "ecor/ecor.hpp"

#include <iostream>
#include <map>
#include <vector>

namespace ecor
{

struct nd_mem
{
        void* allocate( std::size_t bytes, std::size_t align )
        {
                //      std::cout << "alloc " << bytes << " align " << align << " this: " << this <<
                //      "\n";
                return ::operator new( bytes, std::align_val_t( align ) );
        }

        void deallocate( void* p, std::size_t bytes, std::size_t align )
        {
                //   std::cout << "dealloc " << bytes << " align " << align << " this: " << this <<
                //   "\n";
                ::operator delete( p, std::align_val_t( align ) );
        }
};

struct err
{
};

static void test_simple_event_source( task_ctx& ctx, auto& es, auto& val )
{
        ctx.core.run_once();
        int value = 42;
        es.set_value( value );
        CHECK( value == val );

        ctx.core.run_once();
        value = 666;
        es.set_value( value );
        CHECK( value == val );
        ctx.core.run_once();
}

TEST_CASE( "dyn_memory_base" )
{
        nd_mem                   mem;
        task_ctx                 ctx{ mem };
        event_source< int, err > es;
        int                      y = -1;

        auto f = [&]( task_ctx& ) -> ecor::task< void > {
                for ( ;; ) {
                        int x = std::get< 0 >( co_await es.schedule() );
                        y     = x;
                }
        };

        auto h = f( ctx );

        test_simple_event_source( ctx, es, y );
}

TEST_CASE( "void error" )
{
        nd_mem                    mem;
        task_ctx                  ctx{ mem };
        event_source< int, void > es;
        int                       y = -1;

        auto f = [&]( task_ctx& ) -> ecor::task< void > {
                for ( ;; ) {
                        int x = co_await es.schedule();
                        y     = x;
                }
        };

        auto h = f( ctx );

        test_simple_event_source( ctx, es, y );
}

TEST_CASE( "op" )
{
        nd_mem                    mem;
        task_ctx                  ctx{ mem };
        event_source< int, void > es;
        int                       y = -1;

        struct
        {
                int& y;
                void set_value( int v )
                {
                        y = v;
                }
        } receiver{ .y = y };

        auto s  = es.schedule();
        auto op = s.connect( receiver );
        op.start();

        test_simple_event_source( ctx, es, y );
}


TEST_CASE( "recursive" )
{
        nd_mem                    mem;
        task_ctx                  ctx{ mem };
        event_source< int, void > es;
        int                       y = -1;

        std::function< ecor::task< void >( task_ctx& ) > f =
            [&]( task_ctx& ) -> ecor::task< void > {
                int x  = co_await es.schedule();
                y      = x;
                auto h = f( ctx );
                co_await std::move( h );  // XXX: is the move a bright idea?
        };

        auto h = f( ctx );
        test_simple_event_source( ctx, es, y );
}

TEST_CASE( "transitive noop" )
{
        nd_mem                    mem;
        task_ctx                  ctx{ mem };
        event_source< int, void > es;
        int                       y = -1;

        auto g = [&]( task_ctx& ) -> ecor::task< void > {
                co_return;
        };
        auto f = [&]( task_ctx& ctx ) -> ecor::task< void > {
                for ( ;; ) {
                        int x  = co_await es.schedule();
                        y      = x;
                        auto h = g( ctx );
                        co_await std::move( h );
                }
        };

        auto h = f( ctx );
        test_simple_event_source( ctx, es, y );
}

struct timer
{

        using time_point = uint32_t;

        auto wait_until( time_point tp ) noexcept
        {
                return _es.schedule( tp );
        }

        void tick( time_point now )
        {
                while ( !_es.empty() && _es.front().key <= now )
                        _es.set_value( std::move( _es.front().key ) );
        }

private:
        seq_source< time_point, time_point, void > _es;
};

TEST_CASE( "timer" )
{
        nd_mem   mem;
        task_ctx ctx{ mem };
        timer    tm;
        uint32_t cnt = 0;

        auto f = [&]( task_ctx& ctx ) -> ecor::task< void > {
                uint32_t i = 0;
                for ( ;; ) {
                        co_await tm.wait_until( i++ );
                        ++cnt;
                }
        };

        auto h = f( ctx );

        ctx.core.run_once();
        tm.tick( 41 );
        CHECK( cnt == 42 );
}

TEST_CASE( "timer_complex_heap" )
{
        nd_mem   mem;
        task_ctx ctx{ mem };
        timer    tm;

        std::vector< std::pair< uint32_t, uint32_t > > execution_log;
        uint32_t                                       total_executions = 0;

        auto lambda1 = [&]( task_ctx& ctx ) -> ecor::task< void > {
                std::vector< uint32_t > times = { 10, 30, 50, 70, 90 };
                for ( auto time : times ) {
                        co_await tm.wait_until( time );
                        execution_log.emplace_back( 1, time );
                        ++total_executions;
                }
        };

        auto lambda2 = [&]( task_ctx& ctx ) -> ecor::task< void > {
                std::vector< uint32_t > times = { 5, 25, 45, 65, 85 };
                for ( auto time : times ) {
                        co_await tm.wait_until( time );
                        execution_log.emplace_back( 2, time );
                        ++total_executions;
                }
        };

        auto lambda3 = [&]( task_ctx& ctx ) -> ecor::task< void > {
                std::vector< uint32_t > times = { 15, 35, 55, 75, 95 };
                for ( auto time : times ) {
                        co_await tm.wait_until( time );
                        execution_log.emplace_back( 3, time );
                        ++total_executions;
                }
        };

        auto lambda4 = [&]( task_ctx& ctx ) -> ecor::task< void > {
                std::vector< uint32_t > times = { 12, 18, 22, 28, 32, 38, 42, 48, 52, 58 };
                for ( auto time : times ) {
                        co_await tm.wait_until( time );
                        execution_log.emplace_back( 4, time );
                        ++total_executions;
                }
        };

        auto h1 = lambda1( ctx );
        auto h2 = lambda2( ctx );
        auto h3 = lambda3( ctx );
        auto h4 = lambda4( ctx );

        for ( uint32_t current_time = 0; current_time <= 100; ++current_time ) {
                tm.tick( current_time );

                ctx.core.run_once();
        }

        CHECK( total_executions == 25 );

        // Verify execution happened in correct time order
        for ( size_t i = 1; i < execution_log.size(); ++i )
                CHECK( execution_log[i].second >= execution_log[i - 1].second );

        // Verify specific execution order for some key times
        std::map< uint32_t, std::vector< uint32_t > > executions_by_time;
        for ( auto& [coroutine_id, time] : execution_log )
                executions_by_time[time].push_back( coroutine_id );

        // Check some specific timing constraints
        CHECK( executions_by_time[5] == std::vector< uint32_t >{ 2 } );
        CHECK( executions_by_time[10] == std::vector< uint32_t >{ 1 } );
        CHECK( executions_by_time[12] == std::vector< uint32_t >{ 4 } );
        CHECK( executions_by_time[15] == std::vector< uint32_t >{ 3 } );

        // Verify overlapping times are handled correctly
        CHECK( executions_by_time[25] == std::vector< uint32_t >{ 2 } );
        CHECK( executions_by_time[28] == std::vector< uint32_t >{ 4 } );
        CHECK( executions_by_time[30] == std::vector< uint32_t >{ 1 } );
}


};  // namespace ecor
