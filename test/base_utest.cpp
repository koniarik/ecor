
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
#include <functional>
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest.h"
#include "ecor/ecor.hpp"

#include <deque>
#include <iostream>
#include <map>
#include <variant>
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

ecor::task< void > rec_task( task_ctx& ctx, event_source< int, void >& es, int& y )
{
        for ( ;; ) {
                int x  = co_await es.schedule();
                y      = x;
                auto h = rec_task( ctx, es, y );
                co_await std::move( h );  // XXX: is the move a bright idea?
        }
}

TEST_CASE( "recursive" )
{
        nd_mem                    mem;
        task_ctx                  ctx{ mem };
        event_source< int, void > es;
        int                       y = -1;

        auto h = rec_task( ctx, es, y );
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

TEST_CASE( "operator||" )
{
        nd_mem   mem;
        task_ctx ctx{ mem };

        {
                event_source< int, void > es1;
                event_source< int, void > es2;
                int                       result    = -1;
                bool                      completed = false;

                auto test_or = [&]( task_ctx& ctx ) -> ecor::task< void > {
                        result    = co_await ( es1.schedule() || es2.schedule() );
                        completed = true;
                };

                auto h = test_or( ctx );
                ctx.core.run_once();  // Initialize coroutine

                CHECK( !completed );

                es1.set_value( 42 );
                ctx.core.run_once();

                CHECK( completed );
                CHECK( result == 42 );

                completed = false;
                es2.set_value( 99 );
                ctx.core.run_once();

                CHECK( !completed );
        }

        {
                event_source< int, void > es1;
                event_source< int, void > es2;
                int                       result    = -1;
                bool                      completed = false;

                auto test_or = [&]( task_ctx& ctx ) -> ecor::task< void > {
                        result    = co_await ( es1.schedule() || es2.schedule() );
                        completed = true;
                };

                auto h = test_or( ctx );
                ctx.core.run_once();  // Initialize coroutine

                CHECK( !completed );

                // Second event source fires first
                es2.set_value( 123 );
                ctx.core.run_once();

                CHECK( completed );
                CHECK( result == 123 );
        }

        {
                event_source< int, void >         es_int;
                event_source< std::string, void > es_string;
                std::variant< int, std::string >  result;
                bool                              completed = false;

                auto test_mixed_or = [&]( task_ctx& ctx ) -> ecor::task< void > {
                        result    = co_await ( es_int.schedule() || es_string.schedule() );
                        completed = true;
                };

                auto h = test_mixed_or( ctx );
                ctx.core.run_once();  // Initialize coroutine

                CHECK( !completed );

                // String event fires first
                es_string.set_value( std::string( "hello" ) );
                ctx.core.run_once();

                CHECK( completed );
                CHECK( std::holds_alternative< std::string >( result ) );
                CHECK( std::get< std::string >( result ) == "hello" );
        }

        {
                seq_source< uint32_t, uint32_t, void > seq1;
                seq_source< uint32_t, uint32_t, void > seq2;
                uint32_t                               result    = 0;
                bool                                   completed = false;

                auto test_seq_or = [&]( task_ctx& ctx ) -> ecor::task< void > {
                        result    = co_await ( seq1.schedule( 10 ) || seq2.schedule( 5 ) );
                        completed = true;
                };

                auto h = test_seq_or( ctx );
                ctx.core.run_once();  // Initialize coroutine

                CHECK( !completed );

                // Process time 5 - seq2 should win
                while ( !seq2.empty() && seq2.front().key <= 5 )
                        seq2.set_value( seq2.front().key );
                ctx.core.run_once();

                CHECK( completed );
                CHECK( result == 5 );
        }
}

}  // namespace ecor
