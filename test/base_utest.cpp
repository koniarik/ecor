
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

TEST_CASE( "notify" )
{
        nd_mem                     mem;
        task_ctx                   ctx{ mem };
        notify_source< int, void > es;
        int                        y = 0;

        auto f = [&]( task_ctx& ) -> ecor::task< void > {
                for ( ;; ) {
                        co_await es.schedule();
                        y++;
                }
        };

        auto h = f( ctx );

        es.set_value();
        CHECK( y == 1 );
        es.set_value();
        CHECK( y == 2 );
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

};  // namespace ecor
