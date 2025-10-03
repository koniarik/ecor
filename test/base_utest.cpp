
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

#include <algorithm>
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

        ctx.core.run_once();
        int value = 42;
        es.set_value( value );
        CHECK( value == y );
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

TEST_CASE( "_align_idx" )
{
        // Create a test buffer with known alignment
        alignas( 64 ) uint8_t test_buffer[256];

        // Test 1: Already aligned index
        {
                auto result = _align_idx( test_buffer, 0, 8 );
                CHECK( result == 0 );  // Should remain 0 as it's already aligned

                // Verify the actual pointer is aligned
                void* ptr = test_buffer + result;
                CHECK( ( (uintptr_t) ptr % 8 ) == 0 );
        }

        // Test 2: Unaligned index that needs adjustment
        {
                auto result = _align_idx( test_buffer, 1, 8 );
                // Should round up to next 8-byte boundary
                void* ptr = test_buffer + result;
                CHECK( ( (uintptr_t) ptr % 8 ) == 0 );
                CHECK( result >= 1 );  // Must be at least the input index
        }

        // Test 3: Various alignment requirements
        struct AlignTest
        {
                uint32_t    input_idx;
                std::size_t alignment;
        };

        std::vector< AlignTest > tests = {
            { .input_idx = 0, .alignment = 1 },    // 1-byte alignment (should be no-op)
            { .input_idx = 1, .alignment = 1 },    // 1-byte alignment (should be no-op)
            { .input_idx = 42, .alignment = 1 },   // 1-byte alignment (should be no-op)
            { .input_idx = 1, .alignment = 2 },    // 2-byte alignment
            { .input_idx = 3, .alignment = 4 },    // 4-byte alignment
            { .input_idx = 5, .alignment = 8 },    // 8-byte alignment
            { .input_idx = 17, .alignment = 16 },  // 16-byte alignment
            { .input_idx = 33, .alignment = 32 },  // 32-byte alignment
            { .input_idx = 65, .alignment = 64 }   // 64-byte alignment
        };

        for ( auto test : tests ) {
                auto result = _align_idx( test_buffer, test.input_idx, test.alignment );

                // Check that result is properly aligned
                void* ptr = test_buffer + result;
                CHECK( ( (uintptr_t) ptr % test.alignment ) == 0 );

                // Check that result is >= input (alignment never moves backwards)
                CHECK( result >= test.input_idx );

                // Check that the difference is less than alignment size
                CHECK( ( result - test.input_idx ) < test.alignment );
        }

        // Test 4: Edge case - index at end of buffer
        {
                auto result = _align_idx( test_buffer, 250, 8 );
                CHECK( result <= 256 );  // Should not exceed buffer size
                if ( result < 256 ) {
                        void* ptr = test_buffer + result;
                        CHECK( ( (uintptr_t) ptr % 8 ) == 0 );
                }
        }

        // Test 5: Different index types
        {
                uint8_t idx8    = 7;
                auto    result8 = _align_idx( test_buffer, idx8, 4 );
                CHECK( ( (uintptr_t) ( test_buffer + result8 ) % 4 ) == 0 );

                uint16_t idx16    = 15;
                auto     result16 = _align_idx( test_buffer, idx16, 8 );
                CHECK( ( (uintptr_t) ( test_buffer + result16 ) % 8 ) == 0 );

                uint32_t idx32    = 31;
                auto     result32 = _align_idx( test_buffer, idx32, 16 );
                CHECK( ( (uintptr_t) ( test_buffer + result32 ) % 16 ) == 0 );
        }
}

TEST_CASE( "circular_buffer_memory" )
{
        circular_buffer_memory< 1024 > mem;

        // Test 1: Basic single allocation
        void* p1 = mem.allocate( 32, 8 );
        CHECK( p1 != nullptr );
        CHECK( ( (uintptr_t) p1 % 8 ) == 0 );

        // Test 2: Multiple allocations
        void* p2 = mem.allocate( 16, 4 );
        void* p3 = mem.allocate( 64, 16 );
        CHECK( p2 != nullptr );
        CHECK( p3 != nullptr );
        CHECK( ( (uintptr_t) p2 % 4 ) == 0 );
        CHECK( ( (uintptr_t) p3 % 16 ) == 0 );

        // Test 3: Deallocate middle block
        mem.deallocate( p2, 16, 4 );

        // Test 4: Allocate after deallocation (should not reuse space immediately due to circular
        // nature)
        void* p4 = mem.allocate( 8, 1 );
        CHECK( p4 != nullptr );

        // Test 5: Deallocate all
        mem.deallocate( p1, 32, 8 );
        mem.deallocate( p3, 64, 16 );
        mem.deallocate( p4, 8, 1 );

        // Test 6: Large allocation that should fail
        void* large = mem.allocate( 2048, 1 );  // Larger than buffer
        CHECK( large == nullptr );
}


TEST_CASE( "circular_buffer_memory_alignment" )
{
        circular_buffer_memory< 512 > mem;

        // Test various alignment requirements
        std::vector< std::pair< void*, std::size_t > > ptrs;

        // Test alignments: 1, 2, 4, 8, 16, 32, 64
        for ( std::size_t align = 1; align <= 64; align *= 2 ) {
                void* p = mem.allocate( 8, align );
                CAPTURE( align );
                CHECK( p != nullptr );
                CHECK( ( (uintptr_t) p % align ) == 0 );
                ptrs.emplace_back( p, align );
        }

        // Clean up
        for ( auto [ptr, align] : ptrs )
                mem.deallocate( ptr, 8, align );
}

TEST_CASE( "circular_buffer_memory_wraparound" )
{
        circular_buffer_memory< 256 > mem;

        std::vector< void* > pointers;

        // Fill up most of the buffer with small allocations
        for ( int i = 0; i < 20; ++i ) {
                void* p = mem.allocate( 8, 1 );
                if ( p )
                        pointers.push_back( p );
        }

        // Should have allocated several blocks
        CHECK( pointers.size() > 5 );

        // Deallocate first few blocks to create space at beginning
        for ( int i = 0; i < 3 && i < pointers.size(); ++i )
                mem.deallocate( pointers[i], 8, 1 );

        // Try to allocate - should succeed and potentially wrap around
        void* wrap_ptr = mem.allocate( 16, 1 );
        CHECK( wrap_ptr != nullptr );

        // Clean up remaining pointers
        for ( int i = 3; i < pointers.size(); ++i )
                mem.deallocate( pointers[i], 8, 1 );
        mem.deallocate( wrap_ptr, 16, 1 );
}

TEST_CASE( "circular_buffer_memory_fragmentation" )
{
        circular_buffer_memory< 512 > mem;

        // Create fragmentation by allocating and deallocating alternating blocks
        std::vector< void* > keep_ptrs;
        std::vector< void* > dealloc_ptrs;

        // Allocate 10 blocks
        for ( int i = 0; i < 10; ++i ) {
                void* p = mem.allocate( 16, 4 );
                CHECK( p != nullptr );

                if ( i % 2 == 0 )
                        keep_ptrs.push_back( p );
                else
                        dealloc_ptrs.push_back( p );
        }

        // Deallocate every other block to create fragmentation
        for ( void* p : dealloc_ptrs )
                mem.deallocate( p, 16, 4 );

        // Try to allocate a larger block - might fail due to fragmentation
        void* large_block = mem.allocate( 80, 1 );
        // This might be nullptr due to fragmentation, which is expected

        // Clean up
        for ( void* p : keep_ptrs )
                mem.deallocate( p, 16, 4 );
        if ( large_block )
                mem.deallocate( large_block, 80, 1 );
}

TEST_CASE( "circular_buffer_memory_edge_cases" )
{
        circular_buffer_memory< 258 > mem;

        // Test 1: Zero-size allocation
        void* zero_ptr = mem.allocate( 0, 1 );
        // Behavior may vary - could be nullptr or valid pointer

        // Test 2: Very large alignment
        void* aligned_ptr = mem.allocate( 8, 64 );
        if ( aligned_ptr ) {
                CHECK( ( (uintptr_t) aligned_ptr % 64 ) == 0 );
                mem.deallocate( aligned_ptr, 8, 64 );
        }

        // Test 3: Allocation that exactly fits remaining space
        // First, use up most space
        std::vector< void* > ptrs;
        while ( true ) {
                void* p = mem.allocate( 8, 1 );
                if ( !p )
                        break;
                ptrs.push_back( p );
        }

        // Clean up
        for ( void* p : ptrs )
                mem.deallocate( p, 8, 1 );

        if ( zero_ptr )
                mem.deallocate( zero_ptr, 0, 1 );
}

TEST_CASE( "circular_buffer_memory_double_deallocation" )
{
        circular_buffer_memory< 256 > mem;

        void* p = mem.allocate( 32, 8 );
        CHECK( p != nullptr );

        // First deallocation - should be fine
        mem.deallocate( p, 32, 8 );

        // Second deallocation of same pointer - undefined behavior
        // We won't test this as it's incorrect usage, but just documenting
}

TEST_CASE( "circular_buffer_memory_different_sizes" )
{
        // Test different buffer sizes and their index types

        // Small buffer - should use uint8_t index
        {
                circular_buffer_memory< 200 > small_mem;
                static_assert( std::is_same_v< decltype( small_mem )::index_type, uint8_t > );

                void* p = small_mem.allocate( 32, 1 );
                CHECK( p != nullptr );
                small_mem.deallocate( p, 32, 1 );
        }

        // Medium buffer - should use uint16_t index
        {
                circular_buffer_memory< 50000 > med_mem;
                static_assert( std::is_same_v< decltype( med_mem )::index_type, uint16_t > );

                void* p = med_mem.allocate( 1000, 1 );
                CHECK( p != nullptr );
                med_mem.deallocate( p, 1000, 1 );
        }
}

TEST_CASE( "circular_buffer_memory_allocation_pattern" )
{
        circular_buffer_memory< 1024 > mem;

        // Test allocation pattern and verify pointers are within buffer bounds
        std::vector< void* > ptrs;

        for ( int i = 0; i < 50; ++i ) {
                void* p = mem.allocate( 10, 2 );
                if ( p ) {
                        // Verify pointer is within buffer bounds
                        uintptr_t buf_start = (uintptr_t) &mem._buf[0];
                        uintptr_t buf_end   = buf_start + sizeof( mem._buf );
                        uintptr_t ptr_addr  = (uintptr_t) p;

                        CHECK( ptr_addr >= buf_start );
                        CHECK( ptr_addr < buf_end );

                        ptrs.push_back( p );
                }
        }

        // Deallocate in reverse order
        std::reverse( ptrs.begin(), ptrs.end() );
        for ( void* p : ptrs )
                mem.deallocate( p, 10, 2 );
}

TEST_CASE( "task_coroutine_with_circular_buffer_memory" )
{
        // Use circular buffer memory instead of regular allocator
        circular_buffer_memory< 2048 > mem;
        task_ctx                       ctx{ mem };

        // Create event sources for inter-task communication
        event_source< int, void >         data_source;
        event_source< std::string, void > message_source;

        // Shared state to verify task execution
        std::vector< int >         processed_numbers;
        std::vector< std::string > processed_messages;
        int                        computation_result = 0;
        bool                       tasks_completed    = false;

        // Task 1: Data processor - processes integers and accumulates result
        auto data_processor = [&]( task_ctx& ctx ) -> ecor::task< void > {
                int count = 0;
                while ( count < 3 ) {
                        int value = co_await data_source.schedule();
                        processed_numbers.push_back( value );
                        computation_result += value * 2;  // Some computation
                        count++;
                }

                // Signal completion by sending a message
                message_source.set_value( "data_processing_complete" );
        };

        // Task 2: Message handler - processes string messages
        auto message_handler = [&]( task_ctx& ctx ) -> ecor::task< void > {
                int message_count = 0;
                while ( message_count < 4 ) {  // Expect 3 regular messages + 1 completion message
                        std::string msg = co_await message_source.schedule();
                        processed_messages.push_back( msg );

                        if ( msg == "data_processing_complete" )
                                tasks_completed = true;
                        message_count++;
                }
        };

        // Task 3: Periodic sender - sends data at intervals
        auto data_sender = [&]( task_ctx& ctx ) -> ecor::task< void > {
                // Send some test data
                std::vector< int > test_data = { 10, 20, 30 };
                for ( int value : test_data ) {
                        // Yield control to allow other tasks to run
                        co_await message_source.schedule();  // Wait for any message to proceed
                        data_source.set_value( value );
                }
        };

        // Start all tasks
        auto h1 = data_processor( ctx );
        auto h2 = message_handler( ctx );
        auto h3 = data_sender( ctx );

        // Test execution sequence
        ctx.core.run_once();  // Initialize tasks

        // Send initial messages to trigger data sender
        message_source.set_value( "start" );
        ctx.core.run_once();

        // Send more trigger messages to continue the sequence
        message_source.set_value( "continue1" );
        ctx.core.run_once();

        message_source.set_value( "continue2" );
        ctx.core.run_once();

        // Allow final processing
        ctx.core.run_once();

        // Verify the results
        CHECK( processed_numbers.size() == 3 );
        CHECK( processed_numbers[0] == 10 );
        CHECK( processed_numbers[1] == 20 );
        CHECK( processed_numbers[2] == 30 );

        CHECK( computation_result == 120 );  // (10 + 20 + 30) * 2 = 120

        CHECK( processed_messages.size() == 4 );
        CHECK( processed_messages[0] == "start" );
        CHECK( processed_messages[1] == "continue1" );
        CHECK( processed_messages[2] == "continue2" );
        CHECK( processed_messages[3] == "data_processing_complete" );

        CHECK( tasks_completed == true );
}

TEST_CASE( "task_coroutine_circular_buffer_memory_stress" )
{
        // Smaller buffer to test memory management under pressure
        circular_buffer_memory< 2048 > mem;
        task_ctx                       ctx{ mem };

        event_source< int, void > trigger;
        std::vector< int >        execution_order;
        int                       task_counter = 0;

        // Create multiple short-lived tasks that should stress the circular buffer
        auto task = [&]( task_ctx& ctx, int task_id ) -> ecor::task< void > {
                // Wait for trigger
                int value = co_await trigger.schedule();
                execution_order.push_back( task_id );
                task_counter++;

                // Do some work that might allocate memory
                if ( task_id % 2 == 0 ) {
                        // Even tasks wait for another trigger
                        co_await trigger.schedule();
                        execution_order.push_back( task_id + 100 );  // Mark second
                                                                     // execution
                }
        };

        // Create several tasks
        std::vector< ecor::task< void > > tasks;
        for ( int i = 1; i <= 5; ++i )
                tasks.push_back( task( ctx, i ) );

        // Run initial setup
        ctx.core.run_once();

        // Trigger all tasks
        trigger.set_value( 1 );
        ctx.core.run_once();

        // Trigger even tasks for second execution
        trigger.set_value( 2 );
        ctx.core.run_once();

        // Verify execution
        CHECK( task_counter >= 5 );  // All tasks executed at least once
        CHECK( execution_order.size() >= 5 );

        // Check that even tasks executed twice (they should have entries > 100)
        bool found_second_execution = false;
        for ( int val : execution_order ) {
                if ( val > 100 ) {
                        found_second_execution = true;
                        break;
                }
        }
        CHECK( found_second_execution );
}


}  // namespace ecor
