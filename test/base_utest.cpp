
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
#include <cstddef>
#include <functional>
#include <iostream>

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN

#include "doctest.h"
#include "ecor/ecor.hpp"

#include <algorithm>
#include <deque>
#include <list>
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

static void test_simple_broadcast_source( task_ctx& ctx, auto& es, auto& val )
{
        int value = 42;
        es.set_value( value );
        ctx.core.run_n( 10 );
        CHECK( value == val );

        value = 666;
        es.set_value( value );
        ctx.core.run_n( 10 );
        CHECK( value == val );
}

TEST_CASE( "dyn_memory_base" )
{
        nd_mem                                                            mem;
        task_ctx                                                          ctx{ mem };
        broadcast_source< set_value_t( int ), set_error_t( task_error ) > es;
        int                                                               y = -1;


        auto f = [&]( task_ctx& ) -> ecor::task< void > {
                for ( ;; ) {
                        int x = co_await es.schedule();
                        y     = x;
                }
        };

        auto h = f( ctx ).connect( _dummy_receiver{} );
        h.start();
        test_simple_broadcast_source( ctx, es, y );
}

TEST_CASE( "void error" )
{
        nd_mem                                 mem;
        task_ctx                               ctx{ mem };
        broadcast_source< set_value_t( int ) > es;
        int                                    y = -1;

        auto f = [&]( task_ctx& ) -> ecor::task< void > {
                for ( ;; ) {
                        int x = co_await es.schedule();
                        y     = x;
                }
        };

        auto h = f( ctx ).connect( _dummy_receiver{} );
        h.start();

        test_simple_broadcast_source( ctx, es, y );
}

TEST_CASE( "op" )
{
        nd_mem                                 mem;
        task_ctx                               ctx{ mem };
        broadcast_source< set_value_t( int ) > es;
        int                                    y = -1;

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

ecor::task< void > rec_task( task_ctx& ctx, broadcast_source< set_value_t( int ) >& es, int& y )
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
        nd_mem                                 mem;
        task_ctx                               ctx{ mem };
        broadcast_source< set_value_t( int ) > es;
        int                                    y = -1;

        auto h = rec_task( ctx, es, y ).connect( _dummy_receiver{} );
        h.start();
        test_simple_broadcast_source( ctx, es, y );
}

TEST_CASE( "transitive noop" )
{
        nd_mem                                 mem;
        task_ctx                               ctx{ mem };
        broadcast_source< set_value_t( int ) > es;
        int                                    y = -1;

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

        auto h = f( ctx ).connect( _dummy_receiver{} );
        h.start();
        test_simple_broadcast_source( ctx, es, y );
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
        seq_source< time_point, set_value_t( time_point ) > _es;
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

        auto h = f( ctx ).connect( _dummy_receiver{} );
        h.start();

        for ( std::size_t i = 0; i <= 42; i++ ) {
                ctx.core.run_once();
                tm.tick( i );
        }
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

        auto h1 = lambda1( ctx ).connect( _dummy_receiver{} );
        h1.start();
        auto h2 = lambda2( ctx ).connect( _dummy_receiver{} );
        h2.start();
        auto h3 = lambda3( ctx ).connect( _dummy_receiver{} );
        h3.start();
        auto h4 = lambda4( ctx ).connect( _dummy_receiver{} );
        h4.start();

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
                broadcast_source< set_value_t( int ) > es1;
                broadcast_source< set_value_t( int ) > es2;
                int                                    result    = -1;
                bool                                   completed = false;

                auto test_or = [&]( task_ctx& ctx ) -> ecor::task< void > {
                        result    = co_await ( es1.schedule() || es2.schedule() );
                        completed = true;
                };

                auto h = test_or( ctx ).connect( _dummy_receiver{} );
                h.start();
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
                broadcast_source< set_value_t( int ) > es1;
                broadcast_source< set_value_t( int ) > es2;
                int                                    result    = -1;
                bool                                   completed = false;

                auto test_or = [&]( task_ctx& ctx ) -> ecor::task< void > {
                        result    = co_await ( es1.schedule() || es2.schedule() );
                        completed = true;
                };

                auto h = test_or( ctx ).connect( _dummy_receiver{} );
                h.start();
                ctx.core.run_once();  // Initialize coroutine

                CHECK( !completed );

                // Second event source fires first
                es2.set_value( 123 );
                ctx.core.run_once();

                CHECK( completed );
                CHECK( result == 123 );
        }

        {
                broadcast_source< set_value_t( int ) >         es_int;
                broadcast_source< set_value_t( std::string ) > es_string;
                std::variant< int, std::string >               result;
                bool                                           completed = false;

                auto test_mixed_or = [&]( task_ctx& ctx ) -> ecor::task< void > {
                        result = co_await as_variant( es_int.schedule() || es_string.schedule() );
                        completed = true;
                };

                auto h = test_mixed_or( ctx ).connect( _dummy_receiver{} );
                h.start();
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
                seq_source< uint32_t, set_value_t( uint32_t ) > seq1;
                seq_source< uint32_t, set_value_t( uint32_t ) > seq2;
                uint32_t                                        result    = 0;
                bool                                            completed = false;

                auto test_seq_or = [&]( task_ctx& ctx ) -> ecor::task< void > {
                        result    = co_await ( seq1.schedule( 10 ) || seq2.schedule( 5 ) );
                        completed = true;
                };

                auto h = test_seq_or( ctx ).connect( _dummy_receiver{} );
                h.start();
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
        alignas( std::max_align_t ) uint8_t buffer_storage[1024];

        std::span< uint8_t, 1024 > buffer_span{ buffer_storage };
        using it = _index_type< 1024 >;
        circular_buffer_memory< it, noop_base > mem{ buffer_span };
        using node = typename decltype( mem )::node;

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
        std::array< uint8_t, 512 >                              buffer_storage;
        std::span< uint8_t, 512 >                               buffer_span{ buffer_storage };
        circular_buffer_memory< _index_type< 512 >, noop_base > mem{ buffer_span };

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
        std::array< uint8_t, 256 >                              buffer_storage;
        std::span< uint8_t, 256 >                               buffer_span{ buffer_storage };
        circular_buffer_memory< _index_type< 256 >, noop_base > mem{ buffer_span };

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
        std::array< uint8_t, 512 >                              buffer_storage;
        std::span< uint8_t, 512 >                               buffer_span{ buffer_storage };
        circular_buffer_memory< _index_type< 512 >, noop_base > mem{ buffer_span };

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
        std::array< uint8_t, 258 >                              buffer_storage;
        std::span< uint8_t, 258 >                               buffer_span{ buffer_storage };
        circular_buffer_memory< _index_type< 258 >, noop_base > mem{ buffer_span };

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
                auto  size = mem.used_bytes();
                void* p    = mem.allocate( 8, 1 );
                if ( !p )
                        break;
                CHECK( p <= buffer_storage.data() + buffer_storage.size() );
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
        std::array< uint8_t, 256 >                              buffer_storage;
        std::span< uint8_t, 256 >                               buffer_span{ buffer_storage };
        circular_buffer_memory< _index_type< 256 >, noop_base > mem{ buffer_span };

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
                std::array< uint8_t, 200 > buffer_storage;
                std::span< uint8_t, 200 >  buffer_span{ buffer_storage };
                circular_buffer_memory< _index_type< 200 >, noop_base > small_mem{ buffer_span };
                static_assert(
                    std::is_same_v< typename decltype( small_mem )::index_type, uint8_t > );

                void* p = small_mem.allocate( 32, 1 );
                CHECK( p != nullptr );
                small_mem.deallocate( p, 32, 1 );
        }

        // Medium buffer - should use uint16_t index
        {
                std::array< uint8_t, 50000 > buffer_storage;
                std::span< uint8_t, 50000 >  buffer_span{ buffer_storage };
                circular_buffer_memory< _index_type< 50000 >, noop_base > med_mem{ buffer_span };
                static_assert(
                    std::is_same_v< typename decltype( med_mem )::index_type, uint16_t > );

                void* p = med_mem.allocate( 1000, 1 );
                CHECK( p != nullptr );
                med_mem.deallocate( p, 1000, 1 );
        }
}

TEST_CASE( "circular_buffer_memory_allocation_pattern" )
{
        std::array< uint8_t, 1024 >                              buffer_storage;
        std::span< uint8_t, 1024 >                               buffer_span{ buffer_storage };
        circular_buffer_memory< _index_type< 1024 >, noop_base > mem{ buffer_span };

        // Test allocation pattern and verify pointers are within buffer bounds
        std::vector< void* > ptrs;

        for ( int i = 0; i < 50; ++i ) {
                void* p = mem.allocate( 10, 2 );
                if ( p ) {
                        // Verify pointer is within buffer bounds
                        uintptr_t buf_start = (uintptr_t) mem._buff.data();
                        uintptr_t buf_end   = buf_start + mem._buff.size();
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
        std::array< uint8_t, 2048 >                              buffer_storage;
        std::span< uint8_t, 2048 >                               buffer_span{ buffer_storage };
        circular_buffer_memory< _index_type< 2048 >, noop_base > mem{ buffer_span };
        task_ctx                                                 ctx{ mem };

        // Create event sources for inter-task communication
        broadcast_source< set_value_t( int ) >         data_source;
        broadcast_source< set_value_t( std::string ) > message_source;

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
        auto h1 = data_processor( ctx ).connect( _dummy_receiver{} );
        h1.start();
        auto h2 = message_handler( ctx ).connect( _dummy_receiver{} );
        h2.start();
        auto h3 = data_sender( ctx ).connect( _dummy_receiver{} );
        h3.start();

        // Test execution sequence
        ctx.core.run_n( 10 );  // Initialize tasks

        // Send initial messages to trigger data sender
        message_source.set_value( "start" );
        ctx.core.run_n( 10 );

        // Send more trigger messages to continue the sequence
        message_source.set_value( "continue1" );
        ctx.core.run_n( 10 );

        message_source.set_value( "continue2" );
        ctx.core.run_n( 10 );

        // Allow final processing
        ctx.core.run_n( 10 );

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
        std::array< uint8_t, 2048 >                              buffer_storage;
        std::span< uint8_t, 2048 >                               buffer_span{ buffer_storage };
        circular_buffer_memory< _index_type< 2048 >, noop_base > mem{ buffer_span };
        task_ctx                                                 ctx{ mem };

        broadcast_source< set_value_t( int ) > trigger;
        std::vector< int >                     execution_order;
        int                                    task_counter = 0;

        // Create multiple short-lived tasks that should stress the circular buffer
        auto task = [&]( task_ctx& ctx, int task_id ) -> ecor::task< void > {
                // Wait for trigger
                std::cout << "Task " << task_id << " waiting for trigger\n";
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
        using C = connect_type< ecor::task< void >, _dummy_receiver >;
        std::list< C > tasks;
        for ( int i = 1; i <= 5; ++i )
                tasks.emplace_back( task( ctx, i ).connect( _dummy_receiver{} ) );
        for ( C& c : tasks )
                c.start();

        // Run initial setup
        ctx.core.run_n( 10 );

        // Trigger all tasks
        trigger.set_value( 1 );
        ctx.core.run_n( 10 );

        // Trigger even tasks for second execution
        trigger.set_value( 2 );
        ctx.core.run_n( 10 );

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

// ============================================================================
// Circular Buffer Memory Stress Test
// ============================================================================

// Event types for stress testing
enum class event_type
{
        alloc,        // Allocation that should succeed
        dealloc,      // Deallocation
        failed_alloc  // Allocation that should fail (buffer full)
};

struct event
{
        event_type  type;
        std::size_t size;
        std::size_t align;
        int         id;
};

struct allocated_block
{
        void*       ptr;
        std::size_t size;
        std::size_t align;
        int         id;
};

static bool is_aligned( void* ptr, std::size_t alignment )
{
        return reinterpret_cast< std::uintptr_t >( ptr ) % alignment == 0;
}

// Check if two memory blocks overlap
static bool blocks_overlap( void* ptr1, std::size_t size1, void* ptr2, std::size_t size2 )
{
        auto start1 = reinterpret_cast< std::uintptr_t >( ptr1 );
        auto end1   = start1 + size1;
        auto start2 = reinterpret_cast< std::uintptr_t >( ptr2 );
        auto end2   = start2 + size2;

        return !( end1 <= start2 || end2 <= start1 );
}

// Comprehensive sanity check for circular_buffer_memory
template < typename IndexType, typename Base >
static void sanity_check_buffer(
    circular_buffer_memory< IndexType, Base >& buffer,
    std::span< uint8_t >                       buff_span,
    std::vector< allocated_block > const&      allocated_blocks,
    std::string const&                         context )
{
        using index_type = IndexType;
        auto const npos  = circular_buffer_memory< IndexType, Base >::npos;

        // 1. Check that all allocated blocks are within buffer bounds
        auto buff_start = reinterpret_cast< std::uintptr_t >( buff_span.data() );
        auto buff_end   = buff_start + buff_span.size();

        for ( auto const& block : allocated_blocks ) {
                auto ptr_val = reinterpret_cast< std::uintptr_t >( block.ptr );
                INFO( context << ": Block " << block.id << " ptr out of bounds" );
                CHECK( ptr_val >= buff_start );
                CHECK( ptr_val + block.size <= buff_end );

                // Check alignment
                INFO( context << ": Block " << block.id << " misaligned" );
                CHECK( is_aligned( block.ptr, block.align ) );
        }

        // 2. Check that no blocks overlap
        for ( std::size_t i = 0; i < allocated_blocks.size(); ++i ) {
                for ( std::size_t j = i + 1; j < allocated_blocks.size(); ++j ) {
                        INFO(
                            context << ": Blocks " << allocated_blocks[i].id << " and "
                                    << allocated_blocks[j].id << " overlap" );
                        CHECK( !blocks_overlap(
                            allocated_blocks[i].ptr,
                            allocated_blocks[i].size,
                            allocated_blocks[j].ptr,
                            allocated_blocks[j].size ) );
                }
        }

        // 3. Validate internal linked list structure
        if ( buffer._first != npos ) {
                std::set< index_type >    visited;
                index_type                current = buffer._first;
                index_type                prev    = npos;
                std::vector< index_type > node_indices;

                // Walk forward through the list
                while ( current != npos ) {
                        INFO(
                            context << ": Circular reference or infinite loop detected at node "
                                    << (int) current );
                        CHECK( visited.find( current ) == visited.end() );
                        visited.insert( current );
                        node_indices.push_back( current );

                        // Check node is within buffer bounds
                        INFO( context << ": Node " << (int) current << " out of bounds" );
                        CHECK( current < buff_span.size() );

                        auto node = buffer._get_node( buff_span.data(), current );

                        // Check prev link consistency
                        INFO(
                            context << ": Node " << (int) current << " has inconsistent prev_idx" );
                        CHECK( node.prev_idx == prev );

                        prev    = current;
                        current = node.next_idx;
                }

                // Verify _last is correct
                INFO( context << ": _last pointer inconsistent" );
                CHECK( buffer._last == node_indices.back() );

                // Walk backward to verify bidirectional consistency
                current                        = buffer._last;
                index_type                next = npos;
                std::vector< index_type > reverse_nodes;

                while ( current != npos ) {
                        reverse_nodes.push_back( current );
                        auto node = buffer._get_node( buff_span.data(), current );

                        INFO(
                            context << ": Node " << (int) current
                                    << " has inconsistent next_idx in reverse walk" );
                        CHECK( node.next_idx == next );

                        next    = current;
                        current = node.prev_idx;
                }

                // Verify forward and backward walks visit same nodes
                std::reverse( reverse_nodes.begin(), reverse_nodes.end() );
                INFO( context << ": Forward and backward walks don't match" );
                CHECK( node_indices == reverse_nodes );
        } else {
                // If first is npos, last should also be npos
                INFO( context << ": _first is npos but _last is not" );
                CHECK( buffer._last == npos );
        }

        // 4. Verify each allocated block corresponds to a node in the linked list
        // (This is implicit in the structure, but we check that the pointers we have
        // are actually pointing into the buffer at valid locations)
        for ( auto const& block : allocated_blocks ) {
                // Calculate where the node header should be
                using buffer_type = std::remove_reference_t< decltype( buffer ) >;
                auto* node_ptr    = reinterpret_cast< uint8_t* >( block.ptr ) -
                                 sizeof( typename buffer_type::node );
                auto node_idx = node_ptr - buff_span.data();

                INFO( context << ": Block " << block.id << " node header would be out of bounds" );
                CHECK( node_idx >= 0 );
                CHECK( node_idx < buff_span.size() );
        }
}


TEST_CASE( "circular_buffer_memory stress test" )
{

        constexpr std::size_t              BUFFER_SIZE = 2048;
        std::array< uint8_t, BUFFER_SIZE > buffer_storage;
        std::span< uint8_t, BUFFER_SIZE >  buff_span{ buffer_storage };

        circular_buffer_memory< uint16_t, noop_base > buffer{ buff_span };

        std::vector< allocated_block > allocated_blocks;
        std::map< int, std::size_t >   id_to_index;  // Map allocation ID to index in vector
        int                            next_id = 0;

        auto execute_event = [&]( event const& event, int event_num ) {
                std::string context = "Event " + std::to_string( event_num );

                if ( event.type == event_type::alloc ) {
                        void* ptr = buffer.allocate( event.size, event.align );

                        INFO( context << ": Allocation should succeed but got nullptr" );
                        REQUIRE( ptr != nullptr );

                        int id = event.id >= 0 ? event.id : next_id++;
                        allocated_blocks.emplace_back( ptr, event.size, event.align, id );
                        id_to_index[id] = allocated_blocks.size() - 1;

                        // Fill with pattern for debugging
                        std::memset( ptr, 0xAA + ( id % 16 ), event.size );

                        INFO( context << ": After allocation " << id );
                        sanity_check_buffer( buffer, buff_span, allocated_blocks, context );
                } else if ( event.type == event_type::dealloc ) {
                        INFO( context << ": Deallocation ID " << event.id << " not found" );
                        REQUIRE( id_to_index.find( event.id ) != id_to_index.end() );

                        std::size_t idx   = id_to_index[event.id];
                        auto const& block = allocated_blocks[idx];

                        buffer.deallocate( block.ptr, block.size, block.align );

                        // Remove from tracking
                        allocated_blocks.erase(
                            allocated_blocks.begin() + static_cast< std::ptrdiff_t >( idx ) );
                        id_to_index.erase( event.id );

                        // Update indices in map
                        for ( auto& [id, index] : id_to_index )
                                if ( index > idx )
                                        --index;

                        INFO( context << ": After deallocation " << event.id );
                        sanity_check_buffer( buffer, buff_span, allocated_blocks, context );
                } else if ( event.type == event_type::failed_alloc ) {
                        void* ptr = buffer.allocate( event.size, event.align );

                        INFO( context << ": Allocation should fail but succeeded" );
                        auto used = buffer.used_bytes();
                        INFO( context << "Buffer usage: " << used << "/" << buff_span.size() );
                        CHECK( ptr == nullptr );

                        // State shouldn't change on failed allocation
                        INFO( context << ": After failed allocation attempt" );
                        sanity_check_buffer( buffer, buff_span, allocated_blocks, context );
                }
        };

        SUBCASE( "Sequential allocations and deallocations" )
        {
                std::vector< event > events;

                // Allocate several blocks
                events.emplace_back( event_type::alloc, 64, 8, 0 );
                events.emplace_back( event_type::alloc, 128, 8, 1 );
                events.emplace_back( event_type::alloc, 256, 16, 2 );
                events.emplace_back( event_type::alloc, 32, 4, 3 );

                // Deallocate in different order
                events.emplace_back( event_type::dealloc, 0, 0, 1 );  // Middle
                events.emplace_back( event_type::dealloc, 0, 0, 0 );  // First
                events.emplace_back( event_type::dealloc, 0, 0, 3 );  // Last
                events.emplace_back( event_type::dealloc, 0, 0, 2 );  // Remaining

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                CHECK( allocated_blocks.empty() );
        }

        SUBCASE( "Interleaved allocations and deallocations" )
        {
                std::vector< event > events;

                events.emplace_back( event_type::alloc, 100, 8, 0 );
                events.emplace_back( event_type::alloc, 100, 8, 1 );
                events.emplace_back( event_type::dealloc, 0, 0, 0 );
                events.emplace_back( event_type::alloc, 100, 8, 2 );
                events.emplace_back( event_type::alloc, 100, 8, 3 );
                events.emplace_back( event_type::dealloc, 0, 0, 1 );
                events.emplace_back( event_type::dealloc, 0, 0, 2 );
                events.emplace_back( event_type::alloc, 100, 8, 4 );
                events.emplace_back( event_type::dealloc, 0, 0, 3 );
                events.emplace_back( event_type::dealloc, 0, 0, 4 );

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                CHECK( allocated_blocks.empty() );
        }

        SUBCASE( "Fragmentation and reuse" )
        {
                std::vector< event > events;

                // Create fragmentation
                for ( int i = 0; i < 10; ++i )
                        events.emplace_back( event_type::alloc, 80, 8, i );

                // Deallocate every other block
                for ( int i = 0; i < 10; i += 2 )
                        events.emplace_back( event_type::dealloc, 0, 0, i );

                // Try to allocate into gaps
                for ( int i = 10; i < 15; ++i )
                        events.emplace_back( event_type::alloc, 70, 8, i );

                // Cleanup
                for ( int i = 1; i < 10; i += 2 )
                        events.emplace_back( event_type::dealloc, 0, 0, i );
                for ( int i = 10; i < 15; ++i )
                        events.emplace_back( event_type::dealloc, 0, 0, i );

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                CHECK( allocated_blocks.empty() );
        }

        SUBCASE( "Fill buffer to capacity" )
        {
                std::vector< event > events;

                // Allocate until buffer is nearly full
                int id = 0;
                for ( int i = 0; i < 18; ++i )
                        events.emplace_back( event_type::alloc, 100, 8, id++ );

                // Try to allocate more (should fail)
                events.emplace_back( event_type::failed_alloc, 200, 8 );

                // Deallocate one and try again
                events.emplace_back( event_type::dealloc, 0, 0, 0 );
                events.emplace_back( event_type::alloc, 80, 8, id++ );

                // Cleanup
                for ( int i = 1; i < id; ++i )
                        if ( i != 0 )
                                events.emplace_back( event_type::dealloc, 0, 0, i );

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );
        }

        SUBCASE( "Various alignment requirements" )
        {
                std::vector< event > events;

                events.emplace_back( event_type::alloc, 50, 1, 0 );   // 1-byte aligned
                events.emplace_back( event_type::alloc, 50, 2, 1 );   // 2-byte aligned
                events.emplace_back( event_type::alloc, 50, 4, 2 );   // 4-byte aligned
                events.emplace_back( event_type::alloc, 50, 8, 3 );   // 8-byte aligned
                events.emplace_back( event_type::alloc, 50, 16, 4 );  // 16-byte aligned
                events.emplace_back( event_type::alloc, 50, 32, 5 );  // 32-byte aligned

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                // Verify alignments
                for ( auto const& block : allocated_blocks )
                        CHECK( is_aligned( block.ptr, block.align ) );

                // Cleanup
                for ( int i = 0; i < 6; ++i )
                        execute_event( event{ event_type::dealloc, 0, 0, i }, 100 + i );

                CHECK( allocated_blocks.empty() );
        }

        SUBCASE( "Edge cases - very small allocations" )
        {
                std::vector< event > events;

                // Many tiny allocations
                for ( int i = 0; i < 50; ++i )
                        events.emplace_back( event_type::alloc, 1, 1, i );

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                // Cleanup
                for ( int i = 0; i < 50; ++i )
                        events.emplace_back( event_type::dealloc, 0, 0, i );

                for ( int i = 50; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                CHECK( allocated_blocks.empty() );
        }

        SUBCASE( "Edge cases - large allocations" )
        {
                std::vector< event > events;

                // One large allocation
                events.emplace_back( event_type::alloc, 1600, 8, 0 );

                // Should fail - not enough space
                events.emplace_back( event_type::failed_alloc, 500, 8 );

                // Deallocate and try smaller
                events.emplace_back( event_type::dealloc, 0, 0, 0 );
                events.emplace_back( event_type::alloc, 800, 8, 1 );
                events.emplace_back( event_type::alloc, 600, 8, 2 );

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                // Cleanup
                execute_event( event{ event_type::dealloc, 0, 0, 1 }, 100 );
                execute_event( event{ event_type::dealloc, 0, 0, 2 }, 101 );

                CHECK( allocated_blocks.empty() );
        }

        SUBCASE( "Circular wrap-around behavior" )
        {
                std::vector< event > events;

                // Fill buffer
                for ( int i = 0; i < 10; ++i )
                        events.emplace_back( event_type::alloc, 150, 8, i );

                // Deallocate from beginning
                for ( int i = 0; i < 5; ++i )
                        events.emplace_back( event_type::dealloc, 0, 0, i );

                // Allocate again - should wrap around
                for ( int i = 10; i < 15; ++i )
                        events.emplace_back( event_type::alloc, 130, 8, i );

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                // Cleanup
                for ( int i = 5; i < 10; ++i )
                        execute_event( event{ event_type::dealloc, 0, 0, i }, 100 + i );
                for ( int i = 10; i < 15; ++i )
                        execute_event( event{ event_type::dealloc, 0, 0, i }, 200 + i );

                CHECK( allocated_blocks.empty() );
        }

        SUBCASE( "Complex stress sequence" )
        {

                std::vector< event > events;
                std::vector< int >   active_ids;  // IDs of currently allocated blocks

                std::srand( 42 );  // Deterministic seed for reproducibility  // NOLINT

                constexpr int NUM_ROUNDS         = 6;
                constexpr int ALLOCS_PER_ROUND   = 5;
                constexpr int DEALLOCS_PER_ROUND = 4;

                for ( int round = 0; round < NUM_ROUNDS; ++round ) {
                        int deallocs_this_round =
                            ( round < 2 ) ? ALLOCS_PER_ROUND : DEALLOCS_PER_ROUND;
                        for ( int i = 0; i < ALLOCS_PER_ROUND; ++i ) {
                                int id = next_id++;

                                std::size_t size =
                                    32 +
                                    ( static_cast< std::size_t >( std::rand() ) % 96 );  // NOLINT

                                std::size_t align =
                                    1
                                    << ( static_cast< std::size_t >( std::rand() ) % 5 );  // NOLINT

                                events.emplace_back( event_type::alloc, size, align, id );

                                active_ids.push_back( id );
                        }

                        for ( int i = 0; i < deallocs_this_round && !active_ids.empty(); ++i ) {
                                std::size_t idx = static_cast< std::size_t >( std::rand() ) %
                                                  active_ids.size();  // NOLINT
                                int id_to_free = active_ids[idx];

                                events.emplace_back( event_type::dealloc, 0, 0, id_to_free );

                                active_ids.erase(
                                    active_ids.begin() + static_cast< std::ptrdiff_t >( idx ) );
                        }
                }

                for ( int i = 0; i < (int) events.size(); ++i )
                        execute_event( events[i], i );

                for ( int id : active_ids )
                        execute_event( event{ event_type::dealloc, 0, 0, id }, 1000 + id );

                CHECK( allocated_blocks.empty() );
        }
}

TEST_CASE( "task - pass data" )
{
        nd_mem   mem;
        task_ctx ctx{ mem };

        auto f = [&]( task_ctx& ctx ) -> ecor::task< std::vector< int > > {
                std::vector< int > data = { 1, 2, 3, 4, 5 };
                co_return data;
        };

        auto g = [&]( task_ctx& ctx ) -> ecor::task< void > {
                std::vector< int > received_data = co_await f( ctx );
                CHECK( received_data.size() == 5 );
                CHECK( received_data[0] == 1 );
                CHECK( received_data[4] == 5 );
        };

        auto h = g( ctx ).connect( _dummy_receiver{} );
        h.start();
        ctx.core.run_once();
}

TEST_CASE( "broadcast - multiple set_value" )
{
        nd_mem   mem;
        task_ctx ctx{ mem };

        broadcast_source< set_value_t( int ), set_value_t( float ) > source;


        auto f = [&]( task_ctx& ctx ) -> ecor::task< void > {
                for ( int i = 0; i < 3; ++i )
                        auto value = co_await as_variant( source.schedule() );
        };

        auto h = f( ctx ).connect( _dummy_receiver{} );
        h.start();
        ctx.core.run_n( 10 );
        source.set_value( 42 );

        ctx.core.run_n( 10 );
        source.set_value( 3.14f );

        ctx.core.run_n( 10 );
}


}  // namespace ecor
