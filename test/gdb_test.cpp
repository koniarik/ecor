/// MIT License
///
/// Copyright (c) 2026 koniarik
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

#include "ecor/ecor.hpp"
#include "util.hpp"

#include <cassert>
#include <fstream>
#include <iostream>
#include <source_location>
#include <sstream>
#include <string>
#include <string_view>

// ---------------------------------------------------------------------------
// Check infrastructure (same pattern as zll/test/gdb_bin.cpp)
// ---------------------------------------------------------------------------

enum class mode
{
        gen,
        run,
        eval
};

void check_impl(
    mode                              m,
    std::string_view                  var,
    [[maybe_unused]] std::string_view expected,
    std::ostream&                     out,
    std::source_location              sl = std::source_location::current() )
{
        if ( m != mode::gen )
                return;
        out << "break " << sl.file_name() << ":" << sl.line() << "\n";
        out << "commands\n";
        out << "p " << var << "\n";
        out << "c\n";
        out << "end\n";
}

void check_impl(
    [[maybe_unused]] mode             m,
    [[maybe_unused]] std::string_view var,
    std::string_view                  expected,
    std::istream&                     in,
    std::source_location              sl = std::source_location::current() )
{
        assert( m == mode::eval );
        while ( in && in.get() != '$' ) {
        }

        std::string first_line;
        std::getline( in, first_line );

        // strip "N = " prefix from the first line
        auto             sep         = first_line.find( " = " );
        std::string_view first_value = sep != std::string::npos ?
                                           std::string_view( first_line ).substr( sep + 3 ) :
                                           std::string_view( first_line );

        // Accumulate continuation lines (lines that don't start a new GDB prompt)
        std::string full_value{ first_value };
        std::string next_line;
        while ( std::getline( in, next_line ) ) {
                if ( next_line.empty() )
                        break;
                if ( next_line[0] == '$' || next_line[0] == '(' || next_line[0] == '[' )
                        break;
                full_value += '\n';
                full_value += next_line;
        }

        if ( full_value == expected )
                return;
        std::cerr << "Failed match:\n";
        std::cerr << "Expected: " << expected << "\n";
        std::cerr << "     Got: " << full_value << "\n";
        std::cerr << "  Source: " << sl.file_name() << ":" << sl.line() << "\n";
        std::exit( 2 );
}

#define CHECK( m, var, expected, out ) check_impl( m, #var, expected, out )

// ---------------------------------------------------------------------------
// Test infrastructure
// ---------------------------------------------------------------------------

static ecor::_promise_base* g_captured  = nullptr;
static ecor::_promise_base* g_captured2 = nullptr;

struct capture_trace : ecor::task_default_trace
{
        void on_promise_construct( auto& p, auto&, auto&&... ) noexcept
        {
                g_captured = &p;
        }
};

struct capture_trace2 : ecor::task_default_trace
{
        void on_promise_construct( auto& p, auto&, auto&&... ) noexcept
        {
                g_captured2 = &p;
        }
};

struct test_cfg : ecor::task_default_cfg
{
        using trace_type = capture_trace;
};

struct test_cfg2 : ecor::task_default_cfg
{
        using trace_type = capture_trace2;
};

using test_task  = ecor::task< void, test_cfg >;
using test_task2 = ecor::task< void, test_cfg2 >;

// S1/S2: coroutine suspends once via co_await ecor::suspend then returns.
static constexpr int SIMPLE_CORO_LINE = __LINE__ + 1;
static test_task     gdb_coro_simple( ecor::task_ctx& )
{
        co_await ecor::suspend;
        co_return;
}

// S3: inner coroutine awaited by outer — tests _debug_parent chain.
// gdb_coro_inner suspends waiting for outer to drive it via ll_source.
static ecor::ll_source< ecor::unit, ecor::set_value_t() >* g_inner_src = nullptr;

static constexpr int INNER_CORO_LINE = __LINE__ + 1;
static test_task     gdb_coro_inner( ecor::task_ctx& )
{
        co_await g_inner_src->schedule();
        co_return;
}

static constexpr int OUTER_CORO_LINE = __LINE__ + 1;
static test_task2    gdb_coro_outer( ecor::task_ctx& ctx )
{
        co_await gdb_coro_inner( ctx );
        co_return;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

void run_tests( mode m, auto& st )
{
        std::string expected = std::string( "ecor::_promise_type [gdb_coro_simple()] @ " ) +
                               __FILE__ + ":" + std::to_string( SIMPLE_CORO_LINE );

        // S1: promise at initial_suspend (coroutine created, not yet resumed)
        {
                ecor::nd_mem   mem;
                ecor::task_ctx ctx{ mem };
                g_captured = nullptr;
                auto op    = gdb_coro_simple( ctx ).connect( ecor::_dummy_receiver{} );
                op.start();
                assert( g_captured != nullptr );
                [[maybe_unused]] auto* p =
                    static_cast< ecor::_promise_type< test_task >* >( g_captured );
                CHECK( m, *p, expected, st );
        }

        // S2: promise re-queued after co_await ecor::suspend
        {
                ecor::nd_mem   mem;
                ecor::task_ctx ctx{ mem };
                g_captured = nullptr;
                auto op    = gdb_coro_simple( ctx ).connect( ecor::_dummy_receiver{} );
                op.start();
                assert( g_captured != nullptr );
                [[maybe_unused]] auto* p =
                    static_cast< ecor::_promise_type< test_task >* >( g_captured );
                ctx.core.run_once();  // advance past initial_suspend → suspended at co_await
                                      // ecor::suspend
                CHECK( m, *p, expected, st );
                ctx.core.run_once();  // finish coroutine
        }

#ifdef ECOR_DEBUG_PARENT
        // S3: inner promise while suspended inside outer — _debug_parent chain
        {
                ecor::ll_source< ecor::unit, ecor::set_value_t() > src;
                g_inner_src = &src;

                ecor::nd_mem   mem;
                ecor::task_ctx ctx{ mem };
                g_captured  = nullptr;
                g_captured2 = nullptr;

                auto op = gdb_coro_outer( ctx ).connect( ecor::_dummy_receiver{} );
                op.start();
                ctx.core.run_once();  // outer: past initial_suspend → calls gdb_coro_inner
                ctx.core.run_once();  // inner: past initial_suspend → co_await src.schedule() — now
                                      // suspended with _debug_parent set

                // g_captured  = inner promise, g_captured2 = outer promise
                assert( g_captured != nullptr && g_captured2 != nullptr );
                [[maybe_unused]] auto* inner_p =
                    static_cast< ecor::_promise_type< test_task >* >( g_captured );

                std::string inner_expected =
                    std::string( "ecor::_promise_type [gdb_coro_inner()] @ " ) + __FILE__ + ":" +
                    std::to_string( INNER_CORO_LINE ) +
                    "\n    <- awaited by [gdb_coro_outer()] @ " + __FILE__ + ":" +
                    std::to_string( OUTER_CORO_LINE );
                CHECK( m, *inner_p, inner_expected, st );

                ecor::broadcast( src, []( auto& e ) {
                        e.set_value();
                } );                  // unblock inner
                ctx.core.run_once();  // inner completes, outer resumes
                ctx.core.run_once();  // outer completes
                g_inner_src = nullptr;
        }
#endif
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

int main( [[maybe_unused]] int argc, char* argv[] )
{
        assert( argc >= 2 );
        std::string_view mode_str = argv[1];

        if ( mode_str == "gen" ) {
                assert( argc >= 5 );
                std::string_view pprinter   = argv[2];
                std::string_view gdb_script = argv[3];
                std::string_view log_path   = argv[4];
                std::ofstream    out{ std::string( gdb_script ) };
                out << "source " << pprinter << "\n";
                out << "set logging file " << log_path << "\n";
                out << "set logging overwrite on\n";
                out << "set logging redirect on\n";
                out << "set logging enabled on\n";
                out << "set width 0\n";
                out << "set print max-depth 2\n";
                run_tests( mode::gen, out );
                out << "run run\n";
        } else if ( mode_str == "run" ) {
                std::ostringstream ss;
                run_tests( mode::run, ss );
        } else if ( mode_str == "eval" ) {
                assert( argc >= 3 );
                std::ifstream inpt{ argv[2] };
                run_tests( mode::eval, inpt );
        } else {
                std::cerr << "invalid mode\n";
                return 1;
        }
}
