
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
                std::cout << "alloc " << bytes << " align " << align << " this: " << this << "\n";
                return ::operator new( bytes, std::align_val_t( align ) );
        }

        void deallocate( void* p, std::size_t bytes, std::size_t align )
        {
                std::cout << "dealloc " << bytes << " align " << align << " this: " << this << "\n";
                ::operator delete( p, std::align_val_t( align ) );
        }
};

TEST_CASE( "base" )
{
        nd_mem                     mem;
        event_source< int, float > es;
        int                        y = -1;

        auto f = [&]( memory_resource auto& ) -> ecor::task< void > {
                for ( ;; ) {
                        int x = std::get< 0 >( co_await es.schedule() );
                        y     = x;
                }
        };

        task< void > h = f( mem );
        // XXX: unpleasant?
        h.resume();

        int value = 42;
        es.set_value( value );
        CHECK( value == y );

        value = 666;
        es.set_value( value );
        CHECK( value == y );
}

};  // namespace ecor
