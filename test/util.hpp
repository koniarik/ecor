#pragma once


/// MIT License
///
/// Copyright (c) 2025-2026 koniarik
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

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace ecor
{

struct _dummy_receiver
{
        using receiver_concept = receiver_t;

        template < typename... Args >
        void set_value( Args&&... ) noexcept
        {
        }
        template < typename Err >
        void set_error( Err&& ) noexcept
        {
        }

        void set_stopped() noexcept
        {
        }

        [[nodiscard]] empty_env get_env() const noexcept
        {
                return {};
        }
};

struct nd_mem
{
        void* allocate( std::size_t bytes, std::size_t align )
        {
                //      std::cout << "alloc " << bytes << " align " << align << " this: " << this <<
                //      "\n";
                return ::operator new( bytes, std::align_val_t( align ) );
        }

        void deallocate( void* p, std::size_t /*bytes*/, std::size_t align )
        {
                //   std::cout << "dealloc " << bytes << " align " << align << " this: " << this <<
                //   "\n";
                ::operator delete( p, std::align_val_t( align ) );
        }
};

/// Storage for one operation state that its own receiver destroys from inside a
/// completion. `destroy()` ends the operation state's lifetime and fills the storage with
/// `marker`. The storage stays readable, so a later write into the destroyed object shows
/// in `untouched()`, and a pointer read back from it points nowhere.
struct op_slot
{
        static constexpr unsigned char marker = 0xA5;

        op_slot() = default;

        op_slot( op_slot const& )            = delete;
        op_slot& operator=( op_slot const& ) = delete;

        /// Destroys an operation state that is still alive.
        ~op_slot()
        {
                if ( destroy_op )
                        destroy();
        }

        /// Construct the operation state that `connect()` returns. Precondition: empty.
        template < typename F >
        auto& emplace_from( F&& connect )
        {
                using op_t = decltype( connect() );
                static_assert( sizeof( op_t ) <= sizeof( bytes ) );
                static_assert( alignof( op_t ) <= alignof( std::max_align_t ) );
                destroy_op = []( void* p ) noexcept {
                        std::destroy_at( static_cast< op_t* >( p ) );
                };
                return *::new ( static_cast< void* >( bytes ) ) op_t( connect() );
        }

        /// Precondition: holds an operation state.
        void destroy() noexcept
        {
                std::exchange( destroy_op, nullptr )( bytes );
                std::memset( bytes, marker, sizeof( bytes ) );
        }

        [[nodiscard]] bool untouched() const noexcept
        {
                return std::ranges::all_of( bytes, []( unsigned char b ) {
                        return b == marker;
                } );
        }

        alignas( std::max_align_t ) unsigned char bytes[256];
        void ( *destroy_op )( void* ) noexcept = nullptr;
};

/// Receiver that logs which completion it got, then destroys its own operation state,
/// which lives in `slot`.
struct self_destroying_recv
{
        using receiver_concept = receiver_t;

        std::vector< std::string >* log;
        op_slot*                    slot;
        inplace_stop_token          token{};

        void set_value( auto&&... /*vals*/ )
        {
                log->push_back( "value" );
                slot->destroy();
        }

        void set_error( auto&& /*err*/ )
        {
                log->push_back( "error" );
                slot->destroy();
        }

        void set_stopped()
        {
                log->push_back( "stopped" );
                slot->destroy();
        }

        [[nodiscard]] stop_token_env< inplace_stop_token > get_env() const noexcept
        {
                return { token };
        }
};

}  // namespace ecor
