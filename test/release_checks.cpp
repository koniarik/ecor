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

// Compile-time checks of a build without `ECOR_DEFAULT_ASSERT`.

#include "ecor/ecor.hpp"
#include "util.hpp"

#include <type_traits>

namespace ecor
{

static_assert( std::is_trivially_destructible_v< task< void, _owning_cfg >::promise_type > );
static_assert( std::is_trivially_destructible_v< task< int, _owning_cfg >::promise_type > );
static_assert(
    std::is_trivially_destructible_v< task< void, _owning_debug_parent_cfg >::promise_type > );
static_assert( !std::is_trivially_destructible_v< task< void >::promise_type > );

}  // namespace ecor
