---
name: code-size
description: >-
  Measure and reduce ARM code size (.text/.rodata) for ecor's header-only templates. Use
  whenever considering a layout, vtable, type-erasure or template-structure change, or when
  asked "is X smaller", "which of these is more efficient", or to optimize for embedded
  footprint. Provides the probe-and-measure harness plus the size lessons already learned in
  this repo.
---

# Code size for ecor

ecor targets microcontrollers, so flash is the scarce resource and almost every design
question eventually becomes "what does this cost in `.text` + `.rodata` on a Cortex-M?".

**Never answer that from reasoning alone.** Every non-obvious prediction made in this repo has
been wrong at least once, in both directions. Build the variants and measure them.

## The process

1. **Write a probe** — a single TU that instantiates the abstraction and *uses* every operation
   you care about, with `extern` declarations for everything else so nothing is optimized away.
2. **Generate header variants** — copy `include/ecor/ecor.hpp` into per-variant directories and
   patch each with a Python script of exact string replacements, each guarded by an `assert` on
   the match count. Never hand-edit variants; clang-format reflows the header constantly and
   silent non-matches produce fake results.
3. **Compile each variant × probe × core** with CMake and `arm-none-eabi-g++` at `-Os`, as
   object files only. No linking, so no linker script and no startup code in the numbers.
4. **Sum `.text` and `.rodata`** from `arm-none-eabi-objdump -h`, and compare.
5. **Apply the winner to the real header**, then rebuild and run the host test suite —
   `cmake --build --preset debug && ./build/test/ecor_tests` — plus asan and ubsan.
6. **Record the numbers** in the relevant `doc/plans/*.md`, including rejected options. Future
   readers will otherwise re-derive the same experiment, or "simplify" a shape that was chosen
   for measured reasons.

### Harness

Toolchain file:

```cmake
set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR arm)
set(CMAKE_C_COMPILER   arm-none-eabi-gcc)
set(CMAKE_CXX_COMPILER arm-none-eabi-g++)
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
```

Per-target flags:

```cmake
target_compile_options(${TGT} PRIVATE
  -Os -mcpu=${CPU} -mthumb -fno-exceptions -fno-rtti
  -ffunction-sections -fdata-sections -fno-threadsafe-statics)
```

Measurement (BSD awk has no `strtonum`, so use Python):

```python
out = subprocess.run(["arm-none-eabi-objdump","-h",obj],capture_output=True,text=True).stdout
text = rodata = 0
for m in re.finditer(r'^\s+\d+\s+(\S+)\s+([0-9a-f]+)', out, re.M):
    name, size = m.group(1), int(m.group(2), 16)
    if   name.startswith('.text'):   text   += size
    elif name.startswith('.rodata'): rodata += size
```

`zll` comes from `/Users/veverak/Projects/zll/include` (a local `FETCHCONTENT_SOURCE_DIR_ZLL`
override; confirm in `build/CMakeCache.txt`).

Note the `Makefile` once had `size`/`size-m0` targets and `scripts/size_report.sh`. The targets
and presets were removed; the script survives and is a good starting point if a permanent
harness is ever restored.

### Measure more than one scenario

Costs fall into two very different classes, and a single scenario cannot tell them apart:

- **Per-instantiation** — paid once per distinct template instantiation. Flat across element
  counts. Sharing a base or hoisting a function removes it.
- **Per-element-type** — paid per type in the pack. Scales. Only shrinking the per-type work
  removes it.

So always measure at least: one element type, four, eight, and *several queues/sources over one
memory resource*. A change can be neutral on one queue and worth 90 bytes across three — that
happened with `_emplace` hoisting, which looked marginal until the multi-queue probe ran.

Measure on both `cortex-m0plus` and `cortex-m4`. Conclusions have always agreed; magnitudes
differ by 10–20%, and M4's better addressing modes sometimes flip a small result's sign.

## What we have learned

### `zll` list instantiations are the single most expensive thing

Each distinct `zll::ll_base<T>` drags in its own `link_back`, `take_front`, `detach` and header
destructor: **~100–170 bytes per distinct node type**, flat.

- Merging the queue's consumer and producer nodes: −128 to −172.
- Merging the queue's item and waiter nodes under one `_queue_link`: −98 to −124.
- Each additional distinct `ll_source<T,S...>` costs 216–239 bytes, of which the list machinery
  is the shareable ~100–130.

**Share one link node and put a typed view over it.** Constrain the view with
`std::derived_from<T, Base>` — it is defined via the pointer conversion, so it fails to compile
when `T` reaches `Base` twice, which would otherwise make `ll_header` ambiguous and silently
corrupt a list. See `_queue_list` and its test.

### Type erasure is what you want, but erase the *right* layer

Keep machinery in a layer templated on as little as possible, and let the typed facade be thin.
`_queue_core<Mem, Closeable>` holds the lists, waiter handling and shutdown state and knows
nothing of element types, so queues differing only in their elements share all of it.

Corollary: when a type-erased thunk needs to downcast, it must be able to *name* the layer it
downcasts to. Keep a parameter-free intermediate for that (`_queue_consumer_node`), and add
CRTP-flavoured layers only *above* it.

### Hoisting non-template code out of templates

Moving the parts of a function template that don't depend on the template parameter into a
non-template member trades inlining for sharing. It loses on one small instantiation and wins as
instantiations multiply — `_emplace`'s allocate/link/service tail measured +2/+8 for one queue
with four types, and −88/−80 across three queues.

### Merging function pointers into one tagged entry point loses

Folding three vtable function pointers into one `op(kind, ...)` with a switch costs more text
than the two pointers it saves (+4 to +34), and the text cost scales with element count just as
the rodata saving does, so it never crosses over. Replacing scalar vtable fields with a
`query(kind)` function is much worse: +77 to +150.

### Narrowing scalar vtable fields is free

`uint16_t`/`uint8_t` instead of `size_t` for size and alignment: identical text, 8 bytes less
rodata per element type. Guard the limits with `static_assert`.

### A null function pointer beats an empty thunk

`destroy` is null when the payload is trivially destructible, and the caller branches. Worth
−24 to −100, the best return of any single change, because it removes a whole thunk per element
type.

### Conditional virtuals need structure, and always cost something to fake

- A virtual function **may not carry a requires-clause**, and `override` may not be combined
  with one. To make an override conditional, specialize a CRTP mixin
  (`_queue_finish_mixin`-style) that supplies it for one configuration and vanishes for the
  other.
- **A virtual is always instantiated**, even when unreachable. Its body must not require APIs
  the configuration has stopped promising — an `if constexpr`-empty body is the fix. This was a
  real bug: `_stop()` demanded `set_stopped()` from receivers after the completion signatures
  had dropped it.
- Fusing several virtuals into one `_advance()`-style "make progress" call can be worth it more
  for the structure it deletes than for its own −4 to −24.

### Templates are lazy; data members are not

Unused member functions of a class template cost nothing, so `close()` being unused is already
free. Data members are always present, so gating those (via `std::conditional_t<..., unit>`)
is what actually recovers the space.

### Readability refactors are not automatically free — measure them too

Replacing lambdas in a vtable initialiser with named static member functions, and binding an
erased pointer to a local reference before using it, are both byte-for-byte free. Factoring a
repeated `static_cast<node_type&>(n)._val` into a one-line `payload(n)` helper is not: +8 on
cortex-m0plus and +28 on cortex-m4 for a four-element-type queue. GCC at `-Os` folds the
reference binding but not the tiny function.

So when tidying hot template code, measure the tidy-up, and isolate *which* part of it costs —
here three changes were bundled and only one was responsible. Leave a comment where a natural
refactor was deliberately not taken, or it will simply be reapplied later.

### Pin the layout so published numbers cannot drift

Flash figures cannot be asserted from a host test, but the layout underneath them can, and in
practice the flash only moves when the layout does. `queue_utest.cpp` pins
`sizeof(_queue_node_vtable)`, the link/node/waiter sizes, and each core and queue configuration
in **pointer-sized units**, so the same assertions hold on the 64-bit host and a 32-bit target
(verify that claim by compiling them with the ARM toolchain — it is cheap and easy to get
wrong). Any published size table should have such a tripwire, with a comment telling the next
person to re-measure rather than just update the constant.

### Configuration flags earn their keep

`queue_config`'s flags are worth ~620 bytes (both stoppable flags off, with stoppable receivers)
and 254–278 (`is_closeable = false`). When a flag removes a completion path, also drop the
corresponding signature from `completion_signatures` — otherwise receivers must still implement
a completion that can never arrive.
