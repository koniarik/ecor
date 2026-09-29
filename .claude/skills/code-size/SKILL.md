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
override; confirm in `_build/debug/CMakeCache.txt`). ecor needs zll `debae37` or later
(`sh_heap::top()`); to measure against an older zll, adapt the header with
`doc/plans/zll-erase/ecor_compat.py`, as the harnesses under `doc/plans/` do.

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

### Distinct zll node types are nearly free since zll `debae37`

zll before `debae37` instantiated its list code per node type — `link_back`, `take_front`,
`detach`, the header destructor — at **~100–170 bytes per distinct node type**. That is why the
queue shares one `_queue_link` for items and waiters, and one `_queue_waiter` for consumers and
producers. zll `debae37` shares the relinking code across node types: giving the queue's items
a node type of their own now costs 0–36 bytes instead of 68–150
(`doc/plans/queue-config-size.md`), and a firmware's zll code stays flat from 4 node types to 14
(`doc/plans/zll-erase.md`). Merging node types is no longer worth bending a design for.

Where a link node is shared anyway, put a typed view over it and constrain the view with
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

### Split an operation state by what depends on the element

`push()`'s operation state did its parking, retrying and cancellation itself, so all of it was
compiled per element type. A base holding the element's descriptor and address, templated on the
queue core and the receiver type, with a derived part that only holds the element, saved 234 to
888 bytes on the README configurations with `push()` and 90 more per further queue over the same
memory resource (`doc/plans/push-op.md`). Settle at compile time whatever the element types
settle: a run-time byte-or-construct choice in the shared base pulled the byte path into a queue
of owning types only, +42.

### Merging function pointers into one tagged entry point loses

Folding three vtable function pointers into one `op(kind, ...)` with a switch costs more text
than the two pointers it saves (+4 to +34), and the text cost scales with element count just as
the rodata saving does, so it never crosses over. Replacing scalar vtable fields with a
`query(kind)` function is much worse: +77 to +150.

### Out-of-line dispatch keeps losing to inlined folds

Three separate attempts to replace a per-type inlined body with a single dispatch point all
cost more than they saved: merging vtable function pointers into one tagged entry point
(+4 to +34), a `payload(n)` helper over a repeated cast (+8 to +28), and swapping the pump's
`visit` fold for a recorded start-thunk (+12 to +156, worsening with type count).

The pattern: at `-Os` GCC inlines small per-type bodies into their caller and shares the
prologue, epilogue and setup between them. Forcing them out of line gives each its own frame
and adds the dispatch. Assume a fold beats a jump table for small bodies until measured
otherwise.

The same holds the other way round. `pop()` reached each element type's `set_value` row through
a `deliver` thunk in the node's vtable, which forced a vtable on every element type of a queue
with `pop()`; one `||` fold per queue over the descriptor's `index` replaced them and saved 94
to 112 bytes on a four-type queue, since trivially copyable types then need no vtable at all.
Keep the fold short-circuiting: a comma fold over `void` conditionals measured +18 on
cortex-m0plus.

### Narrowing scalar vtable fields is free

`uint16_t`/`uint8_t` instead of `size_t` for size and alignment: identical text, 8 bytes less
rodata per element type. Guard the limits with `static_assert`.

### A null function pointer beats an empty thunk

`destroy` is null when the payload is trivially destructible, and the caller branches. Worth −24
to −100, the best return of any single change, because it removes a whole thunk per element
type. One empty function shared by every trivially destructible type is a different thing: it
costs 2 bytes once, and lets the descriptor's `vt` pointer alone carry the decision (see
"Describe plain data with data").

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
here three changes were bundled and only one was responsible. Record a refactor that was
deliberately not taken, with its numbers, in this skill and the `doc/plans/` note, never as a
code comment: comments are for what a reader of the current code needs.

### Use `size -A`, not a line regex over `objdump -h`

Template-heavy section names get long enough that `objdump -h` wraps them onto a second line,
leaving the size in a column a naive line regex never sees. **`-w`/`--wide` does not fix this** —
it widens the name column to the longest name but the longest names still overflow. A firmware
probe with 153 `.text` sections had only 29 of them matched, reporting 544 bytes against a true
11486.

Use `arm-none-eabi-size -A` and sum by section name; it prints one section per line and never
wraps:

```sh
arm-none-eabi-size -A obj.o | awk '/^\.text/{t+=$2} /^\.rodata/{r+=$2} END{print t+r}'
```

If you must parse `objdump -h`, let the whitespace class span newlines so a wrapped entry still
matches, and cross-check against `size -A`:

```python
re.finditer(r'\n\s*\d+\s+(\S+)\s+([0-9a-f]{8})\s+[0-9a-f]{8}\s+[0-9a-f]{8}', out)
```

Sanity-check: a `.rodata` of 0 on a probe that constructs polymorphic objects means the parse is
broken, not that there are no vtables. Short-named probes are unaffected, so this lies dormant
and then corrupts exactly the large realistic measurement you care about most.

### Group symbols by *full* instantiation, not by a truncated name

A counter that buckets `zll` symbols by node type must not collapse `ll_entry<T,S...>` to
`ll_entry<…>`. Doing so merged 11 distinct instantiations into one bucket and understated the
cost of collapsing them by more than 2×. Truncate for display only, after aggregating.

### A type must be *constructed* in the probe, not just referenced

A class template's virtual functions are instantiated with its vtable, and the vtable is
emitted because the constructor needs it. Hold the object by `extern` reference and never
construct it, and the constructor, the vtable and every virtual — often where all the work
lives — silently vanish from the object file. Measuring `event_pump` that way made it appear to
make programs *smaller* than not using it.

Sanity-check every result against a prediction of its sign and rough magnitude. A `rodata` of
zero, or an addition that shrinks the binary, means the probe is wrong, not the code.

### Match the environment when comparing against a baseline

Receiver environments propagate: a receiver carrying a stop token makes the *queue's* operation
state instantiate its cancellation path too. Comparing a stoppable pump against a hand-rolled
consumer with a non-stoppable receiver therefore counts cancellation once on one side and twice
on the other. Build the baseline with the same environment as the thing under test.

### Pin the layout so published numbers cannot drift

Flash figures cannot be asserted from a host test, but the layout underneath them can, and in
practice the flash only moves when the layout does. `queue_utest.cpp` pins
`sizeof(_queue_node_vtable)`, the link/node/waiter sizes, and each core and queue configuration
in **pointer-sized units**, so the same assertions hold on the 64-bit host and a 32-bit target
(verify that claim by compiling them with the ARM toolchain — it is cheap and easy to get
wrong). Any published size table should have such a tripwire, with a comment telling the next
person to re-measure rather than just update the constant.

The tripwire does not catch a dependency's codegen changing underneath the same layout, and the
probe behind a table has to be kept, or the table cannot be re-measured at all. The README's
queue table once lost its probe, and replacing it changed the published numbers for reasons
that had nothing to do with the code; it is now `doc/plans/queue-config-size/`.

### Configuration flags earn their keep

`queue_config`'s flags are worth ~620 bytes (both stoppable flags off, with stoppable receivers)
and 254–278 (`is_closeable = false`). When a flag removes a completion path, also drop the
corresponding signature from `completion_signatures` — otherwise receivers must still implement
a completion that can never arrive.

### GCC will not fold layout-identical list instantiations — do not wait for ICF

Fourteen `zll` node types in one firmware, most of them bit-identical two-pointer lists, stayed
fourteen separate copies. `-fipa-icf` and `-fipa-icf-functions` changed the total by **0 bytes**:
the functions are address-taken (vtables, `Acc::get`) and live in per-instantiation COMDAT
groups, which is exactly the case GCC's ICF declines. Sharing has to be arranged in the source,
which is what zll `debae37` does.

### Per-type costs scale with the firmware, not the library

The full-firmware probe in `doc/plans/zll-erase/` (5 queues + pumps over 34 element types,
3 arenas, 4 task_holders) varies only the number of distinct `ll_source` signatures. Firmware on
cortex-m0plus, with zll's own code in parentheses:

| distinct `ll_source`s | node types | zll `ba49aa4` | zll `debae37` |
|---|---|---|---|
| 2  | 4  | 10602 (874)  | 9856 (348)  |
| 4  | 6  | 11254 (1122) | 10144 (348) |
| 8  | 10 | 12974 (1618) | 11040 (348) |
| 12 | 14 | 14508 (2016) | 11986 (348) |

On `ba49aa4` every extra node type added ≈114 bytes of list code (≈107 on cortex-m4), so the
cost of not sharing grew with the number of intrusive-node types — the thing that grows as a
firmware grows, and that a small library measurement always understates. zll's list no longer
behaves like this; any other per-type machinery still does, so measure it at firmware scale.

### Erasing the list, not the node, is the way to share it

Merging node types under a shared base cannot work where a type legitimately belongs to two
lists: `event_pump` is both a `schedulable` and a `_queue_waiter`, and one shared base would give
it two ambiguous `ll_header` subobjects (the `std::derived_from` guard correctly rejects this).
Measured in isolation, that blocker makes base-merging worth **0** in the default queue+pump
configuration.

Type-erasing the list *implementation* instead has no such limit: a type keeps as many distinct
link subobjects as it needs while all of them share one copy of the code. `ll_header` is two
tagged pointers for every `T`, and every algorithm (`detach`, `link_detached_as_*`,
`_prev_or_last_set`) is header-to-header, so an untyped core plus inline `static_cast` wrappers
is representable.

A prototype did exactly that: the six functions that go out of line per node type —
`~ll_header`, `detach`, `_prev_or_last_set`, `_next_or_first_set`, `ll_list::link_back`,
`ll_list::detach_nodes` — call `[[gnu::noinline]]` untyped helpers taking the node→header
offset as a runtime argument (`doc/plans/zll-erase/patch.py`). zll `debae37` then shipped its
own version, where links and list ends hold header addresses. Firmware probe at 12 sources:

| | cortex-m0plus | cortex-m4 |
|---|---|---|
| zll `ba49aa4` | 14508 | 13560 |
| prototype | 12246 (−2262) | 11680 (−1880) |
| zll `debae37` | **11986 (−2522)** | **11442 (−2118)** |

zll's own code is about the same size in both (342 bytes, 348 in `debae37`); `debae37` wins
in the ecor code the list operations inline into. Both also beat collapsing everything to one
instantiation, because erasure removes the copies GCC had inlined into call sites, which
counting out-of-line symbols never sees. What stays typed under `debae37` is an out-of-line
`ll_list<T>::link_back` for 4 of 14 node types, 34–40 bytes each. `doc/plans/zll-erase.md` has
the full sweep and two traps from building the prototype.

The sibling-heap side (`seq_source`, `zll::sh_*`) measured a smaller multiplier on zll before
`debae37` — three distinct `seq_source`s cost 942 bytes, of which 270 was heap machinery — and
`debae37` shares heap code too. Not re-measured since.


### Link a real firmware probe when the question is "what does a program pay"

Object-file probes measure a library in one translation unit. A firmware pays for what
happens *across* translation units and after `--gc-sections`, and neither shows up there.
`doc/plans/queue-fw-size/` is a linked probe (generated producers over 10 TUs, `-nostartfiles`,
nano libc, a no-op `__cxa_atexit`, `cmake --workflow --preset m4`) — start from it.

It copies the header at configure time, as do the base and split variants of `queue-config-size`
and `zll-erase`: after editing the header, re-run the configure step, or the build measures the
old copy.

Two traps it found:
- A `const volatile` table of function pointers that nothing reads is garbage-collected, and
  with it everything it pointed at. Index the table from `_start` with a value loaded from a
  volatile address.
- A global with a non-trivial destructor registers it through `__cxa_atexit`, which drags the
  destructor and libc's atexit into a firmware that never exits. Stub `__cxa_atexit` and
  `__dso_handle`, and count what remains as the design's cost.

### GCC clones COMDAT helpers per translation unit — check `nm` for `.isra` and `.constprop`

An out-of-line member of a class template is COMDAT and folds across TUs — until IPA-SRA or
IPA-CP rewrites it into a local `.isra.0` / `.constprop.0` clone, which is per TU and never
folds. `_queue_core::_alloc_node` called from 10 producer TUs became **10 × 220 bytes**, the
largest item in a non-LTO firmware. Changing the parameter list did nothing (the clone was
dropping `this`); `[[gnu::noclone]]` fixed it (−2172), and costs +92 under LTO where the
`constprop` clone was actually useful. Best is to leave such a helper with a single caller
so there is nothing to clone. LTO hides the whole class of problem, so measure without it too.
zll has the same exposure: `ll_list<T>::take_front()` becomes a local 60-byte `.isra.0` clone
in the non-LTO firmware probe, where one translation unit calls it.

So do not reach for `noclone` by default. `_queue_core::_push_bytes()`, called from 50 sites in
10 translation units, is never cloned per unit — it needs its whole `this`, so IPA-SRA has
nothing to split — and one copy exists without any attribute. `noclone` on it changed nothing
without LTO and cost +188 bytes with LTO, where the clone that folds the one global queue's
address saves an argument at every call site. `noinline` alone made no difference: at `-Os`
GCC does not inline a function that size into many callers. Look for `.isra`/`.constprop`
copies in a non-LTO build first, and add the attribute only where they appear.

The copies need not carry a suffix. Without `ECOR_FORCE_INLINE` on `async_queue::_push()`, the
helper that picks the byte copy or the element's `construct`, a non-LTO firmware pushing a mix
of trivially copyable and owning types through a template wrapper held eight copies of the
220-byte `_alloc_node()`, +1256 bytes; count every symbol by name in `nm`, not only the suffixed
ones.

### Per-call-site cost is the one that scales with the firmware, not the library

`try_push<T>` inlined at each site costs 43 bytes per site (LTO, cortex-m4) over a call to a
non-template `push_raw(id, ptr, n)` — 2.1 KB of a 3.8 KB delta on a 50-site probe, more than all
per-type vtables and thunks together. Two changes address it in `doc/plans/size-audit.md`:
`try_push<T>` of a trivially copyable `T` calling one out-of-line core function, and a
`push_bundle` that lets a wrapper around the queue be a non-template function. With both
applied, a push site through the non-template wrapper measured 36 bytes against 68 through a
template wrapper (30 against 54 with LTO): −1592 on the 50-site firmware probe without LTO,
−1016 with it.

The shape of an erased entry matters at every site: GCC materialises a small aggregate argument
on the stack even when it fits in registers, so `try_push(push_bundle const&)` with a two-word
`{descriptor*, src}` bundle costs 3 B/site over three scalar arguments, by value 5 B/site, and a
three-word bundle 7–9 B/site. Keep erased-call bundles small and pass them by `const&`.

A typed entry point that forwards to the erased one must be forced inline: `try_push(T)` calling
an out-of-line `try_push(push_bundle const&)` cost +18 to +40 bytes per queue and +176 / +256 on
the thread firmware, because every site builds the bundle in memory and makes one more call;
with `ECOR_FORCE_INLINE` on the bundle overload it is byte-identical
(`doc/plans/push-erase.md`).

Measure the scenario an API exists for. Erasing the construction of elements that are not
trivially copyable looked like a loss on direct pushes, where a small move constructor inlined
at the site is cheap, but `push_bundle` exists for wrappers, and there it saved 4896 / 1052
bytes (LTO off / on) on a firmware with 40 move-only message types. Put the `construct` thunk in
the vtable and let every push use it, or thunks nothing calls cost about 26 bytes per type;
choose between it and the byte copy in a forced-inline helper, so the choice folds where the
type is known; and skip the choice with `if constexpr` in a queue of trivially copyable types
(`doc/plans/push-erase.md`).

Hand an element to the push by reference and move from it only once the node is allocated.
Taking it by value moved it out even when the allocation then failed, which lost the element of
a parked `push()` (it delivered a moved-from `std::string`), and it cost a move and a destructor
at every site: by reference measured −20 to −122 bytes wherever such types or `push()` are used,
−36 / −24 on the thread firmware.

### Audit the whole library before picking targets

`doc/plans/size-audit/` (local, ignored by git) builds one probe module per abstraction, alone
at 1, 2, 4 and 8 instances, in every pair, and in every subset of ten core modules — 1216 probes
per core. `cmake --workflow --preset audit` measures the current header, `variants` compares
patch variants on the singles and scaling probes in a few minutes, and `compare.py` diffs two
runs. `doc/plans/size-audit.md` has the findings.

Three things made the numbers trustworthy:
- Each module's use sites live in their own `[[gnu::noinline]]` function, so the library code
  inlined into them is attributed to that module rather than to one big `probe()`.
- Attribution is by section, not by `nm` symbol: in an object file every symbol sits at address
  0, so deduplicating by address throws away almost everything, and with
  `-ffunction-sections -fdata-sections` each section is exactly one symbol, aliases once.
- Each module uses payload types of its own, so modules share only library code and never a
  user-typed instantiation.

Sharing is many-way, not pairwise. Base costs minus pairwise overlaps mispredicted combinations
by 668 bytes on average; the union of the modules' symbols predicted them within 142.

### Coroutine frames are GCC's cost, not the library's

A trivial `task<void>` coroutine costs ~265 bytes on cortex-m0plus — ramp (allocation, promise
construction, GCC's frame refcount teardown), resume function (resume-index dispatch, body,
return, final suspend) and destroy function — and each `co_await` adds ~120. Moving promise
pieces out of line barely touches it: constructor out of line −8 per coroutine for +20 once,
`final_suspend` and the continuation calls −1 per coroutine for +26 once, `_alloc` 0. Only
replacing a 4-byte `memcpy` of the memory-resource pointer with an aligned store is worth
doing (−22 once). Expect per-coroutine cost to scale with the firmware and budget for it.

### Comparator-driven code is per node type: share the node type per key

zll `debae37` shares its relinking code, but its skew-heap merge and pop call the comparator and
stay per node type — 130 bytes per `seq_source` type. Giving every entry with key type `K` one
heap node, `_sh_link<K>`, which holds the key, makes that once per key type: measured −174 per
further `seq_source` type on cortex-m0plus, −158 on cortex-m4. The typed view over the shared
node is the same pattern as `_queue_list`.

### A virtual destructor on an internal base costs every derived type

Stop callbacks are never destroyed through their base, yet the base destructor is virtual, so
every callback type carries a deleting destructor and two more vtable slots. Making it protected
and non-virtual measured −60 once, −53 per callback type.

### Describe plain data with data

A trivially copyable element type needs no vtable, with `pop()` or without it: a descriptor
(sizes, payload offset and size, variant index) can drive one shared byte-wise push and take,
and `pop()` delivers it by the descriptor's `index`. Measured with the shared push and an 8-byte
descriptor, per element type the queue goes from 72 bytes to 30; the `vt` pointer below adds 4.
Make the choice per element type, not per queue: a queue that mixes both kinds then pays for
vtables only where they are needed, and telling the kinds apart when taking is one test of the
descriptor's `vt`. Measured −54 on a queue of eight trivially copyable types and one that is
not. Store derived constants such as the payload offset in the descriptor's padding instead of
recomputing them at each use. Re-measured with the `vt` pointer (`doc/plans/desc-fields.md`):
deriving `offset` from `align` saves nothing, as its byte would be padding, and costs +12 to
+164 of text; deriving `size` as well gets the descriptor back to 8 bytes but costs about 46
bytes once per queue core and 37 per further queue, which only one queue with dozens of element
types recovers.

The descriptor points at its vtable through `vt`, null when the element type needs no
operations, and `destroy` is never null, so `vt` is the only test anywhere and nothing is
downcast. That is a deliberate trade for a straightforward design: a vtable extending the
descriptor, with a `bool` in the descriptor's padding saying it is one, keeps the descriptor at
8 bytes instead of 12 and measured 4 bytes smaller per element type in every queue, +160 for the
pointer on a 40-type firmware and +16 on a four-type queue (`doc/plans/desc-ptr.md`). A nullable
`destroy` would add a second test in `_release()`, 2 to 4 bytes.
