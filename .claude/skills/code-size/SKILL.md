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

`test/size/` is the harness. `python3 test/size/size.py --full` builds every probe family for
cortex-m4 and writes `test/size/build/report.md`; `--against REF` compares the working tree's
header with a commit's; `update-baseline` records the figures for the toolchain in use, and a
later run fails on a probe that grew past the tolerance. A new measurement goes in as a probe
or a family, see `test/size/README.md`, so that its numbers can be re-run; `explain PROBE
--against REF` prints the section diff behind a delta.

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

Measurement, from `size -A`, which prints one section per line in decimal (see "Use `size -A`"
below for why not `objdump -h`):

```python
out = subprocess.run(["arm-none-eabi-size","-A",obj],capture_output=True,text=True).stdout
text = rodata = 0
for line in out.splitlines():
    f = line.split()
    if len(f) >= 2 and f[0].startswith('.text'):     text   += int(f[1])
    elif len(f) >= 2 and f[0].startswith('.rodata'): rodata += int(f[1])
```

`zll` comes from a local zll checkout (a `FETCHCONTENT_SOURCE_DIR_ZLL`
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

Measure a change to shared template code against the previous header on programs that do not use
what changed, too: the README configurations and the thread and coroutine firmware. A feature's
own probes cannot show what it costs everyone else.

The suite measures `cortex-m4` only. The M0+ figures in the lessons below are from before that
decision; the two cores always agreed on the conclusion, with magnitudes 10–20% apart.

## What we have learned

### How GCC 15 decides what to inline at `-Os`

At `-Os` every call counts as cold (`cgraph_edge::maybe_hot_p`), so the speed budgets never
apply. The early inliner takes only calls whose inlined body is no bigger than the call. The IPA
inliner takes a call that grows the code only if inlining the callee at all its calls in the
translation unit costs no more than the out-of-line copy it removes. A template or `inline`
function's copy is credited at 80% of its body (`comdat-sharing-probability`, default 20), a
local function's, and every function's under LTO, in full. Sizes are GCC's units, about one per
statement; a call costs 1, plus 1 per argument and 1 for a used result.

Two phases take such calls. The small-function heap comes first, and its quick pre-check in
`growth_positive_p` (`ipa-inline-analysis.cc`, GCC 15 and trunk) counts every call it has already
estimated at its inlined size instead of its growth: a function with body B, growth g per call and
call size s is taken at all N calls of a file iff N·g + (N−1)·s ≤ 0.8·B. A second phase then
inlines a function at all its calls iff N·g ≤ 0.8·B, without the overcount, but only when every
one of them is allowed: no caller may pass 4000 units after inlining
(`max-inline-functions-called-once-insns`), and no call may sit deeper than loop depth 6. One
large caller blocks the function at all its calls. `await_suspend` (8, 1, 3) is inlined with one
or two calls in a file, with three to six only where no caller is near 4000 units (a third of
them in the 1000-coroutine program, whose largest coroutines have 100 to 140 awaits), and never
with seven. What follows from that:

- A small template instantiated per type has one or two calls per file and is inlined. Sharing
  it across many calls keeps it out of line, and splitting it per type pulls it back in, so a
  change to how many calls a small function has in a file flips its inlining.
- The counts are per translation unit: a single-file probe, a program of many files and an LTO
  build, which is one unit counting each copy in full, decide differently.
- A call with fewer arguments is cheaper, so inlining its callee looks like more growth.

Fixing the overcount in a rebuilt 15.3.1 grew the 1000-coroutine program without LTO by 2.0%
and shrank single-file probes by up to 2.5%: the first phase, which has no 4000-unit limit on the
caller, then also inlines into the large coroutines, and each file credits itself a copy that the
linker keeps anyway for another file. `--param=comdat-sharing-probability=80` shrinks the programs
built without LTO by 0.7–5.8% and grows the single-file probes by 4.3%; under LTO it changes
nothing (`doc/plans/gcc-inline-overcount.md`). A decision measured only on single-file probes
can therefore reverse in a firmware of many files built without LTO: check the `program`
family. `size.py explain PROBE --variant FILE:NAME --why` lists the functions whose number of
inlined calls changed, with body size and growth per call.

### Distinct zll node types are nearly free since zll `debae37`

zll before `debae37` instantiated its list code per node type — `link_back`, `take_front`,
`detach`, the header destructor — at **~100–170 bytes per distinct node type**. That is why the
queue shares one `_queue_link` for items and waiters, and one `_queue_waiter` for consumers and
producers. zll `debae37` shares the relinking code across node types: giving the queue's items
a node type of their own now costs 0–36 bytes instead of 68–150
(`doc/plans/queue-config-size.md`), and a firmware's zll code stays flat from 4 node types to 14
(`doc/plans/zll-erase.md`). Merging node types is no longer worth bending a design for, with one
exception: each node type's list still has its own `link_back`, inlined where it is called, so
many types that each link from their own operation state's `start()` pay it per type (see "One
link function for every `ll_source` entry").

Where a link node is shared anyway, put a typed view over it and constrain the view with
`std::derived_from<T, Base>` — it is defined via the pointer conversion, so it fails to compile
when `T` reaches `Base` twice, which would otherwise make `ll_header` ambiguous and silently
corrupt a list. See `_typed_list` and its test.

### Type erasure is what you want, but erase the *right* layer

Keep machinery in a layer templated on as little as possible, and let the typed facade be thin.
`_queue_core_base< Closeable >` holds the lists, waiter handling and shutdown state and knows
nothing of element types or of the memory resource; `_queue_core< Mem, Closeable >` adds only
allocation and release. Queues differing only in their elements share all of it, and queues over
different memory resources share all but allocation, the call and close operation states
included, since those are keyed on the base. Splitting the resource's half off saved 116 to 376
bytes from the second memory-resource type on and 4 to 12 on most programs with one; GCC's
inlining shifted around it elsewhere, +10 on one README row and +6 to +14 on some one-site call
probes (`doc/plans/push-and-wait.md`, "Size round after the commits").

Corollary: when a type-erased thunk needs to downcast, it must be able to *name* the layer it
downcasts to. Keep a parameter-free intermediate for that (`_queue_pop_op_base`, which the rows
of `async_queue::_pop_rows` take), and add CRTP-flavoured layers only *above* it.

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
cortex-m0plus. The exception is a dispatch whose caller is then shared: the pop operation now
indexes a row table, since a fold over the element types would key the shared part on them
again (see "Key the erased part on the core, not on the queue").

Keep work that does not depend on the type out of the per-type body: starting the pump's handler
inside its `_visit_at` fold made GCC outline the lambda for one type as an `.isra` clone, +20 per
pump, so the pump calls `start()` once after the fold.

### Narrowing scalar vtable fields is free

`uint16_t`/`uint8_t` instead of `size_t` for size and alignment: identical text, 8 bytes less
rodata per element type. Guard the limits with `static_assert`.

### A null function pointer beats an empty thunk

`destroy` is null when the payload is trivially destructible, and the caller branches. Worth −24
to −100, the best return of any single change, because it removes a whole thunk per element
type. One empty function shared by every trivially destructible type is a different thing: it
costs 2 bytes once, and lets the descriptor's `vt` pointer alone carry the decision (see
"Describe plain data with data").

A row that is null for every user of a configuration is better left out of the table than kept
as null: a pop operation that cannot stop has no stopped row, and indexes `_rows[ _stops +
index ]`; −332 over the suite, up to −18 on four queues (`doc/plans/poprows/variant.py`).

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

### Key a base on what its code reads, not on a type that carries it

`event_pump`'s `push_and_wait` support was first templated on the queue type, so eight pumps
over eight queues carried eight copies of the same forwarding code. Keying it on the queue's
signature set, the only thing the code reads, made the eight copies one: 0 at one or two pumps,
−40 at four and −224 / −220 (no LTO / LTO) at eight on a linked cortex-m4 firmware. Forcing the
forwarding function out of line on top of that cost +28 to +32 on single-pump programs and
gained nothing at eight: once the template arguments match there is one instantiation, so fix
the arguments and leave inlining to GCC. The larger saving was sharing the forwarding *together
with the reschedule that follows it*: the rows of every pump's handler receiver tail-call one
`_finish()` per signature set, which ends in the one out-of-line
`_reschedulable::_reschedule()`, −524 / −428 at eight pumps. A shared completion alone left each
row with its own reschedule and frame (`doc/plans/push-and-wait.md`).

### Out of line pays only where the code is shared, and only the program knows where

`_task_awaitable::await_suspend` is compiled per awaitable and task type. One out-of-line
`_suspend( PromiseCore* )` per awaitable type, reached from a one-line template, saves 22 KB /
38 KB on the 1000-coroutine programs (default / owning, LTO off), but grows 375 small probes by up
to 924 bytes: an awaitable type awaited once used to be inlined into its coroutine and now gets a
function and a call. No compile-time rule separates the two cases. Keying on the operation state
(only child tasks, all but source entries, by `sizeof`) keeps a quarter to a third of the saving
and still grows probes; passing the promise to the constructor loses outright. The owning
configuration's copies are larger, so sharing there pays at a cost of at most 52 bytes on a probe:
the split became the opt-in task-config flag `shared_await_suspend` (`doc/plans/suspend.md`).
Two side lessons: an `ECOR_NOINLINE` function with an unused parameter is cloned per file without
LTO (`.isra`), and passing a pointer at frame+8 costs a 4-byte `add.w` where the frame pointer
itself was a 2-byte `mov`.

### The promise constructor stays out of line, apart from the coroutine's parameters

Every coroutine's ramp constructs its promise, and the promise constructor is a template on the
coroutine's parameter types. Out of line as that template it is one copy per task type and
parameter list, and GCC keeps byte-identical copies apart, weak instantiations with no folding
without LTO: once the suite's coroutines took varied parameter lists, the programs without LTO
grew by up to 0.59%. The template stays inline, takes the core from the context, and delegates to
a non-template `ECOR_NOINLINE` constructor, one copy per task type: −27664 over the suite, every
program −0.2% to −0.7%, a task type with a single coroutine +12 to +20 bytes, up to +4%. Out of
line in `_promise_core` instead, one copy per configuration, gains less (−18326), since every ramp
then passes the resume function. A suite whose coroutines all take only the context hides the
parameter lists' cost: −32020 for the template out of line there. Making the owning constructor
independent of the task type, by moving the awaited operation into the ready-list node, made GCC
inline it into every ramp, 92 → 104 bytes each, +8134 (`doc/plans/promise_ctor.md`,
`doc/plans/awaited_node.md`).

### One out-of-line reschedule for every schedulable

The promise, the task holder, the arena core and the pump each held a `task_core&` and called
`core.reschedule( *this )`, an out-of-line `ll_list< schedulable >::link_back`, with its argument
setup at every site that puts the object back on the ready list. One non-template base holding
the core reference and a `[[gnu::noinline]] _reschedule()`, `_reschedulable : schedulable`,
moves that setup into one function, 4 bytes less per caller: −36 / −44 on the coroutine firmware
(four holders, three arenas, five pumps), −6 to −26 on every call probe, ±0 on the README rows
and the thread firmware, which have no schedulable. RAM is unchanged except for the pump, which
keeps its context for the handlers beside the core, +1 pointer (`doc/plans/push-and-wait.md`,
"One reschedule for every schedulable").

### One link function for every `ll_source` entry

Each `ll_source` entry type had a list of its own, so each operation state's `start()` carried
its own inlined `link_back`. One node type for every entry, `_ll_link`, holding the list it joins
and linking itself in an out-of-line `_link()` as `_reschedulable` does, saved 35 KB across the
suite: −2030 of 14546 bytes at 50 sources, −8668 and −4620 on two 1000-coroutine programs. Two
narrower shapes lost. One shared `link_back` under typed lists left `start()` at 7 size units
with one or two calls per file, so GCC inlined it at every `co_await`, +5076 on the owning
1000-coroutine program. A link behind the vtable mixin made every site compute a second address
beside the one the owning `await_suspend` stores. The link is the entry's first base, so
`start()` passes the operation state's own address; the price is a `this` adjustment in each
vtable row thunk, +276 over 117 rows of the default 1000-coroutine program, the one program that
grew (+356). With a single entry type the out-of-line call is pure cost, +26
(`doc/plans/llshare.md`).

### An operation state's constructor runs at every call site

`co_await sender` constructs the operation state inside the coroutine frame, inlined at the
site, so every member the constructor initialises is a store per site. A per-type constant such
as a vtable pointer belongs in `start()`, one function per operation state type: −8 bytes per
`push_and_wait` site. The same effect sank two other ideas, smaller on a one-site probe and
larger from a few sites on: a stop callback as a direct member instead of in an optional (−16 at
one site, +36 at eight, +76 at sixteen), and the receiver's address and vtable stored by the
constructor instead of by `start()` (−48, +60, +116) (`doc/plans/push-and-wait.md`).

### A non-trivial awaitable destructor costs code at every `co_await`

GCC destroys an awaitable on the resume path of its suspension point and again from a destroy
stub, so an awaitable with a non-trivial destructor costs a call, a stub and dispatch-table
entries at every `co_await`. With `promise_owns_operations` the awaitable keeps the operation
state in raw storage and leaves it to the promise, which destroys it when the coroutine resumes;
`_task_op` destroys a pending one before the frame. That saves 14 to 17 bytes per source
`co_await`, 15.7 / 26.2 per child `task< void >` / `task< int >`, 14.5 per `push_and_wait()`
and 17.4 per source a program defines, and 12% / 14% (LTO off / on) of the generated
100-coroutine program, 9% / 13% of the 1000-coroutine one, for two pointers of RAM per frame and
a fixed cost of the resume check, `_destroy_await_op()` and `_destroy_coroutine()`: median +24 /
+68 / +124 on the queue, pump and holder probes.

Measured and declined (`doc/plans/zoo/co_await/`): starting the operation state in the
awaitable's constructor, which takes the promise, with an empty `await_suspend`, about doubles
the program saving, because each site then makes one call instead of two (8 bytes per coroutine;
only a coroutine that repeats one kind of await lets GCC merge the second call into one shared
tail). Connecting in `await_suspend` too, with the constructor recording the sender's address,
was 2 to 22 KB larger on the programs; forcing the constructor out of line changed nothing.
Limiting the scheme to source entries left short coroutines with mixed awaits 17 bytes larger
than with a destructor. Destroying a pending operation state from the promise destructor saves
44 to 136 bytes on the programs, but runs after the coroutine's locals.

The option off had to compile byte-identical to before, and a forwarding layer broke that
although it reduces to the same instructions: `~_task_op` calling a member that called a static
`h.destroy()` wrapper taking the handle by value made GCC outline `~_task_op` in holders (+36)
and drop its shared `destroy()` clone. The wrapper called directly, force-inlined, with the
handle by `const&`, restored every probe. Check an option's off state against HEAD on the whole
suite, not on the probes it is about.

### A coroutine with no destroy code at all can grow: GCC moves the trap before its table

GCC dispatches a coroutine's resume index through two jump tables, one for the destroy path and
one for resume. Once no suspension point has destroy code, it turns the destroy table into range
checks and bit tests that fall into a `udf` trap, and places that trap before the resume table;
`tbb` and `tbh` branch forward only, so the resume table falls back to 4-byte words. A
trivially destructible `_bare_optional< T >` for trivial `T` made the owning awaitables of
`int`-valued senders destroy-free and showed both sides: a coroutine whose resume table was
already 4-byte words lost its destroy table outright (−1076 on one of 140 awaits), one with
`tbb`/`tbh` tables paid the fallback (+28 to +58), and one whose destroy dispatch was a single
range check paid 12 to 16 for the bit test. Net with `promise_owns_operations`: −9876 / −10176 on
the 1000-coroutine program, +3208 / +3092 on the 300-coroutine one, −1324 / −1348 on the
100-coroutine one; unchanged in the default configuration. Same in GCC 16.2. For a scalar `T` the
value slot's destructor already compiled to nothing, so the tables were the whole effect.

### An unsized `operator delete` takes the frame size out of every coroutine

When a promise declares both forms, the coroutine's destroy path calls the sized `operator
delete` ([dcl.fct.def.coroutine]) and so materialises its frame size, a constant per coroutine.
Declaring only `operator delete( void* )` and keeping the size in the allocation header, beside
the memory resource, saves 1.9 bytes per coroutine: −3648 / −5528 (LTO off / on) on the
1000-coroutine program, for +2 to +8 once where a program has one or two coroutines. The header
grows from one word to two, room that `alignof( std::max_align_t )` reserves anyway on Cortex-M
and x86-64; on a 64-bit host whose `max_align_t` is 8-byte aligned it grows from 8 to 16 bytes.

### A non-trivial promise destructor costs code in every coroutine

GCC calls the promise destructor from each coroutine's ramp, for a frame whose setup fails, and
from its destroy path, so a promise with a non-trivial destructor costs every coroutine function
the same amount whatever its body awaits. The default promise has two reasons for one: its
ready-list node unlinks itself, and an attached continuation is completed stopped. With
`promise_owns_operations` neither is in the destructor: the node lives in raw storage that
`_launch()` builds when the task starts, and `_destroy_frame()` destroys it and completes the
continuation. That saves 28 bytes per coroutine whatever it awaits (20 in the ramp, 8 in the
actor): −20980 / −22612 (LTO off / on, −1.6% / −1.9%) on the 1000-coroutine owning program,
−2244 / −2124 (−1.9% / −2.0%) on the 100-coroutine one. The node keeps the coroutine handle,
since the ready list can no longer cast it to the promise: one pointer of RAM per frame. The
constructor writes only the core and the resume function into the node's storage, so the stores
per ramp stay two, and the fixed cost is a larger shared destroy function and the node built in
`_start_op`: +4 to +6 on six probes with a single coroutine.

Two details of the shape cost 4 bytes each per instance and were taken out. `_launch()` copying
its two arguments out of the storage as a struct made GCC round-trip them through the stack;
reading the two fields as scalars keeps them in registers. And emptying `_task_op`'s handle
before `_destroy_frame()` is a store per destroy site; only a debug build needs it.

### A check that only undefined behaviour reaches still costs every coroutine

`final_suspend()` completed a receiver still attached with `task_unfinished`, which only a
non-void task whose body ends without `co_return` leaves attached. Compiled per task type and
cloned per file, the check cost 8.8 bytes per coroutine; as an `ECOR_ASSERT` it is −103120 over
the suite with no probe larger, −0.6% to −1.2% on every program. The same check in one
out-of-line function per configuration kept only −3858 and grew 535 probes
(`doc/plans/final_suspend.md`).

### Key the erased part on the core, not on the queue

`_queue_pop_op` was compiled whole per queue, receiver type and sender: 125 bytes per extra
receiver type on a parking queue and 249 on a cancellable one, and `pop()` and `pop_value()` on
one receiver kept two byte-identical copies that GCC did not fold. Erasing it in steps showed
where the bytes are:

- A base keyed on the queue type, reaching the receiver through the operation's own row vtable
  with a new `set_stopped` row: 48 / 71 bytes per extra receiver type, but +6 to +24 per closeable
  or stoppable queue with one receiver type (+96 for four), since every stop then went through
  the vtable and nothing was shared across queues; +422 over the suite.
- The same base reaching the receiver through a function-pointer table the queue builds, with
  stops known in `start()` handed back to the per-receiver `start()`: no single queue grows,
  −3146 on the queue probes.
- Keying the base on the core and the call signatures, `_queue_pop_op_base< Core, CallSigs,
  Stoppable >`, so queues over one memory resource share it: −6392 over the suite, −688 on four
  cancellable queues, 8 probes larger by up to 10 bytes (single-element-type cancellable queues,
  and the 100-coroutine program, where the shared `_queue_release()` goes out of line). RAM is
  unchanged: the table pointer replaces the vtable pointer.

A separate member for the stopped row instead of table entry 0 saved 152 more for a pointer of
RAM per pending pop; not taken. Completing an immediate stop through the row instead of the
`bool` from `_start()` lost about 400 on the suite and saved 12 per extra cancellable receiver
type. Each variant script and probe is in `doc/plans/pop-erase/`.

### Take a payload by reference where it travels through a sender

`seq_source::schedule( K key, T val )` and `_sh_sender`'s constructor took the payload by value,
and at each inlined `co_await src.schedule( key, payload{} )` GCC built the payload, copied it
into the parameter and moved it into the sender through stack slots it never read again. By
`&&`, with a `const&` overload that copies an lvalue, it is built once: −8 bytes per
`seq_source` await with a payload, −38048 / −45944 (LTO off / on, −2.5% / −3.1%) on the
1000-coroutine program. Keep the payload-less call an overload of its own, `schedule( K )`, that
constructs `T{}` inside: a default argument `T&& val = T{}` makes every call site materialise
an empty temporary and pass its address, +24 bytes on a coroutine of 32 such awaits. Where GCC
inlines the awaitable's constructor at a single site, the operation state's own by-value
constructors (`_sh_op`, `_sh_entry`, `_sh_link`) leave the same dead stores and the sender
change alone grew that site by 4 bytes; forwarding the key and payload through them as
templates removes those too: −20 bytes at such a site, up to −1500 on 50 source types. It costs
about 1.7 bytes per operation state connected directly from a temporary sender outside a
coroutine, whose key and payload GCC then keeps in a stack slot instead of registers (+20 and +68
on the plain-receiver probes of 12 and 50 sources), a use declined as not mainstream.

For `ll_source`, by reference is right for aggregates only. A scalar or empty payload stays by
value: a temporary bound to a reference inside a `co_await` expression lives across the
suspension, so it takes a slot in the coroutine frame and is stored and loaded back, +9 bytes
per `co_await src.schedule( 0u )` with a `uint32_t` payload, and an empty one shifts the frame
layout into longer encodings, +44 bytes per coroutine. An aggregate of any size, even 1 to 4
bytes, is cheaper by reference, since by value it goes through two stack slots on its way to
the operation state. `ll_source` picks the overloads by `std::is_reference_v< T > ||
std::is_scalar_v< T > || std::is_empty_v< T >`: −141916 over the suite, −34268 / −8604 on the
1000-coroutine program and −50088 on its owning variant without LTO, nothing on the `task`
probes; by reference for every payload saved only −115260, with +26640 on the `task` family,
and by value up to a pointer's size −65688 (`doc/plans/llref.md`). By reference also takes away
an out-of-line `schedule()`: it is always inlined, which loses at many awaits of one source in
one file, +52 at 16 awaits of a 5-byte payload. The reference is only ever a parameter: the
sender stores its own `T`, as CODE_GUIDELINES.md requires of data a type keeps, and a reference
payload, `ll_source< T& >`, takes the by-value overload.

### Erase an operation state over its receiver, not only over its element type

`_queue_call_op` keyed on the receiver type was compiled once per producer task type, ~376 bytes
each, which a probe with a single `task< void >` producer never showed. Storing the receiver's
address in the node and building the vtable rows on the receiver itself (`_sig_vtable< S... >(
_tag< R >, _tag< R > )`) leaves one copy per core, signature set and stoppability, and the
rows are the same functions a task's continuation vtable instantiates for that receiver, so they
fold in firmware that awaits child tasks. Cost: one pointer of RAM per in-flight call. Probe with
two producer task types before measuring an operation state's sharing.

### One virtual is cheaper as a function pointer in the object

A class with a single virtual function pays a vptr pointing at a three-word vtable per derived
type, of which one word is the function; a function pointer stored in the object is that one
word and no vtable. `_inplace_stop_callback_base`, whose destructor is already non-virtual, had
exactly one: `_execute()` gave every `inplace_stop_callback<F>` a vptr, a vtable and a base
vtable, and `std::optional::reset()` on it re-stored the base vptr in the destructor. A function
pointer set by the derived constructor, with the `_source` member that only the constructor read
dropped: −32 to −36 on the scale probes without LTO, −46 to −76 on the README's cancellable
configurations, −4 bytes of RAM per callback. Classes with more virtuals are unmeasured.

The constructor's register-or-run-now logic, ~46 bytes inlined per callback type, as one
out-of-line non-template `_register()` breaks even with one kind of stoppable operation and
saves ~40 per further kind; a probe with a single callback type shows it as noise, so measure
such sharing on a configuration with two.

`schedulable` had one virtual, `resume()`, so every promise type, task holder, arena core and
pump paid a 12-byte vtable, as did the abstract base itself, and every destructor in those
chains re-stored a vptr. A function pointer passed up the constructor chain, with
`schedulable::_resume_of< D >` as the thunk, shrank every probe that has a schedulable and grew
none, `sizeof` unchanged: −48 to −222 bytes on the task probes, −46 to −612 on the pump probes,
−114 to −1110 on the holder probes, −144 to −1000 on the complete programs. A type with a second
virtual has to fold it into the same pointer, or it pays both: `task_holder` with its virtual
`_make_task()` grew from 60 to 64 bytes and by ~10 bytes of code per construction until its
resume thunk passed the factory's `_make` to the shared `_run()` (`doc/plans/schedulable-fnptr.md`).

### A table of function pointers beats virtuals in a secondary base

`_queue_waiter` had two virtuals. The pump has it as a secondary base, so every pump paid two
non-virtual thunks and a second vtable; every waiter type had a vtable of its own even where it
overrode nothing, as `push()`'s operation state did once per element type; and every program
with a waiter emitted `_queue_waiter`'s own vtable. The waiter now holds a pointer to one
constant `_queue_waiter_ops` per waiter type, two function pointers, set by the waiter's
constructor: the same size, and none of the three. −2 to −128 / −6 to −142 on the README rows,
−248 / −178 on the coroutine firmware, −50 to −146 on every call probe, and on the scale probes
−28 to −72 at one queue and up to −488 at eight, none growing. For an internal interface whose
implementations are a closed set, prefer the table (`doc/plans/push-and-wait.md`, "Size round
after the commits").

### A class nested in a class template is a new type per instantiation

`event_pump::_handler_recv` read only the pump's signature set, call flag and stop source, plus
the handler when it has hooks, but nested in the pump it was a new receiver per pump, and
everything keyed on the receiver came with it: the handler's operation state, its destructor,
its rows. At namespace scope as `_pump_handler_recv< Core, Pump >`, with `Core` holding exactly
what the receiver reads and `Pump` void unless the handler has an `on_error()` or `on_stopped()`
hook, pumps whose handlers return one sender type share all of it: 48 to 156 bytes less at two
such pumps, up to 692 at eight. A one-pump probe sees only the cost: without LTO up to 4 bytes,
and 16 to 28 on a stoppable pump, where the stop source's non-trivial destructor gave the
shared core a 12-byte vtable of its own while `schedulable` was polymorphic; with LTO none.

### Flatten a chain of a binary combinator into one operation

`a || b || c || d` built three nested operations, each with a child receiver type of its own,
so every nesting level instantiated its operands' operation states, their vtables and rows, and
its own completion functions again. One variadic `_when_any_op`, whose operands all connect to
one receiver holding a pointer to the shared part, and an `operator||` overload that appends to
a `_when_any_sender` instead of nesting it: on a plain receiver −10 at two operands of one type,
−134 at four and −422 at eight; −278 at four operands of four types and −1168 at eight; in a
task −168 at four and −468 at eight (`or` family, before `when_any`'s stop handling). The
operands start in a recursive `_when_any_ops` base chain, which builds each operation state
from its `connect()` prvalue: a `std::tuple` of operation states would need them movable.

`when_any`'s stop handling and its stored result, on top of that: +110 to +176 on a plain receiver
without a stop token; +294 to +392 in a task with operands of one type, of which ~120 is the stop
callback machinery (`_register`, `request_stop`, zll's `_link`) that any other stop callback in a
firmware already pays and 56 the shared `_when_any_core`. Each distinct completion signature costs a
finishing function and a store: a child task among four operand types, +308 to +1162. Building an
`inplace_stop_token`'s callback into the core as a node, instead of
`std::optional< inplace_stop_callback< F > >`, measured −1334 on the `or` family (about −38 per
`when_any` in a task) and 8 bytes of RAM less, and was declined: one path for every token type is
worth more than that. Moving the callback reset and the stop check out of the per-signature
finishing functions into one function per receiver pays only inlined: −8 to −42 with mixed types,
nothing with one; out of line, +14 (`doc/plans/whenany.md`). Against the nested form `when_any` is
smaller at eight operands (−140 to −992) and on a plain receiver from four (−20, −114), larger in a
task up to four (+128 to +522).

### Key an awaitable on the promise's configuration-level base

`_task_awaitable_base< PromiseType, V >` and its `_receiver` were keyed on the awaiting
`_promise_type< task< T, CFG > >`, though they read only the trace, `_reschedule()`, `get_env()`,
`invoke_set_error()` and `invoke_set_stopped()`. `T` came in through the continuation vtable's
type, whose value row sat between the error rows and the stopped row. With the value row last,
behind a `_task_cont_vtable_base< ErrSigs >`, and `_promise_core< Trace, ErrSigs >` holding
the trace and a pointer to that base, the awaitable, its receiver, the operation state connected
to it and that state's rows exist once per task configuration and awaited value type instead of
once per awaiting task type: ±0 with one task value type, −234 / −198 (M0+ / M4) with two
awaiting four sender kinds, −902 / −766 with four; a further task value type costs 317 instead
of 617 bytes of awaits (`doc/plans/awaitable-cfg.md`). The probe for this kind of sharing needs
several awaiting task value types, as the receiver lesson above needs two producer task types.

`await_suspend< P >` hands the coroutine handle to the base's `_bind< P >`, which computes the
promise itself. Passing `ch.promise()` instead, a reference computed at the site, measured
identical on probes of up to four coroutines with four awaits each and then cost 5 to 10 bytes
per `co_await`: +260 / +224 at sixteen one-type coroutines with four awaits each, +4836 / +2912
with 32 each, and with four task types and 32 awaits each a net +3454 / +1578 over HEAD, the
sharing included. With the address computation at the site GCC inlines `await_suspend` at every
`co_await` and outlines each op's `start()`, where with the handle it keeps one `await_suspend`
per awaitable type with `start()` inlined, as at HEAD. Forcing the body out of line instead cost
every one-type program 42 to 76 bytes. A probe of a few coroutines with a few awaits cannot see
any of this: `doc/plans/awaitable-cfg/scale/` sweeps 1 to 16 coroutines over 1 or 4 value types
with 4 to 32 awaits each, and the number of coroutines sharing an awaitable and the number of
awaits per coroutine are dimensions of their own, next to the number of types. A non-template
`await_suspend(
std::coroutine_handle<> )` that recovers the base through `coroutine_handle< PromiseCore
>::from_address` shares the function across task types as well, 34 more bytes at four types and
260 at sixteen coroutines of four types, but violates `from_address`'s precondition by the
standard's letter and was declined. Handing the awaitable its promise at construction measured
+538 / +686 at sixteen one-type coroutines. Check `nm` for an out-of-line instantiation before
erasing a template parameter, and `-fopt-info-inline-all` (the harness's `EXTRA_FLAGS`) for why
the inliner moved.

The part of that variant which did save bytes was incidental: `_task_awaitable`'s constructor
value-initialises its base with `: _base()`, which zero-fills the base at every site before the
member initialisers run, a `memset` call for the 12-byte base of a `uint32_t` awaitable. Writing
`: _base( p )`, or leaving the base to default-initialise, removes the fill: −40 on a one-type
probe with four sites, −44 / −8 on the coroutine firmware. An empty-parenthesis mem-initialiser
on a base whose members carry their own initialisers is a zero-fill, not a no-op.

While `schedulable` was polymorphic, moving the promise destructor's completion of an attached
continuation into the base as a destructor saved 26 to 30 bytes per further task value type and
cost 12 to 24 once: an abstract base with a non-trivial destructor gets a vtable of its own and a
vptr store in every destructor of the chain. The base's inheriting constructor, taken out of line
at 32 promise constructions, brought the same vtable, +68 / +100 on the eight-pump probes. Any
constructor or destructor of a dynamic base is an outlining candidate that costs a vtable when
taken. With `schedulable` holding a function pointer neither cost exists: forcing the base's
constructor inline is byte-identical on every probe of the suite, and the destructor lives in the
base, −24 to −192 bytes on the task probes and −96 to −468 on the complete programs.

### Measure a feature's scaling in three forms

For `push_and_wait` the growth per element type and per queue was measured with the feature,
with the hand-rolled form it replaces, and with fire-and-forget that waits for nothing. Only the
third form separates the feature's own cost from the cost of the handlers, call sites and pump
dispatch that every form pays: per awaited element type the feature adds an 8-byte call
descriptor, and its call site is smaller than `try_push()` plus a suspension, while a type costs
~600 bytes in total, most of it the two coroutines and the glue that starts them. Against the
hand-rolled form alone the feature looks like a saving, and in absolute terms like ~600 bytes
per type. `doc/plans/push-and-wait/scale/` (local, ignored by git) is a linked-firmware harness
for this kind of sweep: generated probes, three forms, LTO on and off, bucketed symbol
attribution of the last increment. A per-site slope needs each site's awaits in a coroutine of
its own: routing them through one shared helper coroutine makes the slope the helper's cost.

### Review a feature's size from both ends

For a feature's size pass, run two independent reviews: one bottom-up from the disassembly and
symbols of the linked probes (`doc/plans/push-and-wait/symdiff.py`), one top-down from what each
template is keyed on. For `push_and_wait` their findings were disjoint, and together they cut
the feature's cost over fire and forget from 507 to 379 bytes at one element type and from
128 / 120 to 20 / 40 per further queue, no LTO / LTO (`doc/plans/push-and-wait.md`, "Second
round").

### Measure each change inside the combination that ships

Changes measured one at a time on the same header do not predict their combination.
`callbase`, which puts a call's stopped row first in every vtable so that the shared operation
state needs no signature set, cost a closeable call queue 6 to 14 bytes on its own: its new
`_complete_stopped()`, called from three places, went out of line. Forcing it inline was worse
on its own, +16..+20, yet applied after the other three changes of the same round it was
byte-identical on every object probe and still saved 192 to 212 bytes from a core's second
signature set on. Measure each change on top of the ones it ships with, or leave each out of the
whole in turn, before deciding (`doc/plans/push-and-wait.md`, "Size round after the commits").

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

### A symbol diff over an object counts ICF aliases in full

`-fipa-icf` at `-Os` folds identical internal-linkage functions, such as the four
`_queue_push_op` awaitables of the call probe, into one section that carries every name, and
`nm --print-size` prints the full size under each of them. `push-and-wait/symdiff.py` then
disagrees with `size -A` by the folded copies. `size -A` is the figure; use the symbol diff for
attribution, and a section diff when the two disagree.

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
and 254–278 (`is_closeable = false`). `event_pump`'s `is_stoppable` costs a program's first
stoppable pump about 300 bytes over a plain one and each further one 110 to 200, on linked
cortex-m0plus and cortex-m4 firmware with or without LTO, the `stop()` call and its receiver
included (`doc/plans/push-and-wait/scale/`, forms `callstop` and `nonestop` against `call` and
`none`). The header's comment once said 800, an object-file figure from before the stop
callbacks lost their vtables: re-measure a published cost when the machinery under it changes.
When a flag removes a completion path, also drop the
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
`destroy` would add a second test in `_release_node()`, 2 to 4 bytes.

### A descriptor that reuses a vtable pays for every thunk in it

A call node's descriptor points at its element type's node vtable, so a type that a program only
ever hands to `push_and_wait()` still instantiates `construct` and `destroy` thunks that no path
can reach: about 45 bytes per such type on cortex-m4, still paid. A vtable for call descriptors
holding only the payload-taking `take`, the same thunk the node vtable uses, would remove them
for 12 bytes of rodata per type that is also pushed; measured, not applied. The alternatives
measured in `doc/plans/push-and-wait.md` (`patch.py` variants `callvt`, `calltk`, `allvt`; scale
probes of kind `ntc` and form `callboth`): a node-taking `take` with a second thunk per type
reading the call node's `_src` costs 40 bytes per non-trivially-copyable type that is both
pushed and called and nothing for trivially copyable ones; giving every call descriptor a vtable
costs 31 bytes per trivially copyable type in a call queue plus 6 to 13 per such queue, since the
queue loses its compile-time byte path, and still leaves `_payload()` in place for the parked
consumer's delivery. Probe element types only through the operation under test and the baseline
silently loses the thunks the other paths would need: a type the firmware both pushes and calls
is the honest point.

### Key per-type thunks on the element type alone, not on the queue's type list

`_queue_node_ops< T, Ts... >` carried `Ts...` only because `take` emplaced into
`inplace_variant< Ts... >`, yet all three thunks and the vtable were instantiated once per (type,
type list), so an element type used by several queues with different type lists paid for every
copy. Letting the thunk construct into the variant's storage and the queue set the index from the
descriptor (`take( src, dst._data() )`, then `dst._mark( d.index )`) makes the ops struct and its
vtable depend on `T` alone: measured −104 bytes per further queue sharing two element types, −4
to −5 per type otherwise, ±8 everywhere else, in programs with or without `push_and_wait()`.
Moving only `destroy` and `construct` to a `T`-only base recovers half of that. Probe with a type
that appears in several queues with different type lists (`scale/` axis `shared`); the
disjoint-queues axis cannot show this at all.
A node-taking `take` that casts and calls a shared per-type body is byte-identical to two full
thunks, because GCC inlines the body into both wrappers; forcing it out of line saved 16 per type
that is both pushed and called and cost 6 per type in every program with vtable types
(`doc/plans/push-and-wait.md`, batch two).

### Keep a variant's storage at offset 0, and its empty marker a small constant

A type-less base for `inplace_variant` that could address the storage itself needs the storage at
a fixed non-zero offset, `alignof( std::max_align_t )`, and every site that materialises an
alternative's address then pays 2 bytes: `add r0, r4, #8` has no 2-byte Thumb encoding (the
immediate stops at 7) where `mov r0, r4` had one. Measured +130 / +296 (no LTO / LTO) on the
40-type thread firmware and +4 bytes of RAM per 4-byte-aligned variant. An index-only base ahead
of the storage measured the same, for three reasons worth knowing separately: an 8-byte-aligned
alternative pushes the storage to offset 8 again; `std::size_t( -1 )` as the empty marker needs
`mov.w` and `cmn.w` where a constant below 256 fits 2 bytes; and moving the first store to
offset 0 made GCC place a shared epilogue *before* a 40-way `visit` dispatch, which `tbb`,
forward-only, could not reach, turning a 40-byte byte table into a 160-byte word table. The
layout that costs nothing keeps the storage first and the index after it, which with bases means
the storage must itself be the first base: `inplace_variant : _inplace_storage< Size, Align >,
_inplace_variant_base`, bases laid out in declaration order, `npos` the byte maximum. Against
the previous header: ±0 on the README rows and the thread firmware, −18 / −12 on the coroutine
firmware, with `_take()` forced inline. Without that, the two statements that replaced one
helper call made GCC stop inlining `_take()` into the pump's `_receive()`: a 24-byte copy per
queue, +54 on the coroutine firmware (`doc/plans/push-and-wait.md`, "The variant's type-less
base").

### A hand-rolled optional: clear the flag inline, destroy out of line, skip both for trivial types

`_bare_optional< T >`, whose `emplace()` requires emptiness, replaced a union-and-flag, an
enum-and-union, two `std::optional`s and a single-alternative `inplace_variant`. Its `reset()`
shape decides the bytes. Fully inline, it cost the coroutine firmware +48 / +46: a task holder
resets at two sites, and `std::optional`'s shared out-of-line `_M_reset` had held the operation
state's destructor, while two inline resets made GCC emit that destructor out of line plus two
calls. `[[gnu::noinline]]` on the whole `reset()` was worse again, +56 / +58 against the inline
form, because every optional over an `int` or `uint8_t` got a 10-byte function it does not need.
The shape applied: `if constexpr ( std::is_trivially_destructible_v< T > )` clears the flag and
nothing else, and any other `T` calls one `[[gnu::noinline]] _destroy()` per type: coroutine
firmware −58 / −54 against the inline form and −10 / −8 against the header before
`_bare_optional`, README cancellable rows −14 to −28, call probes +4
(`doc/plans/push-and-wait.md`, "`_bare_optional`"). Measure the trivially destructible
instantiations of any utility template separately; they are the ones a blanket attribute hurts.

### Let the typed layer release what the erased layer delivers

`_queue_core` released every node after delivering it, which made it tell calls from items and,
for a call, complete a receiver it knew nothing about: a type-erased entry in every call vtable, a
`Call` template parameter splitting core sharing, and a pump that nulled the node's vtable pointer
to make the release inert. Moving `_release()` into `async_queue`, which knows the signature set,
and letting each waiter release what it took removed all three: README rows −6 to −8 on m0+ and
−8 or +2 on m4, call probes −20 to −42, thread firmware −24 / −12, at a release call per pump
kind, about 8 bytes, +34 / +24 on a five-pump firmware. Keeping the release in the core behind a
kind test instead saved the per-pump bytes but cost the shared core +10 to +22 on every README
row (`doc/plans/push-and-wait.md`, "The release belongs to the queue"). When an erased layer must
know something only the typed layer knows, move the operation up rather than the knowledge down.

### Measure the shape of a lifetime fix too

Three fixes in `push_and_wait()`'s operation states were each cheapest in a different shape
(`doc/plans/push-and-wait.md`, "Defects from the six-reviewer round, fixed"):

- A stop check before registering a stop callback, so that a receiver stopped inside `start()`
  can destroy the operation state, costs 8 to 24 bytes per stoppable operation type: the inlined
  `stop_requested()` and a second stopped completion. Letting the arm call `_cancel()` itself, to
  reuse its completion, cost 8 to 22 bytes more, since GCC inlined `_cancel()` and the unlink in
  it.
- zll's link destructor unlinks a node that is still linked, so an explicit `zll::detach` in a
  derived destructor is pure cost, 8 to 14 bytes. Detach explicitly only where code that runs
  before the base destructor must see the list without the node, such as `_service()` on a
  closeable queue.
- In that destructor a nested `if ( !detached ) { … }` measured 2 to 6 bytes smaller than an early
  return, and without the closeable branch it is byte-identical to the original.

Probe the configuration a fix is for: no call probe had a closeable queue, so the `close()` fix
measured ±0 everywhere until one was added (+4 / +10).
