# Agent Instructions

Follow [CODE_GUIDELINES.md](CODE_GUIDELINES.md); it applies to agents as well as humans.
The rules below are for coding agents only.

## Building and testing

After editing test files, always build first. Run tests only after a successful build:
`cmake --build --preset debug`, then `ctest --preset debug`.

Never attempt to run tests against a stale binary. A passing test against an old binary proves
nothing.

Use absolute paths in every shell command. Run a tool that must start in a directory, such as
`cmake --workflow`, in a subshell, `( cd /abs/dir && cmake --workflow --preset m4 )`: a bare
`cd` carries over into the commands that follow.

## Implementing a new feature

Follow this workflow when adding any new feature to the library. Each step requires user
confirmation before proceeding.

### Step 1 — UX & Naming

Agree on the public-facing name and usage syntax **before** writing any code.

- Prefer global `inline` variables (like `wait_until_stopped`, `suspend`) — the user writes
  `co_await ecor::foo`, not `co_await ecor::foo{}` or `co_await ecor::foo()`.
- Propose 5–10 alternatives grouped by semantics. Include the `ecor::` prefix in examples so the
  user sees the actual call site.
- Wait for an explicit choice. Do not proceed to design until the name is confirmed.

### Step 2 — Design & Test Plan

Before writing implementation code:

1. Describe the proposed implementation approach in plain English.
2. If the implementation has meaningful alternatives (e.g. stop-callback vs check-at-suspend),
   present each option with its trade-offs and **ask the user to choose**. Consider:
   - Code size and zero-overhead guarantees (important for embedded targets).
   - Thread-safety implications.
   - Whether new public API on existing types is needed (e.g. `task_core::remove`).
3. Write out the full test plan as a numbered list. Each test must state:
   - What scenario it covers.
   - The exact sequence of `run_once()` calls and external events.
   - Which `CHECK` assertions will be made.
4. Ask the user to approve the test plan before implementation begins.

### Step 3 — Implementation in `ecor.hpp`

- Find the logical neighbourhood for the new type (e.g. near `wait_until_stopped` for
  scheduler-related awaitables).
- Add a doc comment block **above** the implementation struct that explains:
  - What the type does.
  - The stop-token behaviour (if any).
  - A `/// Usage:` line showing `co_await ecor::foo;`.
  - Any zero-overhead guarantee.
- Keep the implementation struct name prefixed with `_` (e.g. `_suspend_awaiter`); expose only the
  `inline` variable to the user.
- Build immediately after editing. Fix any errors before moving to tests.

### Step 4 — Tests in `base_utest.cpp`

Follow the test code style rules in CODE_GUIDELINES.md (receiver types and coroutines must be at
file scope, not inside `TEST_CASE`). For each test in the plan:

1. Define any required coroutine helpers as `static task<T> foo(task_ctx&, ...)` at file scope.
2. Define any required receiver types as named `struct`s at file scope (before the `TEST_CASE`).
3. Drive the event loop with explicit `ctx.core.run_once()` calls — do not use `run_until` loops for
   `suspend`-style tests where the number of steps is deterministic.
4. After all new `TEST_CASE` blocks, run `cmake --build --preset debug` then `ctest --preset debug`.
   All tests must pass before marking the task done.

### Step 5 — Update the README

Add a short section (or subsection) to the main `README.md` documenting the new feature:

- Place it near related existing sections (e.g. scheduler-related awaitables go in the **Tasks**
  section).
- Include a minimal, self-contained code example showing the call site with the `ecor::` prefix.
- Describe the stop-token behaviour and any zero-overhead guarantee in prose, not just code.
- Keep it concise — a heading, 1–2 sentences, one code block, and a one-line callout for important
  caveats.

### Step 6 — Guidelines Update

After a successful feature implementation, add any new patterns, pitfalls, or workflow lessons
discovered during the work to this file.


### Feature Implementation Reference: `ecor::suspend`

`ecor::suspend` is the canonical example of a zero-overhead awaiter added to the library. Key
decisions made:

- **Check-at-suspend-only** for stop: no callbacks, no `task_core::remove`. The stop token is
  checked once inside `await_suspend`. If stop is already requested, `invoke_set_stopped()` is
  called and the function returns — the coroutine stays suspended and its frame is cleaned up by
  `_task_op`'s destructor.
- **`await_suspend` returns `void`** (unconditional suspend). Returning `false` would resume the
  frame immediately, which is wrong after calling `invoke_set_stopped()`.
- **Tests drive `run_once()` manually** and verify ordering: `[A, B]` in queue → A suspends → queue
  becomes `[B, A]` → B runs → A resumes.
- The "in-flight stop" window (stop fires after `reschedule` but before the next `run_once`) is
  acceptable because `task_core` is single-threaded; the next `co_await ecor::suspend` will catch
  it.
- **README** note added as a subsection of **Tasks**, with a `while (true)` loop example and a
  callout about the zero-overhead and stop-at-suspend behaviour.

### Feature Implementation Reference: `async_arena`

`async_arena` manages objects with asynchronous destruction via reference-counted `async_ptr`. Key
patterns and pitfalls discovered:

- **`_destroy_receiver` must handle `set_error`** — `task<void>` senders complete with
  `set_error_t(task_error)`, so the destroy receiver needs `set_error(auto&&)`. Using `auto&`
  (lvalue ref) breaks because `_task_op::start()` passes error enums by value (rvalue). Always use
  `auto&&` (forwarding reference) for error parameters.
- **`zll::detached()` requires exact CRTP type** — `ll_base<schedulable>` produces
  `ll_header<schedulable, ...>`, so `zll::detached()` constraints fail on derived types like
  `_async_arena_core_base`. Fix: `zll::detached(static_cast<schedulable&>(*this))`.
- **`task<void>` destroy needs 3 `run_once()` ticks** — (1) arena resume → start_destroy → task
  scheduled, (2) task runs → co_return → set_value on receiver → arena rescheduled, (3) arena resume
  → finish_cleanup. Plain sender destroy needs 2 ticks + external fire + 1 tick.
- **README code blocks are compiled** — `gen_readme_cpp.py` wraps each `cpp` fence in a namespace
  and compiles it. Examples must be self-contained (define all types, no placeholder names like
  `my_mem_type`).
- **Type erasure base classes** — non-templated `_async_arena_core_base` and
  `_async_arena_cb_counted` reduce template bloat. Control block uses `static_cast` to recover the
  typed arena for `_ctx`/`_mem` access.
- **Two-phase cleanup is critical** — `finish_cleanup()` runs on a fresh `run_once()` tick after the
  destroy sender completes, ensuring the op_state is fully inactive before deallocation.

### Feature Implementation Reference: `push_and_wait`

`push_and_wait()` queues an element and completes once a consumer has processed it. Patterns and
pitfalls from the work; the size lessons are in the code-size skill:

- **A `completion_signatures` set as the whole configuration of a mechanism** —
  `push_and_wait_signatures` decides the reply type, whether errors can be reported, whether a
  queued call is cancellable, and whether the operation exists at all (empty set). Validate the
  set with `static_assert`s carrying one message per rule, and expose the per-rule traits so
  tests can check them; the asserts themselves cannot be negative-tested.
- **Let the consumer's completion flow through the existing row vtable** — `_sig_vtable< S... >`
  erases a receiver over any signature set; the call node points at it through an empty common
  base. A consumer that completes a call knows the set and casts to the full type. The call's own
  operation state knows no set: the set is sorted with `set_stopped_t()` first, which makes
  `_queue_call_vtable< set_stopped_t() >` a base of every vtable that has a stopped row, and the
  operation state completes a call stopped through that base. Taking the element completes the
  call through the nullary value row, which is why `pop()` and `try_pop()` exist only for sets
  that have one.
- **Strict mapping is a compile-time check in the consumer** — the pump static_asserts that
  every completion of its handler's sender has a row, through the `_vtable_can_call_*` concepts.
  A `task< void >` handler therefore needs error and stopped rows next to the value row; say so
  in the docs, users will reach for the nullary set first.
- **Keep knowledge of calls out of the type-erased core** — `_queue_core< Mem, Closeable >` is
  shared by queues of any element types and signature set, so its `_release_node()` only destroys
  and deallocates an item node. A waiter's `_receive()` owns the node it took and passes it to the
  queue's `_release()`, which knows the set and completes a call through the nullary value row;
  only a pump holding a call keeps the node instead.
- **A receiver may destroy its operation state in any completion**, including the stopped one
  that a stop requested before `start()` fires from inside the stop callback's registration.
  Save in locals whatever the code still needs before completing, and check `stop_requested()`
  before registering a callback. In tests, `op_slot` in `test/util.hpp` keeps a destroyed
  operation state's storage readable and overwritten, so a later read crashes and a later write
  fails a `CHECK` in the debug preset, without a sanitizer.
- **Anything that leaves the queue by itself must service it** — a pending `close()` completes
  only from `_service()`, so a node that unlinks itself, such as a cancelled or destroyed call,
  services the core afterwards.

### Feature Implementation Reference: `promise_owns_operations`

`promise_owns_operations` is a task-config flag with which the promise owns the operation state of
each `co_await` and builds its ready-list node when the task starts, so that neither the awaitable
nor the promise has a destructor. Patterns and pitfalls from the work; the size figures and the
measured alternatives are in the code-size skill:

- **An operation starts in `await_suspend`, never earlier.** Starting it in the awaitable's
  constructor measured twice the saving and was declined: `start()` would run while the
  coroutine still runs. The tests "co_await starts the operation once the coroutine is
  suspended" build the awaitable through `await_transform` themselves and check that nothing
  started before the suspension; keep them passing through every awaitable change.
- **The promise destructor is the wrong place to destroy an operation state.** It runs after the
  coroutine's locals, and an operation state such as a `push_and_wait` call or a child task
  touches what it is connected to when destroyed. `_task_op` destroys a started frame through
  `_promise_core::_destroy_frame()`: the pending operation state, the ready-list node, the
  frame, then the continuation is completed stopped. The promise has a destructor only in builds
  with `ECOR_DEFAULT_ASSERT`, where it asserts that nothing is pending.
- **A convention replaces the destructor's guarantee**: rules 3 and 4 on `_promise_core`. A new
  `h.resume()` or `h.destroy()` elsewhere breaks the option silently in release builds; the
  "exactly once" test covers the paths that exist.
- **A child's stop arrives after its frame is gone**, so the code it reaches must not look at the
  child. The test "a chain suspended in a child can be destroyed, whatever the two
  configurations" covers every pair of the six test configurations; run it in the asan preset
  after changing a destruction path.
- **`_promise_debug_link` must stay the first base of every promise that has it.** `pprinter.py`
  finds the frame of the task that `_debug_parent` points at from that address, without knowing
  that task's configuration, so the link cannot be a member, whose offset would depend on it. The
  gdb tests run only on Linux with GCC and gdb, so check a change of the promise's bases in a
  Linux container before it lands; a run on macOS passes whatever the layout.
- **A test reaches a task's promise through an awaiter.** `_capture_promise` in `base_utest.cpp`
  records `&h.promise()` in `await_suspend` and returns false, so the coroutine runs on.
- **Test a parent and a child over every pair of configurations.** `dp_pairs_of< Cfgs... >`
  builds the tuple of all pairs for `TEST_CASE_TEMPLATE_APPLY`; doctest names each case after its
  pair. A new task configuration flag joins the six configurations of `dp_cfg_pairs` and the type
  lists of the templated task tests.
- **Check properties of release builds in `test/release_checks.cpp`.** It compiles without
  `ECOR_DEFAULT_ASSERT`, the only build in which the promise is trivially destructible.
- **Pin a destruction order with a local that records what it sees when destroyed.** A watcher
  whose destructor records whether the source or queue still holds the entry fails the moment
  the order changes, where a sanitizer cannot help: the frame's memory is still allocated.
- **One parameter per optimization, members instead of mixins.** `_promise_core` takes
  `LazyNode`, `OwnsAwaitedOp` and `DestructorFree`, all derived from `promise_owns_operations` in
  `_promise_core_of`; each branch tests the one it serves. What a configuration leaves out is
  an `_absent< T >` member or base, with `T` the type left out, so that stand-ins never collide:
  the ready-list node is the `_node` member with `LazyNode` and a `_reschedulable` base without
  it, the awaited operation state is the `_awaited` member. The raw-storage awaitable is a partial
  specialization of `_task_awaitable` chosen by `PromiseCore::_owns_awaited_op`. Keep `_node` and
  `_awaited` the first members, in this order, so the layout of either configuration does not
  move.
- **The debug parent link travels in the environment.** A debug task's environment answers
  `_get_debug_parent_t` with its promise and a debug child's `_task_op::start()` stores the
  answer, so adaptors that forward their receiver's environment, such as `when_any` and `then`,
  need no code for it. Nothing unlinks it: a child's frame is owned by an operation state nested
  in the awaiting task's frame and goes first. An adaptor that passed the environment to an
  operation it detaches would break that.
- **`task_holder`'s deduction guide assumes `task_default_cfg`**, so a holder of tasks with
  another configuration is spelt out.

### Feature Implementation Reference: `pop_value()`

`pop_value()` is `pop()` with the element moved into the receiver, `set_value_t( T )` instead of
`set_value_t( T& )`, so that a task can `co_await` it. Patterns and pitfalls from the work:

- **A second delivery mode is one `if constexpr` in the receiver's rows.** Both senders build
  `_queue_pop_op< Q, R, Owned >`, whose `_value_row< T >` passes the item on as an lvalue or,
  with `Owned`, an rvalue. Everything else is `_queue_pop_op_base< Core, CallSigs, Stoppable >`,
  shared by both senders and by every queue over one core with the same
  `push_and_wait_signatures`: parking, FIFO order, cancellation, `close()` and the release of the
  node after the row returns, so the moved-from element dies with its node, and a
  `push_and_wait()` caller's object is left moved-from.
- **The shared part reaches the receiver through a row table the queue builds.**
  `async_queue::_pop_rows< Op >` holds the receiver's `set_stopped()` first, null where stopping
  is unreachable, then one `set_value()` per element type, indexed by the node's descriptor. A
  stop known in `start()` is handed back to the per-receiver `start()`, which calls the receiver
  directly; only the stop callback and the `close()` nudge use the stopped row.
- **A receiver written for `pop()` may not fit `pop_value()`.** One that takes `T&` cannot bind
  the rvalue; a receiver taking `T` or `T&&` serves both senders.

### Feature Implementation Reference: `when_any`

`ecor::when_any( a, b, c )`, and `a || b || c`, which builds the same sender, follow stdexec's
`exec::when_any`: the first completion is stored, the other operands are asked to stop, and the
receiver completes once every operand has completed. Patterns and pitfalls from the work:

- **Nothing inside a `request_stop()` loop may be the last completion.** The last one may
  destroy the operation, and with it the stop source whose callback list the loop still walks.
  The winner arrives only after it has requested stop, and a stop request of the receiver is
  counted as one more completion in flight while it runs, so the count cannot reach zero inside
  either loop.
- **Completion signatures depend on the environment.** `set_stopped_t()` is added only when the
  receiver's stop token can stop, and the operands' completions are taken with the environment
  they see, `_when_any_env`. Storage is sized from those signatures, so the store
  static_asserts that an operand's completion is among them.
- **ecor's sources see a stop request only when polled.** An `ll_source` or `seq_source` entry
  with `get_stopped_t()` completes stopped when its driver's `query_next()` reaches it, so a
  `when_any` over such an operand waits for that driver: the sources register no stop
  callback, since only the driver knows whether a linked entry is in use. Tests drive the losers
  to completion with `query_next()`, which drains stopped entries.
- **Test the stop loops with operands that complete inside a stop callback.** An `ll_source`
  entry never does, so it cannot reach the invariant above; `async_queue::pop_value()` with a
  cancellable `pop_api` completes stopped from its callback. Such an operation state needs the
  2048 bytes of `op_slot`.
- **Keep the counting out of the receiver's template.** `_when_any_core< Count >` holds the stop
  source, the count and the function that finishes the operation, with out-of-line `_won()`,
  `_arrive()` and `_stop_all()` shared by every `when_any` with the same counter type, the
  smallest that holds the operand count plus one; only the store and one finishing function per
  completion signature are compiled per receiver.
- **The stop callback on the receiver's token stays in a `std::optional`.** Destroying a
  suspended task destroys a started `when_any`, so the operation must know whether the callback
  is still registered. One path serves every token type; `wa_other_token` in the tests is a
  minimal stoppable token that is not an `inplace_stop_token`.

## Source comments

Write each comment for a future reader of the file as it exists now, who has never seen
this change, its discussion, the previous version, or the code that calls it. A comment
states what the code cannot show: a contract (ownership, lifetime, units, what a failed
call leaves untouched), an invariant, a hidden constraint, or a workaround. Anything else
is already in the code or is true only of this change, and it goes stale silently because
nothing fails when it does.

This applies equally to doc comments on types, fields and functions (`///`, `/** */`) and
to inline `//` comments.

Before writing a comment, ask: would I write this sentence if I were writing the file from
scratch today, with no diff in mind and without knowing who calls this code?

| Instead of | Write |
|---|---|
| `/// Alignment of the node.` above `alignment` | nothing, or what the name cannot carry: `/// Power of two.` |
| `Allocator& alloc_; // for allocation and deallocation` | nothing, or the contract: `// Must outlive this pool.` |
| `// Moved out of pool.cpp; now returns a span.` | nothing: describe what the code does, not what it did |
| `// A comma fold reads more simply but measured +18 bytes.` | nothing, or a present-tense constraint if a reader would otherwise undo it: `// Must stay a loop: the fold form exceeds the size budget.` |

Put history, measurements, rejected alternatives and lists of callers in your reply to the
user or in the commit message. Leave existing comments alone unless they are wrong or the
code they describe changed. Tool directives (`NOLINT`, `eslint-disable`, `@ts-expect-error`)
and license headers are exempt.

After each edit, `.claude/hooks/comment_check.py` lists added comments whose wording often
signals one of these problems ("now", "no longer", "used by", "rather than", "measured").
Rewrite or delete each listed comment, or keep it if it states a present-tense contract.

### In ecor

Rationale, measurements and rejected alternatives belong in `doc/plans/` and the code-size skill,
where they are maintained. A user-facing cost of a configuration option ("worth around 800 bytes
of flash") is fine in a comment: it describes the present API.
