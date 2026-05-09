# Plan: Unify `_ll`/`fifo_source` and `trnx_source`

## Core Insight

`trnx_entry/op/sender/source` is a generalisation of `_ll_entry/op/sender/fifo_source`:
- Both use a vtable-backed linked-list node.
- `trnx_entry` adds a `data: T` field inside the node so the driver can read payload
  without knowing the receiver type.
- `fifo_source` is the "data = void" special case — the source owns completion dispatch.
- `trnx_source` lets the driver own completion dispatch: `entry->set_value(...)` called
  directly on the `trnx_entry<T>*` returned by `query_next_trnx()`, with no reference
  back to the source.

## Decision: Remove `trnx_*` and extend `_ll`/`fifo_source` instead

Rationale: trnx revealed gaps in the ll/fifo design. Rather than maintaining two parallel
stacks, generalise the ll stack to cover the trnx use-case.

---

## Decision Log

### 1. Completion signatures source of truth

In `_ll_entry`, signatures come from the *caller* (`signature... S`).
In `trnx_entry`, they come from *T itself* (`_sender_completions_t<T, empty_env>`).

Conclusion: both are valid forms of "user specifies signatures". Under unification, the
"with data" variant derives signatures from T; the "no data" variant takes them directly
as template parameters. The asymmetry is acceptable.

### 2. Who owns completion dispatch

`fifo_source`: source calls `source.set_value(...)` → pops front → dispatches.
`trnx_source`: driver calls `entry->set_value(...)` directly; entry has already left the list.

Under unification the `fifo_source` API changes: it exposes the entry pointer to callers
(mirroring the trnx API) rather than dispatching through the source. Breaking change for
`fifo_source` users — accepted.

### 3. Stop-polling: _get_stopped_t()

`trnx_source::query_next_trnx()` polls each pending entry for cancellation before handing
it to the driver. `fifo_source` has no equivalent.

Conclusion: stop-polling should be the default for all sources that expose `set_stopped_t()`
in their signatures. The trnx implementation revealed this gap.

### 4. Zero-overhead: conditional vtable slot

The `_get_stopped_t()` vtable row is only injected when `_get_stopped_t()` is present in
the *user-facing* signature list (determined by presence of `set_stopped_t()` in the old
trnx model, or explicitly as `_get_stopped_t()` in the new unified model).

No dead function pointers for sources that don't use stop.

### 5. _get_stopped_t() as an explicit user-specified signature

Rather than auto-injecting from `set_stopped_t()` presence, the user explicitly adds
`_get_stopped_t()` to `S...` when they want stop-polling at dequeue time.

Rationale:
- `set_stopped_t()` means "sender *can* complete with stopped" — could be driver-initiated.
- `_get_stopped_t()` means "source should poll the receiver's stop token at dequeue time".
- These are orthogonal. Conflating them (old trnx behaviour) was incorrect.

### 6. Two-tier signature model

The library formally recognises two sets of signatures:

**Standard tier** (sender→receiver contract, exposed via `get_completion_signatures`):
  `set_value_t(...)`, `set_error_t(E)`, `set_stopped_t()`

**Op-level tier** (op→source contract, internal only):
  `_get_stopped_t()`

Rules:
- `get_completion_signatures` never exposes op-level signatures (it is sender-only).
- `receiver_for` / `_receiver_sig_callable` adds a specialisation for `_get_stopped_t()`
  that always returns `true` — the op satisfies this, not the receiver.
- Sender adaptors (`_then`, `_err_to_val`, etc.) iterate only standard-tier signatures.
- The op (e.g. `_ll_op`) synthesises `get_stopped()` from `get_stop_token(get_env(R))`
  gated by `if constexpr (_has_stop_poll)`. User receivers never implement `get_stopped()`.

---

### 7. Data storage: inside the entry, by value

`T data` lives **inside** the entry node by value. No external pointer, no indirection.
If the user needs non-intrusive storage they simply choose `T = MyType*`.

### 8. No-data case: `unit` type + `ECOR_NO_UNIQUE_ADDRESS`

No `void`-specialisation. Instead:
- A library-defined empty struct `unit` (like `std::monostate` but named explicitly).
- The data member is declared `ECOR_NO_UNIQUE_ADDRESS T data;`.
- When `T = unit` the member occupies zero bytes (guaranteed by `[[no_unique_address]]`).
- Single template, no maintenance split.

`ECOR_NO_UNIQUE_ADDRESS` already exists (or will be added) in the library preamble,
consistent with other existing `ECOR_*` attribute macros.

---

### 9. Unified type name: `_ll_entry<T, S...>`

Keep `_ll_entry` as the canonical name, generalised to `_ll_entry<T, S...>`.
`T = unit` reproduces the current no-data behaviour.
`trnx_entry<T>` becomes a typedef / alias over `_ll_entry<T, ...>` or is deleted.

`_sh_entry` will receive the same `T` + `unit` treatment in a future pass.

---

### 10. `fifo_source` / `broadcast_source` → single `ll_source<T, S...>`

Both existing sources are removed. A single `ll_source<T, S...>` replaces them both.

Key insight: `fifo_source` and `broadcast_source` are not two different *types*, they are
two different *usage patterns* of the same linked-list queue. The type should expose the
primitives; the user chooses the dispatch strategy.

**New `ll_source` API (mirrors `trnx_source`):**
- `query_next()` — pop the front pending entry (or return null if empty); caller owns
  completion dispatch (calls `entry->set_value(...)` etc.)
- `empty()` — returns true if no pending entries
- `front()` — peek at the front entry without removing it

**How dispatch strategies are recovered:**
- *Fifo*: `while (auto* e = src.query_next()) { e->set_value(val); break; }` — pop one, complete it.
- *Broadcast*: `while (auto* e = src.query_next()) { e->set_value(val); }` — drain all.

No separate type needed. `broadcast_source` is deleted.

---

### 11. Stop-polling in `query_next()`: two overloads

Two overloads, distinguished by a tag:

```cpp
// Default: auto-complete stopped entries, return first live one (or null).
entry* query_next();

// Opt-out: return front entry unconditionally, no stop check.
entry* query_next(no_stop_check_t);
```

- `query_next()` is the safe default — mirrors old `query_next_trnx()` behaviour.
- `query_next(no_stop_check)` (tag value) for callers that handle cancellation themselves
  or know stop cannot fire (e.g. ISR context without stop support).
- Both overloads are only present / well-formed when `_get_stopped_t()` is in `S...`;
  if stop is not in the signatures, there is only one `query_next()` (no stop check needed).

---

### 12. `query_next()` return type and entry completion API

`query_next()` returns `_ll_entry<T,S...>*` (null = empty).

`_ll_entry<T,S...>` gains the same completion methods `trnx_entry<T>` has today:
`set_value(...)`, `set_error(...)`, `set_stopped()`.

No wrapper type needed.

---

### 13. `trnx_source` / `trnx_entry` fate: deprecated aliases

Both become `[[deprecated]]` `using` aliases over the unified types:

```cpp
template<typename T>
using trnx_entry [[deprecated("use ll_entry")]] = _ll_entry<T, ...>;

template<typename T>
using trnx_source [[deprecated("use ll_source")]] = ll_source<T, ...>;
```

Existing code keeps compiling with a deprecation warning. Aliases can be removed in a
future cleanup pass.

---

## Design Revision: Free-Function Dispatch Strategy

Instead of callers always writing their own loop on top of `query_next()`, expose
`fifo` / `broadcast` as **free functions** that accept `ll_source` by reference and
implement the dispatch strategy.

### Resolved decisions

| # | Decision |
|---|----------|
| Q1 | Option A: lambda-based — `ecor::fifo(src, f)` / `ecor::broadcast(src, f)` |
| Q2 | Lambda takes `auto& e` (reference, never null inside the lambda) |
| Q3 | Both functions skip stopped entries automatically (auto-complete with `set_stopped`) |
| Q4/5 | `fifo` returns `bool` (true = one entry dispatched); `broadcast` returns `size_t` (count dispatched) |
| Q6 | Names `ecor::fifo` / `ecor::broadcast` (no prefix) |
| Q7 | No `no_stop_check` overloads on the free functions; raw callers use `query_next_unchecked()` directly |
| Q8 | Unconditional (no-stop-check) method on `ll_source` is named `query_next_unchecked()` |

### Final API shape

```cpp
// ll_source methods:
_ll_entry<T,S...>*  query_next();              // pops front, skips stopped (when _get_stopped_t in S...)
_ll_entry<T,S...>*  query_next_unchecked();    // pops front unconditionally (always available)
bool                empty() const;
_ll_entry<T,S...>*  front();                   // peek, no removal

// Free functions (header-level, no class):
template<typename T, signature... S, typename F>
bool   fifo(ll_source<T,S...>& src, F&& f);
//   → calls f(e) on the first live entry, returns true if one was dispatched

template<typename T, signature... S, typename F>
size_t broadcast(ll_source<T,S...>& src, F&& f);
//   → calls f(e) on every live entry, returns count dispatched
```

`no_stop_check_t` / `no_stop_check` are **removed** from the library.

## Open Questions — RESOLVED (base design)

All base design decisions captured above.

## Implementation Steps

1. ~~Add `unit` struct and `ECOR_NO_UNIQUE_ADDRESS`~~ ✓ (done)
2. Generalise `_ll_entry<S...>` → `_ll_entry<T, S...>` ✓ (done, needs `no_stop_check_t` removal)
3. Generalise `_ll_op`, `_ll_sender` ✓ (done)
4. Replace `fifo_source`/`broadcast_source` with `ll_source` ✓ (in progress)
5. Remove `no_stop_check_t` / `no_stop_check` from `ll_source`;
   rename second method to `query_next_unchecked()`
6. Add free functions `ecor::fifo` and `ecor::broadcast` after `ll_source`
7. Add `[[deprecated]]` aliases `trnx_entry`, `trnx_source`
8. Migrate internal usages (`_task_holder_base`, `_async_arena_core_base`) to new API
9. Migrate all tests + README to new API
10. Build + all tests green
- Whether `fifo_source` is renamed or collapsed into a single source type.
- `query_next_trnx()` equivalent on the unified source — API name and semantics.
- Handling of `broadcast_source` — does it also get the data+stop-poll treatment, or is
  it left separate?
