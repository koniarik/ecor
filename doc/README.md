# ECOR

Embedded coroutine library serves as a framework for building asynchronous applications in C++.
It provides a set of tools and abstractions to manage and coordinate asynchronous tasks.

The primary abstraction is a task, which represents an asynchronous operation that can be
suspended and resumed. This is built on top of native C++20 coroutines.

```cpp

ecor::task< void > example_task( ecor::task_ctx& ctx, virtual_timer& timer )
{
    for(;;) {
        std::cout << "Hello, world!" << std::endl;
        co_await timer.wait_for( 1_s );
    }
}

```

This task will print "Hello, world!" every second. The `co_await` expression allows the task to
suspend its execution until the timer expires, at which point it will be resumed and continue
with the next iteration of the loop.

In the meantime, the system can perform other tasks, such as handling I/O operations,
processing events, or running other tasks concurrently.

## Context

The `task_ctx` is an object that provides access to necessary resources for the task to run.
It includes:
 - Access to `task_core`, which manages execution of tasks and scheduling.
 - Access to `task_memory_resource`, which provides memory for task frames.
The task expects to receive a context object as its first argument, but users can define their
own type as long as it satisfies the `task_context` concept.

C++ coroutines use dynamic memory allocation for their frames, which is problematic in embedded
environments. Hence `ecor` requires the user to provide a memory resource. This allows users to
use custom allocators, such as pool allocators or stack allocators, to manage memory for
coroutine frames efficiently.

## Main loop

The main loop of an `ecor` application typically involves running the `task_core` to execute
tasks and handle events. The `task_core` manages the scheduling and execution of tasks,
ensuring that they run cooperatively.

```cpp

int main()
{
    uint8_t mem_buffer[ 1024 ];
    ecor::task_ctx ctx{ mem_buffer };
    virtual_timer timer;

    ecor::task_holder h{ ctx, [&](ecor::task_ctx& ctx){ return example_task( ctx, timer ); }};
    h.start();

    while ( true ) {
        ctx.core.run_once();
        timer.tick();
    }
}

```

In this example, the main loop continuously calls `ctx.core.run_once()` to execute tasks
scheduled in the `task_core`. The `timer.tick()` is called to update the timer and trigger any
tasks that are waiting on it.

`ecor::task_holder` is a utility that simplifies the management of a single task. It takes care
of creating and starting the task.

## Nested tasks

Tasks can `co_await` other tasks, allowing for composition and nesting of asynchronous
operations. This is useful for breaking down complex operations into smaller, more manageable
pieces.

```cpp

ecor::task<int> child_task( ecor::task_ctx& ctx )
{
    std::cout << "Child task running!" << std::endl;
    co_await ecor::suspend;
    std::cout << "Child task resuming!" << std::endl;
    co_return 42;
}

ecor::task< void > parent_task( ecor::task_ctx& ctx )
{
    int result = co_await child_task( ctx );
    std::cout << "Child task completed with result: " << result << std::endl;
}

```

In this example, `parent_task` awaits the completion of `child_task`. `child_task` prints a
message, suspends itself, and then resumes to print another message before returning a result.
While `child_task` is suspended, `parent_task` is also suspended until `child_task` completes,
at which point it resumes and prints the result.

The resumption of `parent_task` does not happen immediately after `child_task` completes — it
is scheduled to run in the next iteration of the main loop.

##  What else can I await?

In addition to awaiting other tasks, you can also await on various synchronization sources
provided by `ecor`, such as `ecor::ll_source` and `ecor::seq_source`. All of these sources
support `co_await`.

```cpp

ecor::task< void > example_task( ecor::task_ctx& ctx, ecor::broadcast_source<ecor::set_value_t(int)> source )
{
    std::cout << "Waiting for broadcast..." << std::endl;
    int value = co_await source.schedule();
    std::cout << "Received broadcast with value: " << value << std::endl;
}

```

`ll_source` is the general-purpose event source. Entries can be delivered to one waiter at a
time (via `query_next()`) or broadcast to all waiters (via `ecor::broadcast()`). The signature
`ecor::set_value_t(int)` indicates the type of value delivered to waiting tasks.

```cpp

ecor::task< void > example_task( ecor::task_ctx& ctx, ecor::fifo_source<ecor::set_value_t(int)> source )
{
    std::cout << "Waiting for FIFO..." << std::endl;
    int value = co_await source.schedule();
    std::cout << "Received FIFO value: " << value << std::endl;
}

```

Point-to-point delivery via `query_next()` wakes exactly one waiter at a time, in FIFO order.

```cpp

ecor::task< void > example_task( ecor::task_ctx& ctx, ecor::seq_source<int, ecor::set_value_t(std::string)> source )
{
    std::cout << "Waiting for sequence..." << std::endl;
    int key = 2;
    std::string value = co_await source.schedule(key);
    std::cout << "Received sequence value: " << value << std::endl;
}

```

`seq_source` orders waiting tasks by a key. When a value is sent, the task registered with the
lowest key is resumed.

For any source the completion API mirrors its signatures: `set_value_t(int)` →
`source.set_value(42)`, `set_error_t(E)` → `source.set_error(e)`,
`set_stopped_t()` → `source.set_stopped()`. The effect occurs inside the call.

Note that when a source is awaited by a task, the task is not resumed inside the
`set_value()` / `set_error()` / `set_stopped()` call — it is scheduled for the next
`run_once()` iteration.

## Senders/receivers

The library is built on the sender/receiver model from P2300 (C++26). All sources and tasks
act as senders; any type satisfying `receiver_t` can receive completions. This is a standalone
implementation, independent of any existing library.

This document covers the necessary basics; for a comprehensive overview see the official spec:
<https://cplusplus.github.io/sender-receiver/execution.html>

## Signatures

All sources in `ecor` are strongly typed. The supported completion signatures are:

 - `set_value_t(Args...)` indicates that the source will send values of type `Args...`.
 - `set_error_t(Err)` indicates that the source may signal an error of type `Err`.
 - `set_stopped_t` indicates that the source may signal a stop condition.

The call site mirrors the signature: `set_value_t(int)` → `set_value(42)`,
`set_error_t(std::string)` → `set_error("msg")`, `set_stopped_t()` → `set_stopped()`.

## Task signature and awaiting

`ecor::task<void>` carries implicit signatures `set_value_t()`, `set_error_t(task_error)`, and
`set_stopped_t()`. A task can return (via `co_return`), signal an error (reported natively by
the library), or be stopped. `ecor::task<T>` replaces `set_value_t()` with `set_value_t(T)`;
the error and stop signatures remain the same.

`ecor::task<T>` can `co_await` only on senders that have compatible signatures:
 - At most one `set_value_t` signature with at most one argument (it becomes the result of
   the `co_await` expression; multiple value signatures would be ambiguous).
 - Senders with `set_error_t` signature that is subset of tasks own error signatures.
 - Senders with `set_stopped_t` signature.

 The library supports customization points for the task signatures, see the code for details.

##  Memory usage pattern

Tasks are coroutines and require memory for their frames. The library does not use the global
heap, so the user must provide a memory resource. This enables efficient, fragmentation-free
memory management in embedded environments.

Assume following code:

```cpp

ecor::task< void > nested_task( ecor::task_ctx& ctx )
{
    int x = 42;
    co_await ecor::suspend;
    std::cout << "Value: " << x << std::endl;
}

ecor::task< void > example_task( ecor::task_ctx& ctx )
{
    int x = 42;
    co_await nested_task( ctx );
    std::cout << "Value: " << x << std::endl;
}

```

When `example_task` is created it allocates a frame from the memory resource. When it
`co_await`s `nested_task`, that task also allocates a frame. Once `nested_task` completes,
its frame is released and that memory becomes available for future tasks.

The memory usage therefore mirrors a call stack: each nested `co_await` pushes a new frame
that is popped when the awaited task finishes, preventing fragmentation as long as two
independent call stacks do not share the same memory resource.

Take care not to intermix two call stacks in the same memory resource, as that leads to
complex memory usage patterns that are hard to diagnose.

Note that the size of a coroutine frame is determined at link time, not compile time. The user
must ensure sufficient memory is available at runtime.
