# ECOR

Embedded coroutiner library serves as a framework for building asynchronous applications in C++. It provides a set of tools and abstractions to manage and coordinate asynchronous tasks.

Primary abstraction is a task, which represents an asynchronous operation that can be suspended and resumed. This is build on top of native C++20 coroutines.

```cpp

ecor::task< void > example_task( ecor::task_ctx& ctx, virtual_timer& timer )
{
    for(;;) {
        std::cout << "Hello, world!" << std::endl;
        co_await timer.wait_for( 1_s );
    }
}

```

This task will print "Hello, world!" every second. The `co_await` expression allows the task to suspend its execution until the timer expires, at which point it will be resumed and continue with the next iteration of the loop.

In the meantime, the system can perform other tasks, such as handling I/O operations, processing events, or running other tasks concurrently.

## Context

The `task_ctx` is an object that provides access to necessary resources for the task to run.
It includes:
 - Access to `task_core`, which manages execution of tasks and scheduling.
 - Access to `task_memory_resource`, which provides memory for task frames.
The task expects to get always get an context object as its first argument, but user can define their own type as long as it satisfies the `task_context` concept.

C++ coroutines use dynamic memory allocation for their frames, which is problematic in embedded environments. Hence why `ecor` requires the user to provide a memory resource. This allows users to use custom allocators, such as pool allocators or stack allocators, to manage memory for coroutine frames efficiently.

## Main loop

The main loop of an `ecor` application typically involves running the `task_core` to execute tasks and handle events. The `task_core` manages the scheduling and execution of tasks, ensuring that they are run in a cooperative manner.

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

In this example, the main loop continuously calls `ctx.core.run_once()` to execute tasks scheduled in the `task_core`. The `timer.tick()` is called to update the timer and trigger any tasks that are waiting on it. This allows the application to run indefinitely, handling tasks and events as they occur.

`ecor::task_holder` is a utility that simplifies the management of a single task. It takes care of creating the task and starting it.

## Nested tasks

Task can `co_await` other tasks, allowing for composition and nesting of asynchronous operations. This is useful for breaking down complex operations into smaller, more manageable tasks.

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

In this example, the `parent_task` awaits the completion of the `child_task`. The `child_task` prints a message, suspends itself, and then resumes to print another message before returning a result. As `child_task` is suspended, the `parent_task` is also suspended until `child_task` completes, at which point it resumes and prints the result.

The resumption of the `parent_task` does not happen immediately after the `child_task` completes. It is scheduled to run in the next iteration of the main loop.

##  What else can I await?

In addition to awaiting other tasks, you can also await on various synchronization sources provided by `ecor`, such as `ecor::broadcast_source`, `ecor::fifo_source`, and `ecor::seq_source`. All of these sources support awaiting on them.

```cpp

ecor::task< void > example_task( ecor::task_ctx& ctx, ecor::broadcast_source<ecor::set_value_t(int)> source )
{
    std::cout << "Waiting for broadcast..." << std::endl;
    int value = co_await source.schedule();
    std::cout << "Received broadcast with value: " << value << std::endl;
}

```

`broadcast_source` is a synchronization primitive that allows multiple tasks to wait for a value to be broadcasted. When a value is broadcasted, all waiting tasks are resumed and receive the broadcasted value. `ecor::set_value_t(int)` is a signature that indicates the type of value that will be broadcasted.

```cpp

ecor::task< void > example_task( ecor::task_ctx& ctx, ecor::fifo_source<ecor::set_value_t(int)> source )
{
    std::cout << "Waiting for FIFO..." << std::endl;
    int value = co_await source.schedule();
    std::cout << "Received FIFO value: " << value << std::endl;
}

```

`fifo_source` is a synchronization primitive that allows tasks to wait for values to be sent in a first-in-first-out manner. When a value is sent to the `fifo_source`, the next waiting task is resumed and receives the value.

```cpp

ecor::task< void > example_task( ecor::task_ctx& ctx, ecor::seq_source<int, ecor::set_value_t(std::string)> source )
{
    std::cout << "Waiting for sequence..." << std::endl;
    int key = 2;
    std::string value = co_await source.schedule(key);
    std::cout << "Received sequence value: " << value << std::endl;
}

```

`seq_source` is a synchronization primitive that allows tasks to wait for values in order based on a key. When a value is sent to the `seq_source`, the task with lowest key that is waiting is resumed and receives the value.

## Senders/receivers

The internals of the library is based on the sender/receiver model as specified in PR2300 for C++26. This means that all sources and tasks can be used as senders, and all receivers that satisfy the `receiver_t` concept can be used to receive values from sources or tasks. This is standalone implementation of the sender/receiver model, not based on any existing library.

This document explains necessary info from the sender/receiver model, but user should study the official specification for a more comprehensive understanding:  (https://cplusplus.github.io/sender-receiver/execution.html) [https://cplusplus.github.io/sender-receiver/execution.html].

## Signatures

All sources in `ecor` are strongly-typed and require the user to specify the signature of the values that will be sent or broadcasted. The library supports `set_value`, `set_error`, and `set_stopped` signatures:

 - `set_value_t(Args...)` indicates that the source will send values of type `Args...`.
 - `set_error_t(Err)` indicates that the source may signal an error of type `Err`.
 - `set_stopped_t` indicates that the source may signal a stop condition.

For any source, if the source has signature `set_value_t(int)`, then the user can call `source.on_value( 42 )` to send the value `42` to waiting task as per semantics of the source. If the source has signature `set_error_t(std::string)`, then the user can call `source.on_error( "error message" )` to signal an error to waiting task. If the source has signature `set_stopped_t`, then the user can call `source.on_stopped()` to signal a stop condition to waiting task.

## Task signature and awaiting

`ecor::task<void>` has an implicit signatures of: `set_value_t()`, `set_error_t(task_error)`, and `set_stopped_t`. This means that a task can return (with `co_return`), signal an error (`task_error` is reported nativelly by the library), or be stopped. `ecor::task<T>` just has `set_value_t(T)` instead of `set_value_t()`, but the error and stop signatures are the same.

`ecor::task<T>` can `co_await` only on senders that have compatible signatures:
 - Senders with only one `set_value_t` signature, and there has to be at most one argument in the value signature. This is because that is the result of the `co_await` expression, and multiple value signatures would be ambiguous.
 - Senders with `set_error_t` signature that is subset of tasks own error signatures.
 - Senders with `set_stopped_t` signature.

 The library supports customization points for the task signatures, see the code for details.

##  Memory usage pattern

As tasks are implemented as coroutines, they require memory for their frames. The library does not use dynamic memory allocation, so the user must provide a memory resource for the tasks to use. This allows for efficient memory management in embedded environments.

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

When `example_task` is created, it will allocate memory for its frame from the provided memory resource. When `nested_task` is created, it will also allocate memory for its frame. When `example_task` co_awaits `nested_task`, the `nested_task` will suspend and its frame will remain allocated until it is resumed and completes. After `nested_task` completes, the memory for its frame can be reused for other tasks.

Hence, the memory usage mirrors a call stack pattern, where each nested `co_await` creates a new "frame" of memory that is released when the awaited task completes. This allows for efficient memory usage without fragmentation, as the memory can be reused for subsequent tasks.

Care has to be taken to avoid intermixxing two call stacks in the same memory resource, as that would lead to complex memory usage that might be hard to diagnose.

Note that thanks to nature of coroutines, the size of frame required for a task is determined at link time and it is not known at compile time. This means that the user can not know beforehand how much memory to allocate for a task, and it is up to the user to ensure that enough memory is provided for the tasks to run.
