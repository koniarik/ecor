<div align="center">

# ecor

**Embedded Coroutines & Execution**

[**Documentation**](https://koniarik.github.io/ecor/)

---

Single-header C++20 coroutine and async execution library for embedded systems

</div>

C++ has `std::execution` (P2300) for async operations, contains part of the implementation to provide additional embedded friendly abstractions. We need something lightweight, zero-allocation capable, and usable in resource-constrained systems.

`ecor` is a **single-header** implementation of P2300 sender/receiver model with coroutine support, designed specifically for embedded systems. It provides zero-allocation async primitives, custom memory management, and additional abstractions missing from the standard.

## Quick Start

```cpp

// Async task using coroutines
ecor::task<int> fetch_data(ecor::task_ctx& ctx)
{
    // Your async work here
    co_return 42;
}

// A receiver to handle task completion
struct my_receiver {
    int& result_ref;

    // Receiver concept requires this tag
    using receiver_concept = ecor::receiver_t;

    // Called when task completes successfully
    void set_value(int value) noexcept {
        result_ref = value;
    }

    // Called on error (required by receiver concept)
    template<typename E>
    void set_error(E&&) noexcept {
        // Handle error
    }

    // Called on cancellation (required by receiver concept)
    void set_stopped() noexcept {
        // Handle cancellation
    }

    // Receiver environment (required by receiver concept)
    auto get_env() const noexcept {
        return ecor::empty_env{};
    }
};

int my_main()
{
    // Set up memory for tasks
    uint8_t buffer[4096];
    ecor::circular_buffer_memory<uint16_t> mem{std::span{buffer}};
    ecor::task_ctx ctx{mem};

    // Create task - this returns a sender
    auto task_sender = fetch_data(ctx);

    // A task doesn't run until we connect and start it
    int result = 0;

    // Connect sender to receiver, creating an operation state
    auto op = std::move(task_sender).connect(my_receiver{result});

    // Start the operation - this begins execution
    op.start();

    // Run the task core until operation completes
    // The task is scheduled on the core after start()
    while (ctx.core.run_once()) { }

    // result now contains 42
    return 0;
}
```

## Tasks - Coroutine-based Async

Tasks are the **primary way** to write async code in ecor. They're coroutines that can `co_await` other async operations.

### Basic Task

```cpp
#include <ecor/ecor.hpp>

ecor::task<void> simple_task(ecor::task_ctx& ctx)
{
    // Task automatically runs on the task_core scheduler
    co_return;
}

ecor::task<int> compute_value(ecor::task_ctx& ctx)
{
    // Do some work
    int result = 42;
    co_return result;
}
```

### Awaiting Other Tasks

Tasks can await other tasks, creating async chains:

```cpp
ecor::task<int> get_sensor_value(ecor::task_ctx& ctx)
{
    // Simulate reading sensor
    co_return 100;
}

ecor::task<void> process_sensor(ecor::task_ctx& ctx)
{
    // Await another task
    int value = co_await get_sensor_value(ctx);

    // Process the value
    if (value > 50) {
        // Handle high value
    }

    co_return;
}
```

### Awaiting Senders

Tasks can await any sender (P2300 concept):

```cpp
ecor::task<void> wait_for_event(
    ecor::task_ctx& ctx,
    ecor::broadcast_source<ecor::set_value_t(int)>& events)
{
    // Await an event from broadcast source
    int value = co_await events.schedule();

    // Continue after event received
    co_return;
}
```

## Broadcast Source - One-to-Many Events

`broadcast_source` allows **one producer** to send events to **multiple subscribers**.
After event is emitted, all waiting tasks receive the event. If no tasks are waiting, the event is dropped. After task receives the event, it must re-register to receive future events.

The template parameter defines the event signature (e.g., `set_value_t(Args...)`), see rest of the documentation for details.

```cpp
#include <ecor/ecor.hpp>

// Define event signature: set_value_t(Args...)
using button_event = ecor::broadcast_source<ecor::set_value_t(int)>;

ecor::task<void> button_handler(ecor::task_ctx& ctx, button_event& btn)
{
    while (true) {
        // Wait for button press event
        int button_id = co_await btn.schedule();

        // Handle button press
        // All subscribed tasks receive this event
    }
}

ecor::task<void> led_controller(ecor::task_ctx& ctx, button_event& btn)
{
    while (true) {
        // Same event, different handler
        int button_id = co_await btn.schedule();

        // Update LEDs based on button
    }
}

ecor::task<void> system_task(ecor::task_ctx& ctx, button_event& btn)
{
    btn.set_value(1);  // Button 1 pressed - both handlers receive this
    co_return;
}
```

### Multiple Value Types

Broadcast sources can handle multiple value completion signatures:

```cpp
using sensor_events = ecor::broadcast_source<
    ecor::set_value_t(int),    // Temperature reading
    ecor::set_value_t(float)   // Humidity reading
>;

ecor::task<void> sensor_monitor(ecor::task_ctx& ctx, sensor_events& events)
{
    auto sender = events.schedule();

    // Sender will complete with one of the value signatures
    // as_variant converts multiple set_value signatures into std::variant
    auto result = co_await (sender | ecor::as_variant);

    // Handle different value types
    std::visit([](auto&& val) {
        if constexpr (std::is_same_v<std::decay_t<decltype(val)>, int>) {
            // Process temperature (int)
        } else {
            // Process humidity (float)
        }
    }, result);

    co_return;
}
```

### Custom error types

Broadcast sources can also have custom error signatures:

```cpp
using error_events = ecor::broadcast_source<
    ecor::set_value_t(int),
    ecor::set_error_t(std::string)  // Custom error type
>;
```

This allows you to send error events to all waiting tasks, which can be useful for broadcasting system-wide errors or status updates. However, this is not compatible with standard configuration of `ecor::task` which expects only `set_error_t(ecor::task_error)` - broadcast source with any extra error can't be directly awaited. You have to use one of the sender combinators to handle this, e.g., `sink_err` to convert errors into optional.

### Stoppable signature

You can also add a stoppable signature to allow waiting tasks to be cancelled:

```cpp
using stoppable_events = ecor::broadcast_source<
    ecor::set_value_t(int),
    ecor::set_stopped_t()  // Stoppable signature
>;
```

## FIFO Source - Point-to-Point Events

While `broadcast_source` delivers events to all waiting tasks, `fifo_source` delivers **each event to exactly one waiting task** (the one that has been waiting the longest). If no tasks are waiting when an event is emitted, the event is immediately dropped.

```cpp
#include <ecor/ecor.hpp>

// Define event signature: set_value_t(int)
using job_queue = ecor::fifo_source<ecor::set_value_t(int)>;

ecor::task<void> worker_task(ecor::task_ctx& ctx, int worker_id, job_queue& q)
{
    while (true) {
        // Wait for next available job
        // The longest waiting task receives the next job
        int job_id = co_await q.schedule();

        // Only ONE worker receives this specific job_id
        // Process job...
    }
}

ecor::task<void> producer_task(ecor::task_ctx& ctx, job_queue& q)
{
    // A task must be waiting before set_value is called, otherwise the event is ignored
    q.set_value(101); // Wakes the first waiting worker
    q.set_value(102); // Wakes the second waiting worker
    co_return;
}
```

## Sequential Source - Ordered Event Processing

`seq_source` provides **ordered, keyed event processing**. Events with keys are processed in order of the keys.

```cpp
using seq_source = ecor::seq_source<
    uint64_t,  // Key: timestamp or priority
    ecor::set_value_t(int)  // Value: event payload
>;
```

### Timer Implementation Example

```cpp
#include <ecor/ecor.hpp>

// Timer event: key is wake-up time, value is task ID
using timer_source = ecor::seq_source<
    uint64_t,  // Key: timestamp when to wake
    ecor::set_value_t(uint64_t)  // Value: timestamp when timer fired
>;

class timer_manager {
public:
    timer_manager() = default;

    // Register a timer to fire at specific time
    auto sleep_until(uint64_t wake_time) {
        // Wait for timer event with this timestamp
        // Events are processed in key order (earliest first)
        return source.schedule(wake_time);
    }

    // Called periodically from main loop
    void tick(uint64_t current_time) {
        // Fire all timers that are due
        // Keep firing until we've processed all events with time <= current_time
        while (!source.empty() && source.front().key <= current_time) {
            uint64_t event_time = source.front().key;
            source.set_value(current_time);
        }
    }

private:
    timer_source source;
};

// Usage example
ecor::task<void> blink_led(ecor::task_ctx& ctx, timer_manager& timer) {
    while (true) {
        uint64_t now = get_current_time();

        // Sleep for 1000ms
        co_await timer.sleep_until(now + 1000);

        toggle_led();
    }
}

ecor::task<void> read_sensor(ecor::task_ctx& ctx, timer_manager& timer) {
    while (true) {
        uint64_t now = get_current_time();

        // Sleep for 5000ms
        co_await timer.sleep_until(now + 5000);

        read_temperature_sensor();
    }
}

// Multiple tasks can sleep simultaneously with different wake times
// The seq_source ensures they wake in the correct order
```

### Priority Queue Pattern

`seq_source` can implement priority-based message delivery where messages go to the highest priority (lowest key) waiter:

```cpp
using priority_queue = ecor::seq_source<
    uint32_t,  // Priority (lower = higher priority)
    ecor::set_value_t(std::string)  // Message payload
>;

ecor::task<void> high_priority_handler(
    ecor::task_ctx& ctx,
    priority_queue& queue)
{
    while (true) {
        // Register with priority 0 (highest)
        std::string msg = co_await queue.schedule(0);
        // This task gets messages first
        process_message(msg);
    }
}

ecor::task<void> low_priority_handler(
    ecor::task_ctx& ctx,
    priority_queue& queue)
{
    while (true) {
        // Register with priority 10 (lower)
        std::string msg = co_await queue.schedule(10);
        // This task only gets messages if no high-priority tasks are waiting
        process_message(msg);
    }
}

ecor::task<void> producer(ecor::task_ctx& ctx, priority_queue& queue) {
    // Send a message - it goes to the task with lowest priority key (highest priority)
    queue.set_value("urgent message");
    co_return;
}
```

## Stop Tokens - Cancellation

ecor implements `inplace_stop_token` and `inplace_stop_source` from P2300, providing cooperative cancellation without allocation.

### Basic Cancellation

```cpp
#include <ecor/ecor.hpp>

ecor::inplace_stop_source stop_source;

ecor::task<void> long_running_task(ecor::task_ctx& ctx) {
    auto token = stop_source.get_token();

    for (int i = 0; i < 1000000; ++i) {
        // Check for cancellation
        if (token.stop_requested()) {
            // Clean up and exit
            co_return;
        }

        // Do work...
    }

    co_return;
}

// From another context (e.g., button press)
void cancel_operation() {
    stop_source.request_stop();
}
```

### Stop Callbacks

Register callbacks to be invoked when stop is requested:

```cpp
void example() {
    ecor::inplace_stop_source source;
    auto token = source.get_token();

    // Callback invoked when stop is requested
    ecor::inplace_stop_callback cb{token, []() {
        // Cleanup, close handles, cancel I/O, etc.
        cleanup_resources();
    }};

    // Later...
    source.request_stop();  // Callback executed here
}
```

### Multiple Callbacks

```cpp
void example() {
    ecor::inplace_stop_source source;

    // Multiple callbacks, all invoked on stop
    ecor::inplace_stop_callback cb1{source.get_token(), []() { close_file(); }};
    ecor::inplace_stop_callback cb2{source.get_token(), []() { disable_interrupt(); }};
    ecor::inplace_stop_callback cb3{source.get_token(), []() { flush_buffers(); }};

    source.request_stop();  // All callbacks execute
}
```

### Callback During Stop

If you register a callback after stop has been requested, it executes immediately:

```cpp
void example() {
    ecor::inplace_stop_source source;
    source.request_stop();  // Stop already requested

    // Callback executes immediately in constructor
    ecor::inplace_stop_callback cb{source.get_token(), []() {
        // This runs right now!
    }};
}
```

## Memory Management

ecor provides deterministic memory management for embedded systems through custom allocators.

### Circular Buffer Memory

Pre-allocate a buffer for all async operations:

```cpp
#include <ecor/ecor.hpp>

// Static buffer for task allocation
uint8_t task_buffer[8192];

// Create memory manager
ecor::circular_buffer_memory<uint16_t> mem{std::span{task_buffer}};

// Create task context with this memory
ecor::task_ctx ctx{mem};

// All tasks using this context allocate from the buffer
auto t1 = my_task(ctx);
auto t2 = another_task(ctx);

// No heap allocation, deterministic memory usage
```

### Custom Memory Resources

Implement your own memory resource:

```cpp
struct my_memory_resource {
    void* allocate(std::size_t bytes, std::size_t align) {
        // Your allocation logic
        return custom_alloc(bytes, align);
    }

    void deallocate(void* p, std::size_t bytes, std::size_t align) {
        // Your deallocation logic
        custom_free(p, bytes, align);
    }
};

my_memory_resource my_mem;
ecor::task_memory_resource mem_resource{my_mem};
// Use mem_resource with task_ctx
```

### Checking Memory Usage

```cpp
void check_memory() {
    uint8_t buffer[4096];
    ecor::circular_buffer_memory<uint16_t> mem{std::span{buffer}};

    std::size_t capacity = mem.capacity();      // Total buffer size
    std::size_t used = mem.used_bytes();        // Currently used
    std::size_t available = capacity - used;    // Available space

    // Monitor memory usage for debugging
    if (used > capacity * 0.9) {
        // Warn: running low on memory
    }
}
```

## Sender Combinators

ecor provides P2300-style sender combinators for composing async operations.

### or - Race Multiple Senders

Passess calls to the first sender that completes (value, error, or stopped), other is ignored.

```cpp
ecor::task<void> example(ecor::task_ctx& ctx, auto sender1, auto sender2) {
    // Race two async operations
    auto winner = co_await (sender1 || sender2);
    // Completes with whichever finishes first
    co_return;
}
```

### as_variant - Handle Multiple Completions

If a sender has multiple `set_value` signatures, `as_variant` converts them into a single `std::variant` - combining multiple value types into one.

```cpp
ecor::task<void> example(ecor::task_ctx& ctx) {
    ecor::broadcast_source<
        ecor::set_value_t(int),
        ecor::set_value_t(float)
    > source;

    auto result = co_await (source.schedule() | ecor::as_variant);

    std::visit([](auto val) {
        if constexpr (std::is_same_v<decltype(val), int>) {
            // Handle int
        } else {
            // Handle float
        }
    }, result);
    co_return;
}
```

### sink_err - Convert Errors to Optional

For void-returning senders that might error, `sink_err` converts errors into an optional:

```cpp
ecor::task<void> example_sink(ecor::task_ctx& ctx) {
    // sink_err converts errors into std::optional<std::variant<error_types...>>
    // For void senders, the result contains the error if one occurred
    auto result = co_await (my_task(ctx) | ecor::sink_err);

    // Result is optional - CONTAINS the error if one occurred, empty if succeeded
    if (result) {
        // Operation FAILED - result contains the error variant
        std::visit([](auto&& error) {
            // Handle the error
        }, *result);
    } else {
        // Operation succeeded (no error)
    }
    co_return;
}
```

## Assert customization

By default, ecor uses `assert` for internal checks. You can customize this by defining `ECOR_ASSERT` before including the header:

```cpp
#define ECOR_ASSERT(expr) my_custom_assert(expr)
#include <ecor/ecor.hpp>
```

Or use default `assert` by defining `ECOR_USE_DEFAULT_ASSERT`:

```cpp
#define ECOR_USE_DEFAULT_ASSERT
#include <ecor/ecor.hpp>
```

## Credits

Created by `veverak` (koniarik). Questions? Find me on [#include discord](https://discord.gg/vSYgpmPrra).

## License

MIT License - see LICENSE file for details.
