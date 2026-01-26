<div align="center">

# ecor

**Embedded Coroutines & Execution**

---

Single-header C++20 coroutine and async execution library for embedded systems

</div>

C++ has `std::execution` (P2300) for async operations, contains part of the implementation to provide additional embedded friendly abstractions. We need something lightweight, zero-allocation capable, and usable in resource-constrained systems.

`ecor` is a **single-header** implementation of P2300 sender/receiver model with coroutine support, designed specifically for embedded systems. It provides zero-allocation async primitives, custom memory management, and additional abstractions missing from the standard.

**Key Features:**
- **Single header** - Just include `ecor.hpp`
- **Zero allocation** - Custom allocators and circular buffers for deterministic memory
- **Embedded-first** - Small footprint, no exceptions required
- **P2300 compatible** - Subset implementation of sender/receiver
- **Cancellation** - Cooperative stop tokens throughout
- **Event sources** - Broadcast and sequential event abstractions

---

**Contents:**
- [Quick Start](#quick-start)
- [Tasks - Coroutine-based Async](#tasks---coroutine-based-async)
- [Broadcast Source - One-to-Many Events](#broadcast-source---one-to-many-events)
- [Sequential Source - Ordered Event Processing](#sequential-source---ordered-event-processing)
- [Stop Tokens - Cancellation](#stop-tokens---cancellation)
- [Memory Management](#memory-management)
- [Timer Example - Multiple Tasks Sleeping](#timer-example---multiple-tasks-sleeping)
- [Sender Combinators](#sender-combinators)
- [Status](#status)

---

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

## Sequential Source - Ordered Event Processing

`seq_source` provides **ordered, keyed event processing**. Events with keys are processed in order of the keys.

### Timer Implementation Example

```cpp
#include <ecor/ecor.hpp>

// Timer event: key is wake-up time, value is task ID
using timer_source = ecor::seq_source<
    uint64_t,  // Key: timestamp when to wake
    ecor::set_value_t(int)  // Value: task/timer ID
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

```cpp
ecor::task<void> example(ecor::task_ctx& ctx, auto sender1, auto sender2) {
    // Race two async operations
    auto winner = co_await (sender1 || sender2);
    // Completes with whichever finishes first
    co_return;
}
```

### as_variant - Handle Multiple Completions

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

## Complete Example - Embedded Device

Putting it all together:

```cpp
#include <ecor/ecor.hpp>

// System state shared between ISRs and main loop
struct system_state {
    volatile bool button_pressed = false;
    volatile int button_id = 0;
    volatile uint64_t current_time_ms = 0;
};

system_state g_state;

// Button handler task
ecor::task<void> handle_buttons(
    ecor::task_ctx& ctx,
    ecor::broadcast_source<ecor::set_value_t(int)>& button_events)
{
    while (true) {
        int button = co_await button_events.schedule();

        if (button == 1) {
            turn_on_led();
        } else {
            turn_off_led();
        }
    }
}

// Periodic sensor reading task with timer
ecor::task<void> read_sensors(
    ecor::task_ctx& ctx,
    ecor::seq_source<uint64_t, ecor::set_value_t(int)>& timer)
{
    while (true) {
        uint64_t wake_time = g_state.current_time_ms + 1000;

        // Wait for timer event
        co_await timer.schedule(wake_time);

        int temp = read_temperature();
        process_temperature(temp);
    }
}

// Main system task
ecor::task<void> system_main(ecor::task_ctx& ctx,
    ecor::broadcast_source<ecor::set_value_t(int)>& button_events,
    ecor::seq_source<uint64_t, ecor::set_value_t(int)>& timer)
{
    // Start sub-tasks
    auto button_task = handle_buttons(ctx, button_events);
    auto sensor_task = read_sensors(ctx, timer);

    // Wait for both (they run concurrently)
    co_await (std::move(button_task) || std::move(sensor_task));
}

// Simple receiver for the main task
struct simple_receiver {
    using receiver_concept = ecor::receiver_t;
    void set_value() noexcept {}
    template<typename E>
    void set_error(E&&) noexcept {}
    void set_stopped() noexcept {}
    auto get_env() const noexcept { return ecor::empty_env{}; }
};

int my_main() {
    // Allocate memory for tasks
    alignas(std::max_align_t) uint8_t buffer[16384];
    ecor::circular_buffer_memory<uint16_t> mem{std::span{buffer}};
    ecor::task_ctx ctx{mem};

    // Create event sources
    ecor::broadcast_source<ecor::set_value_t(int)> button_events;
    ecor::seq_source<uint64_t, ecor::set_value_t(int)> timer;

    // Create and connect main task
    auto main_sender = system_main(ctx, button_events, timer);
    auto op = std::move(main_sender).connect(simple_receiver{});
    op.start();

    // Main event loop
    while (true) {
        // Process button events from ISR
        if (g_state.button_pressed) {
            g_state.button_pressed = false;
            button_events.set_value(g_state.button_id);
        }

        // Process timer events - fire all due timers
        while (!timer.empty() && timer.front().key <= g_state.current_time_ms) {
            timer.set_value(0);  // Fire timer with ID 0
        }

        // Run tasks until no more work
        while (ctx.core.run_once()) { }

        // Sleep or wait for interrupt
        __WFI();  // Wait for interrupt (ARM Cortex-M)
    }

    return 0;
}

// Hardware interrupt handlers - set flags, don't call sources directly
extern "C" void button_isr() {
    g_state.button_pressed = true;
    g_state.button_id = read_button_id();
}

extern "C" void systick_isr() {
    g_state.current_time_ms++;
}
```


## Contributing

Feedback, issues, and contributions welcome! This is designed for real embedded use - if something doesn't work for your use case, let's fix it.

## Credits

Created by `veverak` (koniarik). Questions? Find me on [#include discord](https://discord.gg/vSYgpmPrra).

Inspired by P2300 (std::execution) but designed for the real world of embedded systems.

## License

MIT License - see LICENSE file for details.
