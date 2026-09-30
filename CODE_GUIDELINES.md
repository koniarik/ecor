# Code Guidelines

## Tests

### Receiver types must be defined outside test cases

C++ does not allow templates inside local classes. Since receivers commonly use `auto` or templated
`set_value`/`set_error` methods, defining them inside a `TEST_CASE` or `SUBCASE` will cause a
compiler error.

**Wrong:**
```cpp
TEST_CASE( "my test" )
{
    struct my_receiver           // ERROR: templated methods in local class
    {
        using receiver_concept = ecor::receiver_t;
        void set_value( auto v ) { ... }
        void set_error( auto&&... ) { ... }
        void set_stopped() { ... }
    };

    auto op = src.schedule().connect( my_receiver{ ... } );
}
```

**Correct:** Define the receiver in a named or anonymous namespace before the test case.

```cpp
namespace {
    struct my_receiver
    {
        using receiver_concept = ecor::receiver_t;
        void set_value( auto v ) { ... }
        void set_error( auto&&... ) { ... }
        void set_stopped() { ... }
    };
}

TEST_CASE( "my test" )
{
    auto op = src.schedule().connect( my_receiver{ ... } );
}
```

### `ecor::task<T>` is not a lambda return type

`ecor::task<T>` is a coroutine type that requires a `task_ctx&` as its **first argument**. It cannot
be used as the return type of a lambda or a local function defined inside a test case.

**Wrong:**
```cpp
TEST_CASE( "my test" )
{
    auto coro = []( task_ctx& ctx ) -> ecor::task<void> {  // ERROR: lambdas can't return coroutines
        co_return;
    };
}
```

**Correct:** Define coroutine functions as `static` free functions or as `static` methods of a
helper struct at file scope, before the test case.

```cpp
static ecor::task<void> my_coro( task_ctx& ctx, auto& source, int& result )
{
    result = co_await source.schedule();
}

TEST_CASE( "my test" )
{
    nd_mem   mem;
    task_ctx ctx{ mem };
    ...
    auto h = my_coro( ctx, source, result ).connect( _dummy_receiver{} );
    h.start();
}
```

## Common pitfalls

- **Do not assume `set_error_t(std::exception_ptr)` exists** in a sender's signatures unless the
  sender explicitly declares it. `ecor` does not automatically inject `exception_ptr` error
  signatures.
- **Do not reuse an `op_state`** by calling `.start()` twice. Reconnect to create a fresh operation
  state.
- **`_dummy_receiver`** is available in `util.hpp` and is suitable for tasks whose completion
  signals you don't need to inspect.
