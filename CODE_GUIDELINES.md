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

## Interfaces

### Store what is handed over by value

Data that a type keeps after the call that hands it over, such as the payload of an
`ecor::ll_source` entry or the element of `ecor::async_queue::push()`, is the type's own `T`,
not a reference to the caller's object. The type cannot know when the data is used: a sender is
connected, started and completed whenever its owner decides, so a reference would rest on an
assumption about the caller, such as that the call sits inside a `co_await`. A caller who wants
a reference instantiates the template with one, `ecor::ll_source< T& >`, and the entry then
refers to the caller's object; a template that stores a `T` forwards it with `(T&&) x`, never
`std::move( x )`, so that it compiles for a reference `T`.

A parameter may still be a reference, used only during the call or copied into the stored
value. A sender or operation state refers to the source, queue or context it was created from;
that those outlive it is the sender/receiver contract. Where referring is the operation's
meaning, as in `ecor::async_queue::push_and_wait( T& ev )`, which lends `ev` to a consumer and
completes once the consumer is done with it, the function documents what the caller keeps
alive.

## Common pitfalls

- **Do not assume `set_error_t(std::exception_ptr)` exists** in a sender's signatures unless the
  sender explicitly declares it. `ecor` does not automatically inject `exception_ptr` error
  signatures.
- **Do not reuse an `op_state`** by calling `.start()` twice. Reconnect to create a fresh operation
  state.
- **`_dummy_receiver`** is available in `util.hpp` and is suitable for tasks whose completion
  signals you don't need to inspect.
