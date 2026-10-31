"""The fixed cost of one coroutine function, apart from what it awaits.

A probe is one translation unit of `coros` coroutine functions with external linkage that nothing
in the unit calls, like most coroutines of a program's translation unit, so the object holds
their ramps, resume and destroy functions and the shared machinery they call. `probe()` connects
and starts the first one, which puts the task's operation state in the object too. The cost of
one coroutine is the slope between 1 and 16 coroutines.

Dimensions:
  value     the task's value type: void | int
  awaiting  none (the body is only the co_return) | ll_source (one await of a nullary
            `ll_source`) | child_task (one await of an external `task< uint8_t >`) |
            user_awaiter, user_source (one await of the awaitable or the source the program
            defines, as in `common.USER_AWAITABLES`) | mixed (one await of each: `ll_source`,
            `seq_source`, child task, user awaiter, user source)
  coros     1 | 16 coroutine functions
"""
from .common import USER_AWAITABLES, Probe, cell, flash_of, key_of

NAME = 'coroutine'

VALUES = ('void', 'int')
COROS = (1, 16)

# The body of each coroutine before its co_return, in report order.
BODIES = {
    'none': None,
    'll_source': 'co_await the_ll_source().schedule();',
    'child_task': '{ auto v = co_await child_task( ctx ); sink( &v ); }',
    'user_awaiter': 'co_await user_awaiter{};',
    'user_source': 'co_await user_source_sender{};',
    'mixed': ('co_await the_ll_source().schedule(); '
              'co_await the_seq_source().schedule( now() ); '
              '{ auto v = co_await child_task( ctx ); sink( &v ); } '
              'co_await user_awaiter{}; '
              'co_await user_source_sender{};'),
}


def probes(tier):
    return [Probe(NAME, key_of(value=v, awaiting=a, coros=n),
                  lambda v=v, a=a, n=n: source(v, a, n))
            for v in VALUES for a in BODIES for n in COROS]


def source(value, awaiting, coros):
    L = []
    w = L.append
    w(f'// coroutine probe: {key_of(value=value, awaiting=awaiting, coros=coros)}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    w('extern task_ctx& the_ctx();')
    w('extern void      sink( void const* );')
    w('using ll_source_t = ll_source< unit, set_value_t() >;')
    w('extern ll_source_t&     the_ll_source();')
    w('extern task< uint8_t > child_task( task_ctx& );')
    w('using seq_source_t = seq_source< uint32_t, unit, set_value_t() >;')
    w('extern seq_source_t&    the_seq_source();')
    w('extern uint32_t         now();')
    w('')
    w(USER_AWAITABLES)
    w('struct dr')
    w('{')
    w('        using receiver_concept = receiver_t;')
    w(f'        void set_value({" int" if value == "int" else ""} ) noexcept;')
    w('        void set_error( task_error ) noexcept;')
    w('        void set_stopped() noexcept;')
    w('        [[nodiscard]] empty_env get_env() const noexcept')
    w('        {')
    w('                return {};')
    w('        }')
    w('};')
    w('')
    for k in range(coros):
        w(f'task< {value} > coro{k}( task_ctx& ctx )')
        w('{')
        w('        (void) ctx;')
        if BODIES[awaiting]:
            w('        ' + BODIES[awaiting])
        w('        co_return;' if value == 'void' else '        co_return 1;')
        w('}')
        w('')
    w('void probe()')
    w('{')
    w('        auto o = coro0( the_ctx() ).connect( dr{} );')
    w('        o.start();')
    w('        sink( &o );')
    w('}')
    return '\n'.join(L) + '\n'


def report(results, headers, w):
    def get(h='cur', **kw):
        return flash_of(results, key_of(**kw), h)

    w('### the fixed cost of one coroutine function: the slope between 1 and 16 coroutines')
    w('')
    w('A bracket is the per-coroutine delta of `cur` against another header variant.')
    w('')
    w('| value | awaiting | 1 coroutine | 16 coroutines | per coroutine |')
    w('|---|---|---:|---:|---:|')
    for value in VALUES:
        for awaiting in BODIES:
            kw = dict(value=value, awaiting=awaiting)
            a, b = get(**kw, coros=1), get(**kw, coros=16)
            per = '-' if a is None or b is None else f'{(b - a) / 15:.1f}'
            for h in headers[1:]:
                oa, ob = get(h, **kw, coros=1), get(h, **kw, coros=16)
                if None not in (a, b, oa, ob):
                    per += f' [{h}: {((b - a) - (ob - oa)) / 15:+.1f}]'
            w(f'| {value} | {awaiting} | {cell(a)} | {cell(b)} | {per} |')
    w('')
