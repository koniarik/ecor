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
  params    distinct gives coroutine k a parameter of its own type, `tag< k >`, after the
            context, so that no two share a promise constructor instantiation; absent when every
            coroutine takes only the context
  cfg       owning for tasks with `promise_owns_operations`, absent for the default configuration
"""
from .common import CFGS, USER_AWAITABLES, Probe, cell, cfg_dims, flash_of, key_of, ptask_alias

NAME = 'coroutine'

VALUES = ('void', 'int')
COROS = (1, 16)
PARAMS = ('same', 'distinct')


def params_dims(params):
    return {} if params == 'same' else {'params': params}

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
    return [Probe(NAME, key_of(value=v, awaiting=a, coros=n, **params_dims(p), **cfg_dims(cfg)),
                  lambda v=v, a=a, n=n, p=p, cfg=cfg: source(v, a, n, cfg, p))
            for cfg in CFGS for v in VALUES for a in BODIES for n in COROS for p in PARAMS]


def source(value, awaiting, coros, cfg, params='same'):
    L = []
    w = L.append
    key = key_of(value=value, awaiting=awaiting, coros=coros, **params_dims(params),
                 **cfg_dims(cfg))
    w(f'// coroutine probe: {key}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    w('extern task_ctx& the_ctx();')
    w('extern void      sink( void const* );')
    w(ptask_alias(cfg))
    w('using ll_source_t = ll_source< unit, set_value_t() >;')
    w('extern ll_source_t&     the_ll_source();')
    w('extern ptask< uint8_t > child_task( task_ctx& );')
    w('using seq_source_t = seq_source< uint32_t, unit, set_value_t() >;')
    w('extern seq_source_t&    the_seq_source();')
    w('extern uint32_t         now();')
    w('')
    w(USER_AWAITABLES)
    w('template < int N >')
    w('struct tag')
    w('{')
    w('        int v;')
    w('};')
    w('')
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
        arg = f', tag< {k} > /*arg*/' if params == 'distinct' else ''
        w(f'ptask< {value} > coro{k}( task_ctx& ctx{arg} )')
        w('{')
        w('        (void) ctx;')
        if BODIES[awaiting]:
            w('        ' + BODIES[awaiting])
        w('        co_return;' if value == 'void' else '        co_return 1;')
        w('}')
        w('')
    w('void probe()')
    w('{')
    w('        auto o = coro0( the_ctx(){} ).connect( dr{{}} );'.format(
        ', tag< 0 >{ 0 }' if params == 'distinct' else ''))
    w('        o.start();')
    w('        sink( &o );')
    w('}')
    return '\n'.join(L) + '\n'


def report(results, headers, w):
    def get(h, value, awaiting, coros, params, cfg):
        key = key_of(value=value, awaiting=awaiting, coros=coros, **params_dims(params),
                     **cfg_dims(cfg))
        return flash_of(results, key, h)

    def per_coroutine(h, cfg, **kw):
        a = get(h, **kw, coros=1, cfg=cfg)
        b = get(h, **kw, coros=16, cfg=cfg)
        return None if a is None or b is None else (b - a) / 15

    w('### the fixed cost of one coroutine function: the slope between 1 and 16 coroutines')
    w('')
    w('The owning column is the slope with `promise_owns_operations`, its delta against the default'
      ' configuration in parentheses. A bracket is the per-coroutine delta of `cur` against another'
      ' header variant.')
    w('')
    w('| value | awaiting | params | 1 coroutine | 16 coroutines | per coroutine |'
      ' per coroutine, owning |')
    w('|---|---|---|---:|---:|---:|---:|')
    for value in VALUES:
        for awaiting in BODIES:
            for params in PARAMS:
                kw = dict(value=value, awaiting=awaiting, params=params)
                a = get('cur', **kw, coros=1, cfg='default')
                b = get('cur', **kw, coros=16, cfg='default')
                cells = []
                for cfg in CFGS:
                    p = per_coroutine('cur', cfg, **kw)
                    if p is None:
                        cells.append('-')
                        continue
                    c = f'{p:.1f}'
                    d = per_coroutine('cur', 'default', **kw)
                    if cfg != 'default' and d is not None:
                        c += f' ({p - d:+.1f})'
                    for h in headers[1:]:
                        o = per_coroutine(h, cfg, **kw)
                        if o is not None:
                            c += f' [{h}: {p - o:+.1f}]'
                    cells.append(c)
                w(f'| {value} | {awaiting} | {params} | {cell(a)} | {cell(b)} | {cells[0]} |'
                  f' {cells[1]} |')
    w('')
