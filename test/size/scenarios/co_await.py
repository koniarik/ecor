"""What one `co_await` costs, for each kind of awaitable.

A probe is one translation unit with one `task< void >` coroutine that awaits the same kind of
sender `awaits` times in a row, connected to a receiver and started. The cost of one `co_await` is
the slope between 8 and 16 awaits. The sources, the queue and the child coroutines are opaque
externals, so the object holds the coroutine, its awaitables and the shared machinery only.

Kinds:
  ll_source                 `ll_source< unit, set_value_t() >`
  ll_source_payload         an `ll_source` with a 5-byte payload, completing with no value
  ll_source_payload_value   an `ll_source` with a 1-byte payload, completing with `int`
  seq_source                `seq_source< uint32_t, unit, set_value_t() >`
  seq_source_payload_value  a `seq_source` with a `uint64_t` key and a 2-byte payload,
                            completing with `int`
  task_void                 a child `task< void >`
  task_int                  a child `task< int >`
  suspend                   `ecor::suspend`
  push_and_wait             `push_and_wait()` on an `async_queue`
  user_awaiter              an awaitable the program defines, which is no sender
  user_source               the sender of a waiter list the program defines, through ecor's
                            awaitable; its operation state unlinks itself when destroyed

Dimensions: kind as above; awaits 8 | 16; cfg owning for tasks with `promise_owns_operations`,
absent for the default task configuration.
"""
from .common import (CFGS, TASK_RECV, USER_AWAITABLES, Probe, cell, cfg_dims, flash_of, key_of,
                     ptask_alias)

NAME = 'co_await'

AWAITS = (8, 16)

DECLS = '''struct payload5
{
        uint8_t d[5];
};
struct payload1
{
        uint8_t d[1];
};
struct payload2
{
        uint8_t d[2];
};
struct event
{
        uint8_t d[3];
};
using ll_source_t                = ll_source< unit, set_value_t() >;
using ll_source_payload_t        = ll_source< payload5, set_value_t() >;
using ll_source_payload_value_t  = ll_source< payload1, set_value_t( int ) >;
using seq_source_t               = seq_source< uint32_t, unit, set_value_t() >;
using seq_source_payload_value_t = seq_source< uint64_t, payload2, set_value_t( int ) >;
struct queue_cfg : queue_default_cfg< circular_buffer_memory< uint16_t > >
{
        using push_and_wait_signatures =
            completion_signatures< set_value_t(), set_error_t( task_error ), set_stopped_t() >;
};
using queue_t = async_queue< queue_cfg, event >;
ll_source_t&                the_ll_source();
ll_source_payload_t&        the_ll_source_payload();
ll_source_payload_value_t&  the_ll_source_payload_value();
seq_source_t&               the_seq_source();
seq_source_payload_value_t& the_seq_source_payload_value();
queue_t&                    the_queue();
ptask< void >               child_void( task_ctx& );
ptask< int >                child_int( task_ctx& );
'''

# The statement each kind repeats, in report order.
STMTS = {
    'll_source': 'co_await the_ll_source().schedule();',
    'll_source_payload': 'co_await the_ll_source_payload().schedule( payload5{} );',
    'll_source_payload_value':
        'sink_i( int( co_await the_ll_source_payload_value().schedule( payload1{} ) ) );',
    'seq_source': 'co_await the_seq_source().schedule( uint32_t( now() ) );',
    'seq_source_payload_value': 'sink_i( int( co_await the_seq_source_payload_value().schedule( '
                                'uint64_t( now() ), payload2{} ) ) );',
    'task_void': 'co_await child_void( ctx );',
    'task_int': 'sink_i( co_await child_int( ctx ) );',
    'suspend': 'co_await suspend;',
    'push_and_wait': '{ event e{}; co_await the_queue().push_and_wait( e ); sink( &e ); }',
    'user_awaiter': 'co_await user_awaiter{};',
    'user_source': 'co_await user_source_sender{};',
}


def probes(tier):
    return [Probe(NAME, key_of(kind=kind, awaits=n, **cfg_dims(cfg)),
                  lambda kind=kind, n=n, cfg=cfg: source(kind, n, cfg))
            for cfg in CFGS for kind in STMTS for n in AWAITS]


def source(kind, awaits, cfg):
    L = []
    w = L.append
    w(f'// co_await probe: {key_of(kind=kind, awaits=awaits, **cfg_dims(cfg))}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    w('extern task_ctx& the_ctx();')
    w('extern void      sink( void const* );')
    w('extern void      sink_i( int );')
    w('extern uint32_t  now();')
    w('')
    w(TASK_RECV)
    w(ptask_alias(cfg))
    w(DECLS)
    w(USER_AWAITABLES)
    w('namespace')
    w('{')
    w('ptask< void > run( task_ctx& ctx )')
    w('{')
    w('        (void) ctx;')
    for _ in range(awaits):
        w('        ' + STMTS[kind])
    w('        co_return;')
    w('}')
    w('}  // namespace')
    w('')
    w('void probe()')
    w('{')
    w('        auto& ctx = the_ctx();')
    w('        auto  o   = run( ctx ).connect( dr{} );')
    w('        o.start();')
    w('        sink( &o );')
    w('        ctx.core.run_n( 64 );')
    w('}')
    return '\n'.join(L) + '\n'


def report(results, headers, w):
    def get(h='cur', **kw):
        return flash_of(results, key_of(**kw), h)

    w('### one `co_await` of each kind: the slope between 8 and 16 awaits')
    w('')
    def per_await(h, kind, cfg):
        a = get(h, kind=kind, awaits=8, **cfg_dims(cfg))
        b = get(h, kind=kind, awaits=16, **cfg_dims(cfg))
        return None if a is None or b is None else (b - a) / 8

    w('The owning column is the slope with `promise_owns_operations`, its delta against the default'
      ' configuration in parentheses. A bracket is the per-await delta of `cur` against another'
      ' header variant.')
    w('')
    w('| kind | 8 awaits | 16 awaits | per await | per await, owning |')
    w('|---|---:|---:|---:|---:|')
    for kind in STMTS:
        a, b = get(kind=kind, awaits=8), get(kind=kind, awaits=16)
        cells = []
        for cfg in CFGS:
            p = per_await('cur', kind, cfg)
            if p is None:
                cells.append('-')
                continue
            c = f'{p:.1f}'
            d = per_await('cur', kind, 'default')
            if cfg != 'default' and d is not None:
                c += f' ({p - d:+.1f})'
            for h in headers[1:]:
                o = per_await(h, kind, cfg)
                if o is not None:
                    c += f' [{h}: {p - o:+.1f}]'
            cells.append(c)
        w(f'| {kind} | {cell(a)} | {cell(b)} | {cells[0]} | {cells[1]} |')
    w('')
