"""What the sender `operator||` costs, for chains `a || b || ... ` of 1 to 8 operands.

A probe awaits or connects one chain, `uses` times. Every operand reads a distinct opaque
source, so only the combinator, the operands' operation states and the glue are compiled.

Kinds:
  same   every operand an `ll_source< unit, set_value_t() >`
  mixed  the operands cycle through an `ll_source< unit, set_value_t() >`, an `ll_source` with a
         5-byte payload, a `seq_source< uint32_t, unit, set_value_t() >` and a child
         `task< void >`

Dimensions: kind as above; operands 1 | 2 | 3 | 4 | 8, where 1 is the bare operand without `||`;
at task, the chain awaited in a `task< void >` coroutine, `uses` 1 | 4 times; at recv, the chain
connected to a receiver with an empty environment and started; cfg owning for tasks with
`promise_owns_operations`, absent for the default task configuration.
"""
from .common import CFGS, TASK_RECV, Probe, cell, cfg_dims, flash_of, key_of, ptask_alias

NAME = 'or'

KINDS = ('same', 'mixed')
OPERANDS = (1, 2, 3, 4, 8)
USES = (1, 4)

DECLS = '''struct payload5
{
        uint8_t d[5];
};
using ll_source_t         = ll_source< unit, set_value_t() >;
using ll_source_payload_t = ll_source< payload5, set_value_t() >;
using seq_source_t        = seq_source< uint32_t, unit, set_value_t() >;
ll_source_t&         the_ll_source( int );
ll_source_payload_t& the_ll_source_payload( int );
seq_source_t&        the_seq_source( int );
ptask< void >        child_void( task_ctx&, int );
'''


def operand(kind, i):
    if kind == 'same':
        return f'the_ll_source( {i} ).schedule()'
    return (f'the_ll_source( {i} ).schedule()',
            f'the_ll_source_payload( {i} ).schedule( payload5{{}} )',
            f'the_seq_source( {i} ).schedule( uint32_t( now() ) )',
            f'child_void( ctx, {i} )')[i % 4]


def chain(kind, operands, use):
    return ' || '.join(operand(kind, use * operands + i) for i in range(operands))


def dims(kind, operands, at, uses=1, cfg='default'):
    d = dict(kind=kind, operands=operands, at=at)
    if at == 'task':
        d.update(uses=uses, **cfg_dims(cfg))
    return d


def probes(tier):
    ps = []
    for kind in KINDS:
        for n in OPERANDS:
            ps.append(Probe(NAME, key_of(**dims(kind, n, 'recv')),
                            lambda kind=kind, n=n: source(kind, n, 'recv', 1, 'default')))
            for cfg in CFGS:
                for u in USES:
                    if tier == 'quick' and (cfg != 'default' or u != 1):
                        continue
                    ps.append(Probe(NAME, key_of(**dims(kind, n, 'task', u, cfg)),
                                    lambda kind=kind, n=n, u=u, cfg=cfg:
                                    source(kind, n, 'task', u, cfg)))
    return ps


def source(kind, operands, at, uses, cfg):
    L = []
    w = L.append
    w(f'// or probe: {key_of(**dims(kind, operands, at, uses, cfg))}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    w('extern task_ctx& the_ctx();')
    w('extern void      sink( void const* );')
    w('extern uint32_t  now();')
    w('')
    w(TASK_RECV)
    w(ptask_alias(cfg))
    w(DECLS)
    if at == 'task':
        w('namespace')
        w('{')
        w('ptask< void > run( task_ctx& ctx )')
        w('{')
        w('        (void) ctx;')
        for u in range(uses):
            w(f'        co_await ( {chain(kind, operands, u)} );')
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
    else:
        w('void probe()')
        w('{')
        w('        auto& ctx = the_ctx();')
        w('        (void) ctx;')
        w(f'        auto o = ( {chain(kind, operands, 0)} ).connect( dr{{}} );')
        w('        o.start();')
        w('        sink( &o );')
        w('}')
    return '\n'.join(L) + '\n'


def report(results, headers, w):
    w('### `a || b || ...`: one chain of 1 to 8 operands')
    w('')
    w('A row is a kind and a use of the chain; 1 operand is the bare operand. A bracket is the'
      ' delta of `cur` against another header variant.')
    w('')
    w('| kind | at | ' + ' | '.join(f'{n} operands' if n > 1 else '1 operand' for n in OPERANDS)
      + ' |')
    w('|---|---|' + '---:|' * len(OPERANDS))
    rows = [('recv', 1, 'default')] + [('task', u, cfg) for cfg in CFGS for u in USES]
    for kind in KINDS:
        for at, u, cfg in rows:
            label = 'recv' if at == 'recv' else f'task, {u} use{"s" if u > 1 else ""}' + (
                '' if cfg == 'default' else f', {cfg}')
            cells = []
            for n in OPERANDS:
                k = key_of(**dims(kind, n, at, u, cfg))
                v = flash_of(results, k)
                c = cell(v)
                for h in headers[1:]:
                    o = flash_of(results, k, h)
                    if v is not None and o is not None:
                        c += f' [{h}: {v - o:+d}]'
                cells.append(c)
            if all(c == '-' for c in cells):
                continue
            w(f'| {kind} | {label} | ' + ' | '.join(cells) + ' |')
    w('')
