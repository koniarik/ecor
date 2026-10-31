"""What one awaiting coroutine pays per distinct awaited value type.

A probe is one translation unit with one `task< void >` coroutine that awaits `n` senders once
each, connected to a receiver and started. The senders are `ll_source`s of distinct payload types
(`kind=source`) or distinct child coroutines (`kind=child`). With `distinct=0` every sender
completes with `uint32_t`; with `distinct=1` each completes with a value type of its own, so the
difference between the two, divided by the extra value types, is what one more awaited value type
costs on top of one more awaited sender. A source's completion signatures are its value row only
(`rows=value`) or the value row with `set_error_t( task_error )` and `set_stopped_t()`
(`rows=full`); a child task always has all three.

Dimensions: kind source | child; rows value | full; n 1 | 2 | 4 | 8; distinct 0 | 1.
"""
from .common import TASK_RECV, Probe, cell, flash_of, key_of

NAME = 'awaited'

VALUE_TYPES = ('uint8_t', 'uint16_t', 'uint32_t', 'uint64_t', 'int8_t', 'int16_t', 'int32_t',
               'int64_t')
COUNTS = (1, 2, 4, 8)
SHAPES = (('source', 'value'), ('source', 'full'), ('child', 'full'))


def probes(tier):
    out = []
    for kind, rows in SHAPES:
        for n in COUNTS if tier == 'full' else (1, 8):
            for distinct in (0, 1):
                k = key_of(kind=kind, rows=rows, n=n, distinct=distinct)
                out.append(Probe(NAME, k, lambda kw=dict(kind=kind, rows=rows, n=n,
                                                         distinct=distinct): source(**kw)))
    return out


def value_type(i, distinct):
    return VALUE_TYPES[i % len(VALUE_TYPES)] if distinct else 'uint32_t'


def source(kind, rows, n, distinct):
    L = []
    w = L.append
    w(f'// awaited probe: {key_of(kind=kind, rows=rows, n=n, distinct=distinct)}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    w('extern task_ctx& the_ctx();')
    w('extern void      sink( void const* );')
    w('extern void      sink_i( int );')
    w('')
    w(TASK_RECV)
    extra = ', set_error_t( task_error ), set_stopped_t()' if rows == 'full' else ''
    if kind == 'source':
        for i in range(n):
            w(f'struct p{i}')
            w('{')
            w('};')
            w(f'using s{i}_t = ll_source< p{i}, set_value_t( {value_type(i, distinct)} ){extra} >;')
            w(f's{i}_t& src{i}();')
        w('')
    w('namespace')
    w('{')
    if kind == 'child':
        for i in range(n):
            t = value_type(i, distinct)
            w(f'task< {t} > child{i}( task_ctx& )')
            w('{')
            w(f'        co_return {t}( {i + 1} );')
            w('}')
            w('')
    w('task< void > run( task_ctx& ctx )')
    w('{')
    w('        (void) ctx;')
    for i in range(n):
        if kind == 'source':
            w(f'        sink_i( int( co_await src{i}().schedule( p{i}{{}} ) ) );')
        else:
            w(f'        sink_i( int( co_await child{i}( ctx ) ) );')
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

    w('### distinct awaited value types: one coroutine awaiting n senders once each')
    w('')
    w('Cells: `distinct=0`, every sender completes with `uint32_t`, then `distinct=1`, each with'
      ' a value type of its own, and its delta. Per value type is the delta at 8 senders over the'
      ' 7 extra value types; a bracket is the delta of `cur` against another header variant.')
    w('')
    w('| senders | rows | ' + ' | '.join(f'n={n}' for n in COUNTS) + ' | per value type |')
    w('|---|---|' + '---:|' * (len(COUNTS) + 1))
    for kind, rows in SHAPES:
        cells = []
        for n in COUNTS:
            d0 = get(kind=kind, rows=rows, n=n, distinct=0)
            d1 = get(kind=kind, rows=rows, n=n, distinct=1)
            c = '-' if d0 is None else f'{d0} / {cell(d1, d0)}'
            for h in headers[1:]:
                o = get(h, kind=kind, rows=rows, n=n, distinct=1)
                if o is not None and d1 is not None:
                    c += f' [{h}: {d1 - o:+d}]'
            cells.append(c)
        d0 = get(kind=kind, rows=rows, n=8, distinct=0)
        d1 = get(kind=kind, rows=rows, n=8, distinct=1)
        per = '-' if d0 is None or d1 is None else f'{(d1 - d0) / 7:.1f}'
        w(f'| {kind} | {rows} | ' + ' | '.join(cells) + f' | {per} |')
    w('')
