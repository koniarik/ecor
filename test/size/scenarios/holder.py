"""`task_holder`: holders per program, factory type, distinct coroutine bodies, stopping.

A probe is one translation unit of `holders` task holders, each started, each restarting a
`task< void >` coroutine that awaits an opaque source once. The factory is a lambda per holder
(`lambda`, a holder type per holder) or one function pointer type (`fnptr`, one holder type).
With `bodies=1` every holder runs the same coroutine function; with `bodies=n` each has a
function of its own, so the difference is the fixed cost of a coroutine function: its ramp,
its frame destruction and its resume body. With `stop=1` every holder is also asked to `stop()`.

Dimensions: holders 1 | 4 | 10 | 100; factory lambda | fnptr; bodies 1 | n; stop 0 | 1.
"""
from .common import Probe, cell, flash_of, key_of

NAME = 'holder'

BASE = dict(holders=4, factory='lambda', bodies=1, stop=0)


def probes(tier):
    out = {}

    def add(**over):
        kw = dict(BASE, **over)
        if kw['holders'] == 1:
            kw['bodies'] = 1
        k = key_of(**kw)
        out.setdefault(k, Probe(NAME, k, lambda kw=kw: source(**kw)))

    if tier == 'quick':
        for holders in (1, 100):
            for factory in ('lambda', 'fnptr'):
                add(holders=holders, factory=factory)
        add(holders=100, bodies='n')
        add(holders=100, stop=1)
    else:
        for holders in (1, 4, 10, 100):
            for factory in ('lambda', 'fnptr'):
                for bodies in (1, 'n'):
                    for stop in (0, 1):
                        add(holders=holders, factory=factory, bodies=bodies, stop=stop)
    return list(out.values())


def source(holders, factory, bodies, stop):
    L = []
    w = L.append
    w(f'// task_holder probe: {key_of(holders=holders, factory=factory, bodies=bodies, stop=stop)}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    w('using src_t = ll_source< unit, set_value_t() >;')
    w('extern task_ctx& the_ctx();')
    w('extern src_t&    the_src();')
    w('extern void      sink( void const* );')
    w('extern void      sink_i( int );')
    w('')
    w('struct done_recv')
    w('{')
    w('        using receiver_concept = receiver_t;')
    w('        void set_value() noexcept;')
    w('        [[nodiscard]] empty_env get_env() const noexcept')
    w('        {')
    w('                return {};')
    w('        }')
    w('};')
    w('')
    w('namespace')
    w('{')
    nbodies = holders if bodies == 'n' else 1
    for b in range(nbodies):
        w(f'task< void > body{b}( task_ctx& )')
        w('{')
        w('        co_await the_src().schedule();')
        w(f'        sink_i( {b} );')
        w('        co_return;')
        w('}')
    if factory == 'fnptr':
        w('using factory_t = task< void > ( * )( task_ctx& );')
    w('}  // namespace')
    w('')
    w('void probe()')
    w('{')
    w('        auto& ctx = the_ctx();')
    for h in range(holders):
        b = h % nbodies
        if factory == 'lambda':
            w(f'        task_holder th{h}{{ ctx, []( task_ctx& c ) {{ return body{b}( c ); }} }};')
        else:
            w(f'        task_holder< task_default_cfg, task_ctx, factory_t > th{h}{{ ctx, &body{b} }};')
        w(f'        th{h}.start();')
    if stop:
        for h in range(holders):
            w(f'        {{ auto o = th{h}.stop().connect( done_recv{{}} ); o.start(); sink( &o ); }}')
    w('        ctx.core.run_n( 64 );')
    w('}')
    return '\n'.join(L) + '\n'


def report(results, headers, w):
    def get(**over):
        kw = dict(BASE, **over)
        if kw['holders'] == 1:
            kw['bodies'] = 1
        return flash_of(results, key_of(**kw))

    w('### task holders per program')
    w('')
    w('Per holder is (100 − 1) / 99. With `bodies=n` the per-holder figure includes a coroutine'
      ' function of its own.')
    w('')
    w('| factory | bodies | stop | 1 | 4 | 10 | 100 | per holder |')
    w('|---|---|---:|---:|---:|---:|---:|---:|')
    for factory in ('lambda', 'fnptr'):
        for bodies in (1, 'n'):
            for stop in (0, 1):
                v = {n: get(holders=n, factory=factory, bodies=bodies, stop=stop)
                     for n in (1, 4, 10, 100)}
                if all(x is None for x in v.values()):
                    continue
                slope = '-' if v[100] is None or v[1] is None else f'{(v[100] - v[1]) / 99:.1f}'
                w(f'| {factory} | {bodies} | {stop} | '
                  + ' | '.join(cell(v[n]) for n in (1, 4, 10, 100)) + f' | {slope} |')
    w('')
