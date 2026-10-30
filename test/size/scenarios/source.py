"""`ll_source` and `seq_source`: distinct sources per program, stop polling, draining, awaiting.

A probe is one translation unit of `n` sources of distinct payload types, their completion
signatures cycling through nullary, `int`, `uint8_t` and `uint32_t` values and, for `seq`, their
key types through `uint32_t`, `uint64_t` and `uint16_t`. Every source gets one operation state
connected to a receiver type of its own and started (`await=0`), or is awaited once from a
single `task< void >` (`await=1`), and is then drained: `broadcast()` with a lambda completing
each entry, or one `query_next()` completing the front entry. With `stopcheck=1` the signature
set carries `get_stopped_t()` and the receivers a live stop token, so draining polls it.

Dimensions:
  kind       ll | seq
  n          1 | 2 | 4 | 8 | 12 | 50 sources
  stopcheck  0 | 1
  drain      broadcast | next   (seq sources and stop-polling sets drain with next only)
  await      0 plain receivers | 1 one coroutine awaiting every source
"""
from .common import Probe, TASK_RECV, cell, flash_of, key_of

NAME = 'source'

SIGS = (('set_value_t()', '', ''), ('set_value_t( int )', 'int', '1'),
        ('set_value_t( uint8_t )', 'uint8_t', 'uint8_t( 2 )'),
        ('set_value_t( uint32_t )', 'uint32_t', '3u'))
KEYS = ('uint32_t', 'uint64_t', 'uint16_t')

BASE = dict(kind='ll', n=4, stopcheck=0, drain='next', await_=0)


def key(**kw):
    kw = dict(kw)
    kw['await'] = kw.pop('await_')
    return key_of(**kw)


def probes(tier):
    out = {}

    def add(**over):
        kw = dict(BASE, **over)
        # broadcast() exists for ll_source only, and its constraint rejects a set that carries
        # get_stopped_t().
        if kw['drain'] == 'broadcast' and (kw['kind'] == 'seq' or kw['stopcheck']):
            return
        k = key(**kw)
        out.setdefault(k, Probe(NAME, k, lambda kw=kw: source(**kw)))

    if tier == 'quick':
        for kind in ('ll', 'seq'):
            for n in (1, 12):
                add(kind=kind, n=n)
                add(kind=kind, n=n, await_=1)
        add(n=12, drain='broadcast')
        add(n=12, stopcheck=1)
    else:
        for kind in ('ll', 'seq'):
            for n in (1, 2, 4, 8, 12, 50):
                for stopcheck in (0, 1):
                    for drain in ('broadcast', 'next'):
                        for await_ in (0, 1):
                            add(kind=kind, n=n, stopcheck=stopcheck, drain=drain, await_=await_)
    return list(out.values())


def source(kind, n, stopcheck, drain, await_):
    L = []
    w = L.append
    w(f'// source probe: {key(kind=kind, n=n, stopcheck=stopcheck, drain=drain, await_=await_)}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    w('extern void     sink( void const* );')
    w('extern void     sink_i( int );')
    w('extern void     sink_b( bool );')
    w('extern uint32_t now();')
    if stopcheck:
        w('extern inplace_stop_token the_token();')
    if await_:
        w('extern task_ctx& the_ctx();')
        w('')
        w(TASK_RECV)
    w('')
    w('namespace')
    w('{')
    for k in range(1, n):
        w(f'struct p{k} {{ uint8_t d[{1 + k % 7}]; }};')
    # A stop-polling set needs the stopped row: draining completes a stopped entry with it.
    stop_sig = ', set_stopped_t(), get_stopped_t()' if stopcheck else ''
    for k in range(n):
        pay = 'unit' if k == 0 else f'p{k}'
        sig = SIGS[k % len(SIGS)][0]
        if kind == 'll':
            w(f'using s{k}_t = ll_source< {pay}, {sig}{stop_sig} >;')
        else:
            w(f'using s{k}_t = seq_source< {KEYS[k % len(KEYS)]}, {pay}, {sig}{stop_sig} >;')
    w('')
    w('struct sources')
    w('{')
    for k in range(n):
        w(f'        s{k}_t s{k};')
    w('};')
    w('}  // namespace')
    w('')
    if not await_:
        env = ('        inplace_stop_token token;\n'
               '        [[nodiscard]] stop_token_env< inplace_stop_token > get_env() const noexcept\n'
               '        {\n                return { token };\n        }') if stopcheck else \
              ('        [[nodiscard]] empty_env get_env() const noexcept\n'
               '        {\n                return {};\n        }')
        for k in range(n):
            arg = SIGS[k % len(SIGS)][1]
            w(f'struct r{k}')
            w('{')
            w('        using receiver_concept = receiver_t;')
            w(f'        void set_value( {arg} ) noexcept;')
            if stopcheck:
                w('        void set_stopped() noexcept;')
            w(env)
            w('};')
        w('')
    else:
        w('namespace')
        w('{')
        w('task< void > run( task_ctx&, sources& S )')
        w('{')
        for k in range(n):
            arg = SIGS[k % len(SIGS)][1]
            sched = _schedule(kind, k)
            if arg:
                w(f'        sink_i( int( co_await S.s{k}.{sched} ) );')
            else:
                w(f'        co_await S.s{k}.{sched};')
        w('        co_return;')
        w('}')
        w('}  // namespace')
        w('')
    w('void probe()')
    w('{')
    w('        sources S;')
    if await_:
        w('        auto& ctx = the_ctx();')
        w('        auto  o   = run( ctx, S ).connect( dr{} );')
        w('        o.start();')
        w('        sink( &o );')
    else:
        tok = 'the_token()' if stopcheck else ''
        for k in range(n):
            w(f'        {{ auto op = S.s{k}.{_schedule(kind, k)}.connect( r{k}{{ {tok} }} ); op.start(); sink( &op ); }}')
    for k in range(n):
        val = SIGS[k % len(SIGS)][2]
        if drain == 'broadcast':
            w(f'        broadcast( S.s{k}, []( auto& e ) {{ e.set_value( {val} ); }} );')
        else:
            w(f'        if ( auto* e = S.s{k}.query_next() )')
            w(f'                e->set_value( {val} );')
        w(f'        sink_b( S.s{k}.empty() );')
    if await_:
        w('        ctx.core.run_n( 64 );')
    w('}')
    return '\n'.join(L) + '\n'


def _schedule(kind, k):
    pay = '' if k == 0 else f'p{k}{{}}'
    if kind == 'll':
        return f'schedule( {pay} )' if pay else 'schedule()'
    key_t = KEYS[k % len(KEYS)]
    return f'schedule( {key_t}( now() ){", " + pay if pay else ""} )'


def report(results, headers, w):
    def get(**over):
        return flash_of(results, key(**dict(BASE, **over)))

    w('### sources per program')
    w('')
    w('Per source is (50 sources − 1 source) / 49.')
    w('')
    w('| kind | stopcheck | drain | await | 1 | 2 | 4 | 8 | 12 | 50 | per source |')
    w('|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|')
    for kind in ('ll', 'seq'):
        for stopcheck in (0, 1):
            for drain in ('broadcast', 'next'):
                if kind == 'seq' and drain == 'broadcast':
                    continue
                for await_ in (0, 1):
                    v = {n: get(kind=kind, n=n, stopcheck=stopcheck, drain=drain, await_=await_)
                         for n in (1, 2, 4, 8, 12, 50)}
                    if all(x is None for x in v.values()):
                        continue
                    slope = '-' if v[50] is None or v[1] is None else f'{(v[50] - v[1]) / 49:.1f}'
                    w(f'| {kind} | {stopcheck} | {drain} | {await_} | '
                      + ' | '.join(cell(v[n]) for n in (1, 2, 4, 8, 12, 50)) + f' | {slope} |')
    w('')
