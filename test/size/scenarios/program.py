"""Complete programs: coroutines, task holders, queues with pumps, and sources, linked.

A probe is a program linked without startup code and entered at `_start`, with unreferenced
sections dropped, so its figure is what the program pays. `_start` runs the library's static
initialisation, starts one `task_holder` per root coroutine and one `event_pump` per queue, and
loops forever pushing an element into every queue, draining every source and running the core.
The probe's key is its population:

  coros     coroutine functions in all, `holders` of them roots that a holder restarts and that
            await the others, so every function is reachable; the rest cycle through shapes:
            60% leaves awaiting one to three sources, 20% nesting two to five other coroutines
            with a fifth of the picks going to ten popular ones, 10% with 60 to 140 awaits,
            5% recursive, 5% producers calling `push_and_wait()` on a queue. No two functions
            have the same statements, except one coroutine in twenty, a leaf, that repeats the
            one before it
  types     distinct `task< T >` value types the functions cycle through
  queues    queues of `qtypes` element types each, drained by a pump with task handlers
  ll, seq   `ll_source` and `seq_source` instances of distinct payload types
  lto       linked with link-time optimisation (1) or without (0)
  cfg       owning for tasks with `promise_owns_operations` and `shared_await_suspend`, absent for
            the default configuration

Populations: 100, 300 and 1000 coroutines, each linked with and without LTO; the quick tier holds
the 100-coroutine program without LTO in the default configuration only.
"""
import hashlib
import re
import subprocess

from .common import CFGS, Probe, cell, cfg_dims, flash_of, key_of, ptask_alias

NAME = 'program'

POPULATIONS = (
    dict(coros=100, types=5, holders=10, queues=1, qtypes=4, ll=5, seq=2),
    dict(coros=300, types=10, holders=30, queues=2, qtypes=6, ll=15, seq=5),
    dict(coros=1000, types=20, holders=100, queues=5, qtypes=8, ll=50, seq=10),
)
PER_TU = 50

VT = ['void', 'int', 'bool', 'uint8_t', 'uint16_t', 'uint32_t', 'int8_t', 'int16_t', 'int32_t',
      'uint64_t', 'char', 'unsigned', 'long', 'short', 'float', 'vs1', 'vs2', 'vs3', 'vs4', 'vs5']
SIGS = (('set_value_t()', ''), ('set_value_t( int )', '1'), ('set_value_t( uint8_t )', 'uint8_t( 2 )'),
        ('set_value_t( uint32_t )', '3u'))
KEYS = ('uint32_t', 'uint64_t', 'uint16_t')

BUCKETS = [
    ('coroutines', re.compile(r'^(c\d+|root\d+|rec\d+|on\d+_\d+)\(')),
    ('vtable rows', re.compile(r'ecor::(_sig_vtable|_vtable_of)')),
    ('queue', re.compile(r'ecor::(async_queue|_queue_)')),
    ('pump', re.compile(r'ecor::(event_pump|_pump_)')),
    ('sources', re.compile(r'ecor::(ll_source|_ll_|ll_entry|seq_source|_sh_|seq_entry|broadcast)')),
    ('holders', re.compile(r'ecor::(task_holder|_task_holder)')),
    ('stop', re.compile(r'ecor::(inplace_stop|_stop_|stop_token)')),
    ('memory', re.compile(r'ecor::(circular_buffer_memory|task_memory_resource|_promise_base::_alloc'
                          r'|_promise_base::_dealloc)')),
    ('task machinery', re.compile(r'ecor::')),
    ('zll', re.compile(r'^zll::')),
]


def probes(tier):
    out = []
    for cfg in CFGS:
        for pop in POPULATIONS:
            for lto in (0, 1):
                if tier == 'quick' and (cfg != 'default' or pop is not POPULATIONS[0] or lto):
                    continue
                out.append(Probe(NAME, key_of(**pop, lto=lto, **cfg_dims(cfg)),
                                 sources=lambda pop=pop, cfg=cfg: sources(pop, cfg),
                                 lto=bool(lto)))
    return out


def describe(pop):
    """The population in words, for the report."""
    queues = f"{pop['queues']} queue{'' if pop['queues'] == 1 else 's'}"
    return (f"{pop['coros']} coroutines over {pop['types']} value types, {pop['holders']} holders,"
            f" {queues} of {pop['qtypes']} element types, {pop['ll']} `ll_source`s,"
            f" {pop['seq']} `seq_source`s")


def ret(t):
    if t == 'void':
        return 'co_return;'
    if t.startswith('vs'):
        return f'co_return {t}{{}};'
    if t == 'bool':
        return 'co_return true;'
    if t == 'float':
        return 'co_return 1.0f;'
    if t == 'char':
        return "co_return 'c';"
    return 'co_return 1;'


def await_child(j, t):
    return f'co_await c{j}( ctx );' if t == 'void' else f'{{ auto v = co_await c{j}( ctx ); sink( &v ); }}'


def pick(n, *key):
    """A number below n that depends only on `key`, the same on every machine."""
    digest = hashlib.blake2b(repr(key).encode(), digest_size=8).digest()
    return int.from_bytes(digest, 'little') % n


def element_size(i):
    return 1 + 4 * (i % 4)


def sources(pop, cfg):
    S = pop
    coros, types, holders = S['coros'], S['types'], S['holders']
    queues, qtypes, nll, nseq = S['queues'], S['qtypes'], S['ll'], S['seq']
    others = coros - holders
    vt = VT[:types]
    files = []

    files.append(('sys.hpp', '''#pragma once
#include <ecor/ecor.hpp>

#include <cstdint>

using namespace ecor;

extern "C" {
void     sink( void const* );
void     sink_i( int );
void     sink_b( bool );
uint32_t now();
bool     failed();
}
task_ctx& the_ctx();
''' + ptask_alias('owning_shared' if cfg == 'owning' else cfg)))

    L = ['#include "sys.hpp"', '', '#include <span>', '',
         'extern "C" {',
         '[[gnu::noinline]] void sink( void const* p ) { asm volatile( "" : : "r"( p ) : "memory" ); }',
         '[[gnu::noinline]] void sink_i( int i ) { asm volatile( "" : : "r"( i ) : "memory" ); }',
         '[[gnu::noinline]] void sink_b( bool b ) { asm volatile( "" : : "r"( b ) : "memory" ); }',
         '[[gnu::noinline]] uint32_t now() { return *(uint32_t volatile*) 0x40000000; }',
         '[[gnu::noinline]] bool failed() { return *(uint8_t volatile*) 0x40000004 != 0; }',
         '/// Static destructor registration: the program never exits.',
         'void* __dso_handle = nullptr;',
         'int   __cxa_atexit( void ( * )( void* ), void*, void* ) { return 0; }',
         '/// __libc_init_array() calls it; the startup code that defines it is left out.',
         'void _init() {}',
         '}', '',
         'static uint8_t                           frames[32768];',
         'static circular_buffer_memory< uint16_t > mem{ std::span{ frames } };',
         'static task_ctx                           ctx{ mem };',
         'task_ctx& the_ctx() { return ctx; }']
    files.append(('sys.cpp', '\n'.join(L) + '\n'))

    # ---- value, element and source types, queues and sources as globals behind accessors ----
    L = ['#pragma once', '#include "sys.hpp"', '']
    for n in range(1, 6):
        L.append(f'struct vs{n} {{ uint8_t d[{4 * n}]; }};')
    L.append('')
    for q in range(queues):
        for i in range(qtypes):
            L.append(f'struct e{q}_{i} {{ uint8_t d[{element_size(i)}]; }};')
    L += ['', 'struct qcfg : queue_default_cfg< circular_buffer_memory< uint16_t > >', '{',
          '        using push_and_wait_signatures =',
          '            completion_signatures< set_value_t(), set_error_t( task_error ), set_stopped_t() >;',
          '};']
    for q in range(queues):
        L.append(f'using q{q}_t = async_queue< qcfg, {", ".join(f"e{q}_{i}" for i in range(qtypes))} >;')
        L.append(f'q{q}_t& q{q}();')
    L.append('')
    for k in range(nll):
        pay = 'unit' if k == 0 else f'lp{k}'
        if k:
            L.append(f'struct lp{k} {{ uint8_t d[{1 + k % 5}]; }};')
        L.append(f'using ll{k}_t = ll_source< {pay}, {SIGS[k % 4][0]} >;')
        L.append(f'll{k}_t& ll{k}();')
    for k in range(nseq):
        pay = 'unit' if k == 0 else f'sp{k}'
        if k:
            L.append(f'struct sp{k} {{ uint8_t d[{1 + k % 3}]; }};')
        L.append(f'using seq{k}_t = seq_source< {KEYS[k % 3]}, {pay}, {SIGS[k % 4][0]} >;')
        L.append(f'seq{k}_t& seq{k}();')
    files.append(('types.hpp', '\n'.join(L) + '\n'))

    L = ['#include "types.hpp"', '', '#include <span>', '',
         'static uint8_t                           nodes[16384];',
         'static circular_buffer_memory< uint16_t > qmem{ std::span{ nodes } };']
    for q in range(queues):
        L.append(f'static q{q}_t q{q}_obj{{ qmem }};')
        L.append(f'q{q}_t& q{q}() {{ return q{q}_obj; }}')
    for k in range(nll):
        L.append(f'static ll{k}_t ll{k}_obj;')
        L.append(f'll{k}_t& ll{k}() {{ return ll{k}_obj; }}')
    for k in range(nseq):
        L.append(f'static seq{k}_t seq{k}_obj;')
        L.append(f'seq{k}_t& seq{k}() {{ return seq{k}_obj; }}')
    files.append(('types.cpp', '\n'.join(L) + '\n'))

    # ---- coroutine declarations ----
    copied = {k: k - 1 for k in range(11, others, 20)}
    vts = [vt[k % types] for k in range(others)]
    for k, original in copied.items():
        vts[k] = vts[original]

    def vtype(k):
        return vts[k]

    L = ['#pragma once', '#include "types.hpp"', '']
    for k in range(others):
        L.append(f'ptask< {vtype(k)} > c{k}( task_ctx& ctx );')
        if k % 20 == 18:
            L.append(f'ptask< int > rec{k}( task_ctx& ctx, int d );')
    for h in range(holders):
        L.append(f'ptask< void > root{h}( task_ctx& ctx );')
    files.append(('coros.hpp', '\n'.join(L) + '\n'))

    def await_source(*key):
        """One await of a source that `key` picks."""
        if nll and pick(2, 'kind', *key) == 0:
            k = pick(nll, 'll', *key)
            pay = '' if k == 0 else f'lp{k}{{}}'
            call = f'll{k}().schedule( {pay} )' if pay else f'll{k}().schedule()'
        else:
            k = pick(nseq, 'seq', *key)
            pay = '' if k == 0 else f', sp{k}{{}}'
            call = f'seq{k}().schedule( {KEYS[k % 3]}( now() ){pay} )'
        if SIGS[k % 4][1]:
            return f'sink_i( int( co_await {call} ) );'
        return f'co_await {call};'

    seen = set()

    def distinct(draw, name):
        """The first statements `draw( attempt )` returns that no function has had."""
        for attempt in range(1000):
            B = draw(attempt)
            if tuple(B) not in seen:
                seen.add(tuple(B))
                return B
        raise RuntimeError(f'{name}: no distinct body left')

    def statements(k, a):
        """A leaf's await count and a producer's extra await change only after 16 attempts."""
        shape = k % 20
        if shape < 12:
            B = [await_source('leaf', k, a, i) for i in range(1 + (k + a // 16) % 3)]
            if k % 4 == 0:
                B.append('co_await suspend;')
        elif shape < 16:
            B = []
            for i in range(2 + k % 4):
                j = pick(10, 'child', k, a, i) if pick(5, 'popular', k, a, i) == 0 else \
                    pick(others, 'child', k, a, i)
                if j == k:
                    j = (k + 1) % others
                B.append(await_child(j, vtype(j)))
        elif shape < 18:
            B = []
            for i in range(60 + (k % 3) * 40):
                if i and i % 10 == 0:
                    B.append(f'sink_i( {i} );')
                B.append(await_source('long', k, a, i))
        elif shape == 18:
            B = [f'sink_i( co_await rec{k}( ctx, 3 ) );']
        else:
            q, i = pick(queues, 'queue', k, a), pick(qtypes, 'element', k, a)
            B = [f'e{q}_{i} ev{{}};']
            if a >= 16:
                B.append(await_source('producer', k, a))
            B += [f'sink_b( q{q}().try_push( e{q}_{i}{{}} ) );',
                  f'co_await q{q}().push_and_wait( ev );',
                  'sink( &ev );']
        return B

    def recursion(k, a):
        """A recursive helper's statements, `SELF` standing for its name."""
        wait = await_source('rec', k, a)
        recurse = ['if ( d > 0 )', '        sink_i( co_await SELF( ctx, d - 1 ) );']
        return [wait] + recurse if pick(2, 'order', k, a) else recurse + [wait]

    bodies = {}
    for k in range(others):
        if k in copied:
            bodies[k] = bodies[copied[k]]
        else:
            bodies[k] = distinct(lambda a, k=k: statements(k, a), f'c{k}')

    for tu, start in enumerate(range(0, others, PER_TU)):
        L = ['#include "coros.hpp"', '']
        for k in range(start, min(others, start + PER_TU)):
            L.append(f'ptask< {vtype(k)} > c{k}( task_ctx& ctx )')
            L.append('{')
            L.append('        (void) ctx;')
            L += ['        ' + s for s in bodies[k]]
            L.append('        ' + ret(vtype(k)))
            L.append('}')
            if k % 20 == 18:
                L.append(f'ptask< int > rec{k}( task_ctx& ctx, int d )')
                L.append('{')
                rec = distinct(lambda a, k=k: recursion(k, a), f'rec{k}')
                L += ['        ' + s.replace('SELF', f'rec{k}') for s in rec]
                L.append('        co_return d;')
                L.append('}')
            L.append('')
        files.append((f'coros{tu}.cpp', '\n'.join(L) + '\n'))

    L = ['#include "coros.hpp"', '']
    for h in range(holders):
        L.append(f'ptask< void > root{h}( task_ctx& ctx )')
        L.append('{')
        for k in range(h, others, holders):
            L.append('        ' + await_child(k, vtype(k)))
        L.append('        co_return;')
        L.append('}')
    files.append(('roots.cpp', '\n'.join(L) + '\n'))

    # ---- pumps and the entry point ----
    L = ['#include "coros.hpp"', '', 'extern "C" void __libc_init_array();', '', 'namespace', '{']
    for q in range(queues):
        for i in range(qtypes):
            handler = distinct(lambda a, q=q, i=i: [
                f'sink_i( ev.d[{pick(element_size(i), "field", q, i, a)}] );',
                await_source('handler', q, i, a)], f'on{q}_{i}')
            L.append(f'ptask< void > on{q}_{i}( task_ctx&, e{q}_{i}& ev )')
            L.append('{')
            L += ['        ' + s for s in handler]
            L.append('        co_return;')
            L.append('}')
    L.append('struct pcfg : pump_default_cfg< task_ctx, ptask< void > >')
    L.append('{')
    L.append('};')
    for q in range(queues):
        L.append(f'struct h{q}')
        L.append('{')
        for i in range(qtypes):
            L.append(f'        ptask< void > handle( task_ctx& ctx, e{q}_{i}& ev )')
            L.append('        {')
            L.append(f'                return on{q}_{i}( ctx, ev );')
            L.append('        }')
        L.append('};')
    L += ['}  // namespace', '', 'extern "C" [[gnu::used]] void _start()', '{',
          '        __libc_init_array();', '        auto& ctx = the_ctx();']
    for q in range(queues):
        L.append(f'        h{q}                                hh{q}{{}};')
        L.append(f'        event_pump< pcfg, q{q}_t, h{q} > p{q}{{ ctx, q{q}(), hh{q} }};')
        L.append(f'        p{q}.start();')
    for h in range(holders):
        if cfg == 'default':
            L.append(f'        task_holder th{h}{{ ctx, []( task_ctx& c ) {{ return root{h}( c ); }} }};')
        else:
            L.append(f'        auto f{h} = []( task_ctx& c ) {{ return root{h}( c ); }};')
            L.append(f'        task_holder< owning_cfg, task_ctx, decltype( f{h} ) > th{h}{{ ctx, f{h} }};')
        L.append(f'        th{h}.start();')
    L += ['        for ( ;; ) {', '                uint32_t const i = now();']
    for q in range(queues):
        for i in range(qtypes):
            L.append(f'                if ( i % {qtypes} == {i} )')
            L.append(f'                        sink_b( q{q}().try_push( e{q}_{i}{{}} ) );')
    for k in range(nll):
        L.append(f'                broadcast( ll{k}(), []( auto& e ) {{ e.set_value( {SIGS[k % 4][1]} ); }} );')
    for k in range(nseq):
        L.append(f'                if ( auto* e = seq{k}().query_next() )')
        L.append(f'                        e->set_value( {SIGS[k % 4][1]} );')
    L += ['                ctx.core.run_n( 64 );', '        }', '}']
    files.append(('main.cpp', '\n'.join(L) + '\n'))
    return files


def buckets(path):
    """Flash bytes per bucket of BUCKETS from the ELF's sized symbols, 'other' for the rest."""
    out = subprocess.run(['arm-none-eabi-nm', '--print-size', '--size-sort', '--radix=d', '-C', path],
                         capture_output=True, text=True, check=True).stdout
    sizes = {name: 0 for name, _ in BUCKETS}
    sizes['other'] = 0
    for line in out.splitlines():
        p = line.split(None, 3)
        if len(p) < 4 or p[2] not in 'TtWwRrVv':
            continue
        size, name = int(p[1]), p[3]
        for bucket, rx in BUCKETS:
            if rx.search(name):
                sizes[bucket] += size
                break
        else:
            sizes['other'] += size
    return sizes


def report(results, headers, w):
    names = [b for b, _ in BUCKETS] + ['other']
    w('### complete programs: `.text + .rodata`, `.data + .bss`, and the flash by symbol family')
    w('')
    w('Buckets come from the demangled names of sized symbols, so code inlined elsewhere counts'
      ' where it landed; `.data + .bss` is dominated by the 32 KiB frame and 16 KiB node buffers.'
      ' The owning rows run their tasks with `promise_owns_operations` and'
      ' `shared_await_suspend`, their delta against the default configuration in parentheses.')
    w('')
    w('| program | cfg | lto | flash | ram | ' + ' | '.join(names) + ' |')
    w('|---|---|---:|---:|---:|' + '---:|' * len(names))
    for pop in POPULATIONS:
        for cfg in CFGS:
            for lto in (0, 1):
                k = key_of(**pop, lto=lto, **cfg_dims(cfg))
                m = results.get(('cur', k))
                if m is None:
                    continue
                base = None if cfg == 'default' else flash_of(results, key_of(**pop, lto=lto))
                c = cell(m.flash, base)
                for h in headers[1:]:
                    o = flash_of(results, k, h)
                    if o is not None:
                        c += f' [{h}: {m.flash - o:+d}]'
                b = buckets(m.path)
                w(f'| {describe(pop)} | {cfg} | {lto} | {c} | {m.ram} | '
                  + ' | '.join(str(b[n]) for n in names) + ' |')
    w('')
