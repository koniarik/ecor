"""`async_arena`: arenas per program, distinct memory resource types, object types per arena.

A probe is one translation unit of `arenas` arenas over `mems` distinct memory resource types
(one shared by all, or one per arena: an opaque `sys_mem`, `circular_buffer_memory< uint16_t >`,
`circular_buffer_memory< uint32_t >`, a second opaque type). Each arena makes one object of
each of its `objs` object types, whose `async_destroy` returns an opaque source's sender, drops
the pointer, and is asked to `async_destroy()` itself.

Dimensions: arenas 1 | 2 | 4; mems 1 | n; objs 1 | 4.
"""
from .common import Probe, SYS_MEM, TASK_RECV, cell, flash_of, key_of

NAME = 'arena'

MEMS = ('sys_mem', 'circular_buffer_memory< uint16_t >', 'circular_buffer_memory< uint32_t >',
        'sys_mem2')

BASE = dict(arenas=1, mems=1, objs=1)


def probes(tier):
    out = {}

    def add(**over):
        kw = dict(BASE, **over)
        if kw['arenas'] == 1:
            kw['mems'] = 1
        k = key_of(**kw)
        out.setdefault(k, Probe(NAME, k, lambda kw=kw: source(**kw)))

    if tier == 'quick':
        add()
        add(arenas=4)
        add(arenas=4, mems='n')
        add(objs=4)
    else:
        for arenas in (1, 2, 4):
            for mems in (1, 'n'):
                for objs in (1, 4):
                    add(arenas=arenas, mems=mems, objs=objs)
    return list(out.values())


def source(arenas, mems, objs):
    L = []
    w = L.append
    w(f'// async_arena probe: {key_of(arenas=arenas, mems=mems, objs=objs)}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('#include <span>')
    w('')
    w('using namespace ecor;')
    w('')
    w(SYS_MEM)
    w(SYS_MEM.replace('sys_mem', 'sys_mem2'))
    w('using src_t = ll_source< unit, set_value_t() >;')
    w('extern task_ctx& the_ctx();')
    w('extern sys_mem&  the_mem();')
    w('extern sys_mem2& the_mem2();')
    w('extern src_t&    the_src();')
    w('extern void      sink( void const* );')
    w('')
    w(TASK_RECV)
    w('namespace')
    w('{')
    for a in range(arenas):
        for o in range(objs):
            w(f'struct obj{a}_{o}')
            w('{')
            w(f'        uint8_t d[{4 + 4 * o}];')
            w('        src_t&  s;')
            w('        auto    async_destroy()')
            w('        {')
            w('                return s.schedule();')
            w('        }')
            w('};')
    w('}  // namespace')
    w('')
    w('void probe()')
    w('{')
    w('        auto& ctx = the_ctx();')
    w('        uint8_t buf16[256];')
    w('        uint8_t buf32[256];')
    w('        circular_buffer_memory< uint16_t > cb16{ std::span{ buf16 } };')
    w('        circular_buffer_memory< uint32_t > cb32{ std::span{ buf32 } };')
    w('        sink( &cb16 );')
    w('        sink( &cb32 );')
    for a in range(arenas):
        m = 0 if mems == 1 else a % len(MEMS)
        mem_expr = ('the_mem()', 'cb16', 'cb32', 'the_mem2()')[m]
        w(f'        async_arena< task_ctx, {MEMS[m]} > ar{a}{{ ctx, {mem_expr} }};')
        for o in range(objs):
            w(f'        {{ auto p = ar{a}.make< obj{a}_{o} >( obj{a}_{o}{{ {{}}, the_src() }} ); sink( &p ); }}')
        w(f'        {{ auto d = ar{a}.async_destroy().connect( dr{{}} ); d.start(); sink( &d ); }}')
    w('        ctx.core.run_n( 64 );')
    w('}')
    return '\n'.join(L) + '\n'


def report(results, headers, w):
    def get(**over):
        kw = dict(BASE, **over)
        if kw['arenas'] == 1:
            kw['mems'] = 1
        return flash_of(results, key_of(**kw))

    w('### arenas per program')
    w('')
    w('Per arena is (4 arenas − 1) / 3. `mems=n` gives every arena a memory resource type of'
      ' its own.')
    w('')
    w('| mems | objs | 1 arena | 2 | 4 | per arena |')
    w('|---|---:|---:|---:|---:|---:|')
    for mems in (1, 'n'):
        for objs in (1, 4):
            v = {n: get(arenas=n, mems=mems, objs=objs) for n in (1, 2, 4)}
            if all(x is None for x in v.values()):
                continue
            slope = '-' if v[4] is None or v[1] is None else f'{(v[4] - v[1]) / 3:.1f}'
            w(f'| {mems} | {objs} | {cell(v[1])} | {cell(v[2])} | {cell(v[4])} | {slope} |')
    w('')
