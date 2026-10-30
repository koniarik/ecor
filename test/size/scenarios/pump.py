"""`event_pump`: pumps per program, stoppability, handler kinds, element types, queue flags.

A probe is one translation unit of `pumps` pumps, each draining a queue of its own with `types`
element types over one opaque memory resource. Every element type is pushed once with
`try_push()`; a stoppable pump is also asked to `stop()`. The handler returns an immediate
sender (`imm`), a `task< void >` from a free coroutine (`task`), or that with `on_error()` and
`on_stopped()` hooks (`hooks`). On a `call` queue the handler completes `push_and_wait()` calls
as well, and one coroutine per queue makes such a call.

Dimensions:
  pumps      1 | 2 | 4 | 8
  stoppable  0 | 1
  handler    imm | task | hooks
  types      1 | 4 | 8 element types per queue
  qcfg       default | closeable | call (a `push_and_wait` set: value and stopped rows for
             `imm`, value, error and stopped rows for a task handler)
"""
from .common import IMM_SENDER, Probe, SYS_MEM, TASK_RECV, cell, flash_of, key_of

NAME = 'pump'

HANDLERS = ('imm', 'task', 'hooks')
QCFGS = ('default', 'closeable', 'call')

BASE = dict(pumps=1, stoppable=0, handler='imm', types=4, qcfg='default')


def probes(tier):
    out = {}

    def add(**over):
        kw = dict(BASE, **over)
        k = key_of(**kw)
        out.setdefault(k, Probe(NAME, k, lambda kw=kw: source(**kw)))

    if tier == 'quick':
        for handler in ('imm', 'task'):
            for stoppable in (0, 1):
                add(handler=handler, stoppable=stoppable)
                add(handler=handler, stoppable=stoppable, pumps=8)
        add(handler='hooks', stoppable=1)
        add(qcfg='call', handler='task')
        add(types=8)
    else:
        for handler in ('imm', 'task'):
            for stoppable in (0, 1):
                for pumps in (1, 2, 4, 8):
                    add(handler=handler, stoppable=stoppable, pumps=pumps)
                for types in (1, 4, 8):
                    add(handler=handler, stoppable=stoppable, types=types)
        for qcfg in QCFGS:
            for handler in HANDLERS:
                for stoppable in (0, 1):
                    add(qcfg=qcfg, handler=handler, stoppable=stoppable)
        for pumps in (1, 8):
            add(handler='hooks', stoppable=1, pumps=pumps)
    return list(out.values())


def source(pumps, stoppable, handler, types, qcfg):
    sender_sigs = 'set_value_t(), set_stopped_t()'
    call_sigs = None
    if qcfg == 'call':
        call_sigs = sender_sigs if handler == 'imm' else \
            'set_value_t(), set_error_t( task_error ), set_stopped_t()'
    L = []
    w = L.append
    w(f'// event_pump probe: {key_of(pumps=pumps, stoppable=stoppable, handler=handler, types=types, qcfg=qcfg)}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    w(SYS_MEM)
    w('extern sys_mem&  the_mem();')
    w('extern task_ctx& the_ctx();')
    w('extern void      sink( void const* );')
    w('extern void      sink_b( bool );')
    w('')
    w(TASK_RECV)
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
    for q in range(pumps):
        for i in range(types):
            w(f'struct e{q}_{i} {{ uint8_t d[{1 + 4 * (i % 4)}]; }};')
    w('')
    w('struct qcfg : queue_default_cfg< sys_mem >')
    w('{')
    if qcfg == 'closeable':
        w('        static constexpr bool is_closeable = true;')
    if call_sigs:
        w(f'        using push_and_wait_signatures = completion_signatures< {call_sigs} >;')
    w('};')
    for q in range(pumps):
        w(f'using q{q}_t = async_queue< qcfg, {", ".join(f"e{q}_{i}" for i in range(types))} >;')
    w('')
    if handler == 'imm':
        w(IMM_SENDER)
        w(f'using handler_sender = imm< completion_signatures< {sender_sigs} >, false >;')
    else:
        w('using handler_sender = task< void >;')
        for q in range(pumps):
            for i in range(types):
                w(f'task< void > on{q}_{i}( task_ctx&, e{q}_{i}& ev )')
                w('{')
                w('        sink( &ev );')
                w('        co_return;')
                w('}')
    w('')
    w('struct pcfg : pump_default_cfg< task_ctx, handler_sender >')
    w('{')
    if stoppable:
        w('        static constexpr bool is_stoppable = true;')
    w('};')
    w('')
    for q in range(pumps):
        w(f'struct h{q}')
        w('{')
        for i in range(types):
            if handler == 'imm':
                w(f'        handler_sender handle( task_ctx&, e{q}_{i}& ev )')
                w('        {')
                w('                sink( &ev );')
                w('                return { uint16_t( 7 ) };')
                w('        }')
            else:
                w(f'        handler_sender handle( task_ctx& ctx, e{q}_{i}& ev )')
                w('        {')
                w(f'                return on{q}_{i}( ctx, ev );')
                w('        }')
        if handler == 'hooks':
            w('        void on_error( task_error& e )')
            w('        {')
            w('                sink( &e );')
            w('        }')
            w('        void on_stopped()')
            w('        {')
            w('                sink( this );')
            w('        }')
        w('};')
        w(f'using pump{q}_t = event_pump< pcfg, q{q}_t, h{q} >;')
    if call_sigs:
        for q in range(pumps):
            w(f'task< void > call{q}( task_ctx&, q{q}_t& q )')
            w('{')
            w(f'        e{q}_0 ev{{}};')
            w('        co_await q.push_and_wait( ev );')
            w('        sink( &ev );')
            w('}')
    w('}  // namespace')
    w('')
    w('void probe()')
    w('{')
    w('        auto& ctx = the_ctx();')
    for q in range(pumps):
        w(f'        q{q}_t    q{q}{{ the_mem() }};')
        w(f'        h{q}      hh{q}{{}};')
        w(f'        pump{q}_t p{q}{{ ctx, q{q}, hh{q} }};')
        w(f'        p{q}.start();')
        for i in range(types):
            w(f'        sink_b( q{q}.try_push( e{q}_{i}{{}} ) );')
        if call_sigs:
            w(f'        {{ auto o = call{q}( ctx, q{q} ).connect( dr{{}} ); o.start(); sink( &o ); }}')
        if stoppable:
            w(f'        {{ auto o = p{q}.stop().connect( done_recv{{}} ); o.start(); sink( &o ); }}')
    w('        ctx.core.run_n( 64 );')
    w('}')
    return '\n'.join(L) + '\n'


def report(results, headers, w):
    def get(h='cur', **over):
        return flash_of(results, key_of(**dict(BASE, **over)), h)

    def slope(a, b, n):
        return '-' if a is None or b is None else f'{(b - a) / n:.1f}'

    w('### pumps per program: default queue, four element types each')
    w('')
    w('| handler | stoppable | 1 pump | 2 | 4 | 8 | per pump |')
    w('|---|---:|---:|---:|---:|---:|---:|')
    for handler in ('imm', 'task', 'hooks'):
        for stoppable in (0, 1):
            p = {n: get(handler=handler, stoppable=stoppable, pumps=n) for n in (1, 2, 4, 8)}
            if all(v is None for v in p.values()):
                continue
            w(f'| {handler} | {stoppable} | {cell(p[1])} | {cell(p[2])} | {cell(p[4])} |'
              f' {cell(p[8])} | {slope(p[1], p[8], 7)} |')
    w('')
    w('### element types per queue: one pump, default queue')
    w('')
    w('| handler | stoppable | 1 type | 4 | 8 | per type |')
    w('|---|---:|---:|---:|---:|---:|')
    for handler in ('imm', 'task'):
        for stoppable in (0, 1):
            t = {n: get(handler=handler, stoppable=stoppable, types=n) for n in (1, 4, 8)}
            if all(v is None for v in t.values()):
                continue
            w(f'| {handler} | {stoppable} | {cell(t[1])} | {cell(t[4])} | {cell(t[8])} |'
              f' {slope(t[1], t[8], 7)} |')
    w('')
    w('### queue flags under a pump: one pump, four element types')
    w('')
    w('Cells: total, then the delta against the default queue with the same handler and'
      ' stoppability.')
    w('')
    cols = [(h, s) for h in HANDLERS for s in (0, 1)]
    w('| queue | ' + ' | '.join(f'{h}, stop={s}' for h, s in cols) + ' |')
    w('|---|' + '---:|' * len(cols))
    for qcfg in QCFGS:
        cells = []
        for h, s in cols:
            v = get(qcfg=qcfg, handler=h, stoppable=s)
            base = get(handler=h, stoppable=s) if qcfg != 'default' else None
            cells.append(cell(v, base))
        if any(c != '-' for c in cells):
            w(f'| {qcfg} | ' + ' | '.join(cells) + ' |')
    w('')
