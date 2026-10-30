"""`async_queue`: every valid configuration, and how the interesting ones scale.

A probe is one translation unit of `queues` queues over one opaque memory resource, each with
`types` element types of its own, exercised with everything its configuration has: `try_push()`
and `try_pop()` for every element type, `push()` per type where push parks, one `pop()` where
pop parks, `close()` and `closed()` where closeable, and `push_and_wait()` from one
`task< void >` per type where the call set is non-empty, drained by an `event_pump` whose
handler completes the call. Receivers carry a live stop token (`stop=1`) or an empty environment
(`stop=0`). `owning=1` makes every element type move-only with a destructor.

Dimensions: pop, push in try_only | parking | cancellable; close 0 | 1; call in none | value |
value_stop | reply_stop | full; stop 0 | 1; types 1 | 4 | 8; queues 1 | 2 | 4; owning 0 | 1.
"""
from .common import BLK, IMM_SENDER, Probe, SYS_MEM, TASK_RECV, cell, flash_of, key_of, params_of

NAME = 'queue'

APIS = ('try_only', 'parking', 'cancellable')

# call set name: (push_and_wait_signatures, consumer handler kind)
CALLS = {
    'none': (None, None),
    'value': ('set_value_t()', 'imm'),
    'value_stop': ('set_value_t(), set_stopped_t()', 'imm'),
    'reply_stop': ('set_value_t( uint16_t ), set_stopped_t()', 'imm'),
    'full': ('set_value_t(), set_error_t( task_error ), set_stopped_t()', 'task'),
}

DEFAULT = ('try_only', 'try_only', 0, 'none')

# The configurations scaled along the other dimensions.
INTERESTING = [
    DEFAULT,
    ('try_only', 'try_only', 1, 'none'),
    ('parking', 'try_only', 0, 'none'),
    ('cancellable', 'try_only', 0, 'none'),
    ('parking', 'parking', 0, 'none'),
    ('cancellable', 'cancellable', 0, 'none'),
    ('cancellable', 'cancellable', 1, 'none'),
    ('try_only', 'try_only', 0, 'value_stop'),
    ('try_only', 'try_only', 0, 'reply_stop'),
    ('try_only', 'try_only', 0, 'full'),
    ('try_only', 'try_only', 1, 'full'),
]

SHAPES = ('uint8_t v;', 'uint32_t v;', 'uint32_t a, b;', 'uint8_t d[16];', 'uint16_t v;',
          'uint64_t v;', 'uint8_t d[32];', 'uint32_t a, b, c;')


def valid(pop, push, close, call):
    """The configurations the header accepts: a closeable queue's call set needs the stopped
    row, and a call set without a nullary value row leaves the queue without `pop()`."""
    if close and call == 'value':
        return False
    if call == 'reply_stop' and pop != 'try_only':
        return False
    return True


def probes(tier):
    out = {}

    def add(cfg, stop, types, queues, owning):
        pop, push, close, call = cfg
        kw = dict(pop=pop, push=push, close=close, call=call, stop=stop, types=types,
                  queues=queues, owning=owning)
        k = key_of(**kw)
        out.setdefault(k, Probe(NAME, k, lambda kw=kw: source(**kw)))

    if tier == 'quick':
        for cfg in INTERESTING:
            add(cfg, 1, 4, 1, 0)
        for t in (1, 8):
            add(DEFAULT, 1, t, 1, 0)
            add(INTERESTING[6], 1, t, 1, 0)
        for q in (2, 4):
            add(DEFAULT, 1, 4, q, 0)
    else:
        for pop in APIS:
            for push in APIS:
                for close in (0, 1):
                    for call in CALLS:
                        if valid(pop, push, close, call):
                            for stop in (0, 1):
                                add((pop, push, close, call), stop, 4, 1, 0)
        for cfg in INTERESTING:
            for t in (1, 4, 8):
                add(cfg, 1, t, 1, 0)
                add(cfg, 1, t, 1, 1)
            for q in (2, 4):
                add(cfg, 1, 4, q, 0)
    return list(out.values())


def source(pop, push, close, call, stop, types, queues, owning):
    sigs, kind = CALLS[call]
    tok = 'the_token()' if stop else ''
    L = []
    w = L.append
    w(f'// async_queue probe: {key_of(pop=pop, push=push, close=close, call=call, stop=stop, types=types, queues=queues, owning=owning)}')
    w('#include <ecor/ecor.hpp>')
    w('')
    w('#include <cstdint>')
    w('')
    w('using namespace ecor;')
    w('')
    if owning:
        w('extern "C" void release( void* );')
        w('')
    w(SYS_MEM)
    w('namespace')
    w('{')
    if owning:
        w(BLK)
    for q in range(queues):
        for i in range(types):
            w(f'struct e{q}_{i} {{ {SHAPES[i % len(SHAPES)]}{" blk h;" if owning else ""} }};')
    w('')
    w('struct cfg : queue_default_cfg< sys_mem >')
    w('{')
    w(f'        static constexpr async_queue_api pop_api      = async_queue_api::{pop};')
    w(f'        static constexpr async_queue_api push_api     = async_queue_api::{push};')
    w(f'        static constexpr bool            is_closeable = {"true" if close else "false"};')
    if owning:
        w('        static constexpr bool trivially_copyable_only = false;')
    if sigs:
        w(f'        using push_and_wait_signatures = completion_signatures< {sigs} >;')
    w('};')
    for q in range(queues):
        w(f'using q{q}_t = async_queue< cfg, {", ".join(f"e{q}_{i}" for i in range(types))} >;')
    w('}  // namespace')
    w('')
    w('extern sys_mem& the_mem();')
    if stop:
        w('extern inplace_stop_token the_token();')
    if kind:
        w('extern task_ctx& the_ctx();')
    w('extern void sink( void const* );')
    w('extern void sink_b( bool );')
    if call == 'reply_stop':
        w('extern void sink_i( int );')
    w('')
    if kind:
        w(TASK_RECV)
    w('namespace')
    w('{')
    if stop:
        w('struct env_holder')
        w('{')
        w('        inplace_stop_token token;')
        w('        [[nodiscard]] stop_token_env< inplace_stop_token > get_env() const noexcept')
        w('        {')
        w('                return { token };')
        w('        }')
        w('};')
    else:
        w('struct env_holder')
        w('{')
        w('        [[nodiscard]] empty_env get_env() const noexcept')
        w('        {')
        w('                return {};')
        w('        }')
        w('};')
    w('')
    w('struct sig_recv : env_holder')
    w('{')
    w('        using receiver_concept = receiver_t;')
    w('        void set_value() noexcept { sink( this ); }')
    w('        void set_stopped() noexcept { sink( nullptr ); }')
    w('};')
    if pop != 'try_only':
        for q in range(queues):
            w(f'struct pop_recv{q} : env_holder')
            w('{')
            w('        using receiver_concept = receiver_t;')
            for i in range(types):
                w(f'        void set_value( e{q}_{i}& v ) noexcept {{ sink( &v ); }}')
            w('        void set_stopped() noexcept { sink( nullptr ); }')
            w('};')
    if kind:
        if kind == 'imm':
            w(IMM_SENDER)
            reply = 'true' if call == 'reply_stop' else 'false'
            w(f'using imm_t = imm< completion_signatures< {sigs} >, {reply} >;')
            handler_ret = 'imm_t'
        else:
            handler_ret = 'task< void >'
            # A member function cannot be a task coroutine: its implicit object parameter would
            # come before the context. The handler returns a free coroutine's task.
            for q in range(queues):
                for i in range(types):
                    w(f'task< void > on{q}_{i}( task_ctx&, e{q}_{i}& ev )')
                    w('{')
                    w('        sink( &ev );')
                    w('        co_return;')
                    w('}')
        for q in range(queues):
            w(f'struct h{q}')
            w('{')
            for i in range(types):
                if kind == 'task':
                    w(f'        task< void > handle( task_ctx& ctx, e{q}_{i}& ev )')
                    w('        {')
                    w(f'                return on{q}_{i}( ctx, ev );')
                    w('        }')
                else:
                    w(f'        imm_t handle( task_ctx&, e{q}_{i}& ev )')
                    w('        {')
                    w('                sink( &ev );')
                    w('                return { uint16_t( 7 ) };')
                    w('        }')
            w('};')
            w(f'using pump{q}_t = event_pump< pump_default_cfg< task_ctx, {handler_ret} >, q{q}_t, h{q} >;')
        for q in range(queues):
            for i in range(types):
                w(f'task< void > call{q}_{i}( task_ctx&, q{q}_t& q )')
                w('{')
                w(f'        e{q}_{i} ev{{}};')
                if call == 'reply_stop':
                    w('        sink_i( co_await q.push_and_wait( ev ) );')
                else:
                    w('        co_await q.push_and_wait( ev );')
                w('        sink( &ev );')
                w('}')
    w('}  // namespace')
    w('')
    w('void probe()')
    w('{')
    if kind:
        w('        auto& ctx = the_ctx();')
    for q in range(queues):
        w(f'        q{q}_t q{q}{{ the_mem() }};')
        if kind:
            w(f'        h{q}      hh{q}{{}};')
            w(f'        pump{q}_t p{q}{{ ctx, q{q}, hh{q} }};')
            w(f'        p{q}.start();')
        for i in range(types):
            t = f'e{q}_{i}'
            if push != 'try_only':
                w(f'        {{ auto op = q{q}.push( {t}{{}} ).connect( sig_recv{{ {tok} }} ); op.start(); sink( &op ); }}')
            w(f'        sink_b( q{q}.try_push( {t}{{}} ) );')
        if call != 'reply_stop':
            w(f'        {{ auto v = q{q}.try_pop(); v.visit( []( auto& x ) {{ sink( &x ); }} ); }}')
            if pop != 'try_only':
                w(f'        {{ auto op = q{q}.pop().connect( pop_recv{q}{{ {tok} }} ); op.start(); sink( &op ); }}')
        if close:
            w(f'        {{ auto op = q{q}.close().connect( sig_recv{{ {tok} }} ); op.start(); sink( &op ); sink_b( q{q}.closed() ); }}')
        if kind:
            for i in range(types):
                w(f'        {{ auto op = call{q}_{i}( ctx, q{q} ).connect( dr{{}} ); op.start(); sink( &op ); }}')
    if kind:
        w('        ctx.core.run_n( 64 );')
    w('}')
    return '\n'.join(L) + '\n'


def _cfg_label(pop, push, close, call):
    return f'pop={pop} push={push} close={close} call={call}'


def report(results, headers, w):
    """Two tables: every configuration at four element types against the default, and the
    interesting configurations' scaling over element types, queues and owning types."""
    def get(h, cfg, stop=1, types=4, queues=1, owning=0):
        pop, push, close, call = cfg
        k = key_of(pop=pop, push=push, close=close, call=call, stop=stop, types=types,
                   queues=queues, owning=owning)
        return flash_of(results, k, h)

    def with_ref(v, cfg, base=None, **kw):
        c = cell(v, base)
        for h in headers[1:]:
            o = get(h, cfg, **kw)
            if o is not None and v is not None:
                c += f' [{h}: {v - o:+d}]'
        return c

    cfgs = sorted({tuple(params_of(k)[d] for d in ('pop', 'push', 'close', 'call'))
                   for (h, k) in results if h == 'cur'})
    cfgs = [(p, u, int(c), l) for p, u, c, l in cfgs]
    w('### queue configurations: four element types, one queue, bytes of `.text + .rodata`')
    w('')
    w('Cells: total, then the delta against the default configuration with the same'
      ' receivers; a bracket is the delta of `cur` against another header variant.')
    w('')
    w('| configuration | stop=0 | stop=1 |')
    w('|---|---:|---:|')
    rows = []
    for cfg in cfgs:
        cells = []
        for stop in (0, 1):
            v = get('cur', cfg, stop=stop)
            base = get('cur', DEFAULT, stop=stop)
            cells.append(with_ref(v, cfg, base if cfg != DEFAULT else None, stop=stop))
        if any(c != '-' for c in cells):
            order = get('cur', cfg, stop=1) or get('cur', cfg, stop=0) or 0
            rows.append((order, f'| {_cfg_label(*cfg)} | ' + ' | '.join(cells) + ' |'))
    for _, r in sorted(rows):
        w(r)
    w('')
    w('### queue scaling: the interesting configurations, stoppable receivers')
    w('')
    w('Per element type is (8 types − 1 type) / 7; per queue is (4 queues − 1) / 3, each'
      ' queue with four element types of its own on one memory resource.')
    w('')
    w('| configuration | 1 type | 4 types | 8 types | per type | owning: 1 | 4 | 8 | per type'
      ' | 2 queues | 4 queues | per queue |')
    w('|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|')
    for cfg in INTERESTING:
        t = {n: get('cur', cfg, types=n) for n in (1, 4, 8)}
        o = {n: get('cur', cfg, types=n, owning=1) for n in (1, 4, 8)}
        q = {n: get('cur', cfg, queues=n) for n in (1, 2, 4)}
        if all(v is None for v in list(t.values()) + list(o.values()) + list(q.values())):
            continue

        def slope(d, hi, lo, n):
            return '-' if d[hi] is None or d[lo] is None else f'{(d[hi] - d[lo]) / n:.1f}'

        cells = [cell(t[1]), cell(t[4]), cell(t[8]), slope(t, 8, 1, 7),
                 cell(o[1]), cell(o[4]), cell(o[8]), slope(o, 8, 1, 7),
                 cell(q[2]), cell(q[4]), slope(q, 4, 1, 3)]
        w(f'| {_cfg_label(*cfg)} | ' + ' | '.join(cells) + ' |')
    w('')
