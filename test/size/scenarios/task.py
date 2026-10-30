"""`task`: configurations, value types, coroutines, awaits per coroutine, and what is awaited.

A probe is one translation unit of `coros` coroutine functions whose value types cycle through
the first `types` of void, int, bool, uint16_t, uint8_t, int16_t, uint32_t and char, each
awaiting the sender set `kinds` `awaits` times with a `sink_i` call between rounds, each
connected to a receiver of its value type and started. The sources are opaque externals, so the
object holds the coroutines, their awaitables and the task machinery only.

Dimensions:
  cfg     default | err2 (two error signatures) | err3 (three) | trace (a trace type with state
          and two live hooks) | convert (two error signatures, `convert_error` maps to a wrapper)
  types   1 | 4 | 8 value types
  coros   1 | 4 | 16 coroutine functions
  awaits  1 | 4 | 8 | 32 rounds of the sender set per coroutine
  kinds   none | ll (a nullary `ll_source`) | core4 (ll, an `ll_source` completing with
          `uint32_t`, a `seq_source`, a child `task< uint8_t >`) | all7 (core4, `suspend`,
          `wait_until_stopped`, and one `co_yield with_error` at the end of the body)
  shared  1: every coroutine awaits the same source types; 0: each coroutine has source types
          of its own, so no two share an awaitable (the child task stays shared)
  stop    the root receivers carry a stop token (1) or an empty environment (0)
"""
from .common import Probe, cell, flash_of, key_of, params_of

NAME = 'task'

VT = ('void', 'int', 'bool', 'uint16_t', 'uint8_t', 'int16_t', 'uint32_t', 'char')
RET = ('co_return;', 'co_return 1;', 'co_return true;', 'co_return 2;', 'co_return 3;',
       'co_return 4;', 'co_return 5u;', "co_return 'c';")

KINDS = {
    'none': (),
    'll': ('ll',),
    'core4': ('ll', 'll32', 'seq', 'child'),
    'all7': ('ll', 'll32', 'seq', 'child', 'suspend', 'stop', 'yield'),
}

# cfg name: (error types the receivers need, config definition)
CFGS = {
    'default': (('task_error',), 'using cfg_t = task_default_cfg;'),
    'err2': (('task_error', 'my_error'), '''struct my_error
{
        uint8_t code;
};
struct cfg_t
{
        using error_signatures =
            completion_signatures< set_error_t( task_error ), set_error_t( my_error ) >;
        using trace_type = task_default_trace;
        static task_error convert_error( task_error e ) noexcept
        {
                return e;
        }
};'''),
    'err3': (('task_error', 'my_error', 'my_error2'), '''struct my_error
{
        uint8_t code;
};
struct my_error2
{
        uint16_t code;
};
struct cfg_t
{
        using error_signatures = completion_signatures<
            set_error_t( task_error ),
            set_error_t( my_error ),
            set_error_t( my_error2 ) >;
        using trace_type = task_default_trace;
        static task_error convert_error( task_error e ) noexcept
        {
                return e;
        }
};'''),
    'trace': (('task_error',), '''struct stateful_trace : task_default_trace
{
        uint32_t resumes = 0;
        void     on_resume( auto& ) noexcept
        {
                trace_sink( ++resumes );
        }
        void on_await_suspend( auto& ) noexcept
        {
                trace_sink( resumes );
        }
};
struct cfg_t
{
        using error_signatures = completion_signatures< set_error_t( task_error ) >;
        using trace_type       = stateful_trace;
        static task_error convert_error( task_error e ) noexcept
        {
                return e;
        }
};'''),
    'convert': (('task_error_wrap', 'my_error'), '''struct my_error
{
        uint8_t code;
};
struct task_error_wrap
{
        task_error inner;
        /*implicit*/ task_error_wrap( task_error e ) noexcept
          : inner( e )
        {
        }
};
struct cfg_t
{
        using error_signatures =
            completion_signatures< set_error_t( task_error_wrap ), set_error_t( my_error ) >;
        using trace_type = task_default_trace;
        static task_error_wrap convert_error( task_error e ) noexcept
        {
                return e;
        }
};'''),
}

BASE = dict(cfg='default', types=1, coros=4, awaits=4, kinds='core4', shared=1, stop=0)


def probes(tier):
    out = {}

    def add(**over):
        kw = dict(BASE, **over)
        k = key_of(**kw)
        out.setdefault(k, Probe(NAME, k, lambda kw=kw: source(**kw)))

    if tier == 'quick':
        for cfg in CFGS:
            add(cfg=cfg, types=4)
        for coros in (1, 16):
            for awaits in (1, 32):
                add(coros=coros, awaits=awaits)
                add(coros=coros, awaits=awaits, kinds='none')
        add(coros=16, awaits=32, shared=0)
        add(kinds='all7')
        add(stop=1)
    else:
        for cfg in CFGS:
            for types in (1, 4):
                add(cfg=cfg, types=types)
        for types in (1, 4, 8):
            for coros in (1, 4, 16):
                for awaits in (1, 4, 8, 32):
                    add(types=types, coros=coros, awaits=awaits)
                    add(types=types, coros=coros, awaits=awaits, kinds='none')
        for coros in (1, 4, 16):
            for awaits in (1, 4, 8, 32):
                add(coros=coros, awaits=awaits, shared=0)
        for kinds in KINDS:
            add(kinds=kinds)
            add(kinds=kinds, types=4)
        add(stop=1)
        add(stop=1, types=4)
    return list(out.values())


def source(cfg, types, coros, awaits, kinds, shared, stop):
    errs, cfg_def = CFGS[cfg]
    kind_list = KINDS[kinds]
    L = []
    w = L.append
    w(f'// task probe: {key_of(cfg=cfg, types=types, coros=coros, awaits=awaits, kinds=kinds, shared=shared, stop=stop)}')
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
    w('extern bool      failed();')
    w('extern void      trace_sink( uint32_t );')
    if stop:
        w('extern inplace_stop_token the_token();')
    w('')
    w(cfg_def)
    w('')
    # sources
    def src_decls(k):
        """Declarations of the sources coroutine k awaits: shared ones are declared once."""
        tag = f'p{k}' if not shared else None
        if tag:
            w(f'struct {tag}')
            w('{')
            w('};')
        pay = tag or 'unit'
        suffix = str(k) if tag else ''
        if 'll' in kind_list:
            w(f'using ll{suffix}_t = ll_source< {pay}, set_value_t() >;')
            w(f'extern ll{suffix}_t& src_ll{suffix}();')
        if 'll32' in kind_list:
            w(f'using ll32{suffix}_t = ll_source< {tag or "uint32_t"}, set_value_t( uint32_t ) >;')
            w(f'extern ll32{suffix}_t& src_ll32{suffix}();')
        if 'seq' in kind_list:
            w(f'using seq{suffix}_t = seq_source< uint32_t, {pay}, set_value_t() >;')
            w(f'extern seq{suffix}_t& src_seq{suffix}();')

    if shared:
        src_decls(0)
    else:
        for k in range(coros):
            src_decls(k)
    w('')
    # receivers
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
    w('}  // namespace')
    w('')
    for spec in ('template < typename T >\nstruct dr : env_holder', 'template <>\nstruct dr< void > : env_holder'):
        w(spec)
        w('{')
        w('        using receiver_concept = receiver_t;')
        w('        void set_value( T ) noexcept;' if 'typename T' in spec else '        void set_value() noexcept;')
        for e in errs:
            w(f'        void set_error( {e} ) noexcept;')
        w('        void set_stopped() noexcept;')
        w('};')
    w('')
    w('namespace')
    w('{')
    if 'child' in kind_list:
        w('task< uint8_t, cfg_t > child( task_ctx& )')
        w('{')
        w('        co_return 1;')
        w('}')
        w('')
    tok = 'the_token()' if stop else ''
    for k in range(coros):
        t = k % types
        suffix = '' if shared else str(k)
        w(f'task< {VT[t]}, cfg_t > t{k}( task_ctx& ctx )')
        w('{')
        w('        (void) ctx;')
        for r in range(awaits):
            if r:
                w(f'        sink_i( {r} );')
            for kind in kind_list:
                if kind == 'll':
                    w(f'        co_await src_ll{suffix}().schedule();')
                elif kind == 'll32':
                    arg = '0u' if shared else f'p{k}{{}}'
                    w(f'        sink_i( int( co_await src_ll32{suffix}().schedule( {arg} ) ) );')
                elif kind == 'seq':
                    w(f'        co_await src_seq{suffix}().schedule( now() );')
                elif kind == 'child':
                    w('        sink_i( co_await child( ctx ) );')
                elif kind == 'suspend':
                    w('        co_await suspend;')
                elif kind == 'stop':
                    w('        co_await wait_until_stopped;')
        if 'yield' in kind_list:
            w('        if ( failed() )')
            w('                co_yield with_error( task_error::task_unfinished );')
        w('        ' + RET[t])
        w('}')
        w('')
    w('}  // namespace')
    w('')
    w('void probe()')
    w('{')
    w('        auto& ctx = the_ctx();')
    for k in range(coros):
        t = k % types
        w(f'        {{ auto o = t{k}( ctx ).connect( dr< {VT[t]} >{{ {tok} }} ); o.start(); sink( &o ); }}')
    w('        ctx.core.run_n( 64 );')
    w('}')
    return '\n'.join(L) + '\n'


def report(results, headers, w):
    def get(h='cur', **over):
        return flash_of(results, key_of(**dict(BASE, **over)), h)

    def with_ref(v, base=None, **over):
        c = cell(v, base)
        for h in headers[1:]:
            o = get(h, **over)
            if o is not None and v is not None:
                c += f' [{h}: {v - o:+d}]'
        return c

    w('### task configurations: four coroutines, four rounds of ll, ll32, seq and a child task')
    w('')
    w('Cells: total, then the delta against the default configuration with the same value'
      ' types; a bracket is the delta of `cur` against another header variant.')
    w('')
    w('| cfg | 1 value type | 4 value types |')
    w('|---|---:|---:|')
    for cfg in CFGS:
        cells = []
        for types in (1, 4):
            v = get(cfg=cfg, types=types)
            base = get(cfg='default', types=types) if cfg != 'default' else None
            cells.append(with_ref(v, base, cfg=cfg, types=types))
        if any(c != '-' for c in cells):
            w(f'| {cfg} | ' + ' | '.join(cells) + ' |')
    w('')
    w('### task scaling: default configuration, rounds of ll, ll32, seq and a child task')
    w('')
    w('"none" is the same coroutines without awaits, so a row\'s difference to it is what its'
      ' awaits cost. A round is one await of each of the four kinds; per round is'
      ' (32 rounds − 1 round) / 31.')
    w('')
    w('| value types | coroutines | none | 1 round | 4 | 8 | 32 | per round |')
    w('|---:|---:|---:|---:|---:|---:|---:|---:|')
    for types in (1, 4, 8):
        for coros in (1, 4, 16):
            a = {n: get(types=types, coros=coros, awaits=n) for n in (1, 4, 8, 32)}
            none = get(types=types, coros=coros, awaits=1, kinds='none')
            if all(v is None for v in a.values()) and none is None:
                continue
            slope = '-' if a[32] is None or a[1] is None else f'{(a[32] - a[1]) / 31:.1f}'
            w(f'| {types} | {coros} | {cell(none)} | {cell(a[1])} | {cell(a[4])} | {cell(a[8])} |'
              f' {cell(a[32])} | {slope} |')
    w('')
    w('### awaitables shared or private: one value type, default configuration')
    w('')
    w('Private: every coroutine awaits source types of its own. Cells: private total, then its'
      ' delta against the shared probe.')
    w('')
    w('| coroutines | 1 round | 4 | 8 | 32 |')
    w('|---:|---:|---:|---:|---:|')
    for coros in (1, 4, 16):
        cells = []
        for awaits in (1, 4, 8, 32):
            p = get(coros=coros, awaits=awaits, shared=0)
            s = get(coros=coros, awaits=awaits)
            cells.append(cell(p, s))
        if any(c != '-' for c in cells):
            w(f'| {coros} | ' + ' | '.join(cells) + ' |')
    w('')
    w('### what is awaited: four coroutines, four rounds, default configuration')
    w('')
    w('| kinds | 1 value type | 4 value types |')
    w('|---|---:|---:|')
    for kinds in KINDS:
        cells = []
        for types in (1, 4):
            v = get(kinds=kinds, types=types)
            base = get(kinds='none', types=types) if kinds != 'none' else None
            cells.append(with_ref(v, base, kinds=kinds, types=types))
        if any(c != '-' for c in cells):
            w(f'| {kinds} | ' + ' | '.join(cells) + ' |')
    w('')
    w('### root receivers with a stop token: four coroutines, four rounds, default configuration')
    w('')
    w('| value types | empty environment | stop token |')
    w('|---:|---:|---:|')
    for types in (1, 4):
        v0, v1 = get(types=types), get(types=types, stop=1)
        if v0 is not None or v1 is not None:
            w(f'| {types} | {cell(v0)} | {cell(v1, v0)} |')
    w('')
