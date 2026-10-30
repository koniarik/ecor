"""RAM: `sizeof` of every abstraction and operation state, as the probe's figure.

A probe is one translation unit whose only definition is a `.rodata` array of `sizeof( T )`
bytes, so the suite's flash figure of the probe is `sizeof( T )` and the baseline comparison
covers object sizes with a tolerance of zero. Types that a program never names, such as
operation states and awaitables, are spelt through `connect_type` and `await_transform`.
"""
from .common import IMM_SENDER, Probe, SYS_MEM, cell, flash_of

NAME = 'ram'
TOLERANCE = (0, 0)

PRELUDE = '''#include <ecor/ecor.hpp>

#include <cstdint>
#include <utility>

using namespace ecor;

''' + SYS_MEM + '''
struct e0
{
        uint8_t v;
};
struct e1
{
        uint32_t v;
};
struct e2
{
        uint32_t a, b;
};
struct e3
{
        uint8_t d[16];
};

struct dr
{
        using receiver_concept = receiver_t;
        void set_value() noexcept;
        void set_error( task_error ) noexcept;
        void set_stopped() noexcept;
        [[nodiscard]] empty_env get_env() const noexcept;
};

struct sig_recv
{
        using receiver_concept = receiver_t;
        inplace_stop_token token;
        void               set_value() noexcept;
        void               set_stopped() noexcept;
        [[nodiscard]] stop_token_env< inplace_stop_token > get_env() const noexcept;
};

struct pop_recv
{
        using receiver_concept = receiver_t;
        inplace_stop_token token;
        void               set_value( e0& ) noexcept;
        void               set_value( e1& ) noexcept;
        void               set_value( e2& ) noexcept;
        void               set_value( e3& ) noexcept;
        void               set_stopped() noexcept;
        [[nodiscard]] stop_token_env< inplace_stop_token > get_env() const noexcept;
};

template < async_queue_api Pop, async_queue_api Push, bool Close >
struct qcfg : queue_default_cfg< sys_mem >
{
        static constexpr async_queue_api pop_api      = Pop;
        static constexpr async_queue_api push_api     = Push;
        static constexpr bool            is_closeable = Close;
};
struct call_cfg : queue_default_cfg< sys_mem >
{
        static constexpr async_queue_api pop_api = async_queue_api::parking;
        using push_and_wait_signatures =
            completion_signatures< set_value_t(), set_error_t( task_error ), set_stopped_t() >;
};
template < typename CFG >
using q_t = async_queue< CFG, e0, e1, e2, e3 >;
using api                = async_queue_api;
using q_default          = q_t< qcfg< api::try_only, api::try_only, false > >;
using q_closeable        = q_t< qcfg< api::try_only, api::try_only, true > >;
using q_pop_parking      = q_t< qcfg< api::parking, api::try_only, false > >;
using q_pop_cancellable  = q_t< qcfg< api::cancellable, api::try_only, false > >;
using q_both_parking     = q_t< qcfg< api::parking, api::parking, false > >;
using q_both_cancellable = q_t< qcfg< api::cancellable, api::cancellable, false > >;
using q_all              = q_t< qcfg< api::cancellable, api::cancellable, true > >;
using q_call             = q_t< call_cfg >;

using src_t = ll_source< unit, set_value_t() >;
using seq_t = seq_source< uint32_t, unit, set_value_t() >;

''' + IMM_SENDER + '''
using imm_t = imm< completion_signatures< set_value_t(), set_stopped_t() >, false >;
struct h
{
        imm_t handle( task_ctx&, e0& );
        imm_t handle( task_ctx&, e1& );
        imm_t handle( task_ctx&, e2& );
        imm_t handle( task_ctx&, e3& );
};
struct pcfg_stop : pump_default_cfg< task_ctx, imm_t >
{
        static constexpr bool is_stoppable = true;
};
using factory_t = task< void > ( * )( task_ctx& );
struct obj
{
        src_t& s;
        auto   async_destroy()
        {
                return s.schedule();
        }
};
using promise_v = task< void >::promise_type;
'''

TYPES = [
    ('task_core', 'task_core'),
    ('task_ctx', 'task_ctx'),
    ('task_memory_resource', 'task_memory_resource'),
    ('inplace_stop_source', 'inplace_stop_source'),
    ('inplace_stop_token', 'inplace_stop_token'),
    ('inplace_stop_callback', 'inplace_stop_callback< void ( * )() >'),
    ('task_void', 'task< void >'),
    ('promise_void', 'promise_v'),
    ('promise_int', 'task< int >::promise_type'),
    ('task_op', 'connect_type< task< void >, dr >'),
    ('await_ll', 'decltype( std::declval< promise_v& >().await_transform('
                 ' std::declval< src_t& >().schedule() ) )'),
    ('await_seq', 'decltype( std::declval< promise_v& >().await_transform('
                  ' std::declval< seq_t& >().schedule( 0u ) ) )'),
    ('await_task', 'decltype( std::declval< promise_v& >().await_transform('
                   ' std::declval< task< uint8_t > >() ) )'),
    ('ll_source', 'src_t'),
    ('ll_op', 'connect_type< decltype( std::declval< src_t& >().schedule() ), sig_recv >'),
    ('seq_source', 'seq_t'),
    ('seq_op', 'connect_type< decltype( std::declval< seq_t& >().schedule( 0u ) ), sig_recv >'),
    ('queue_default', 'q_default'),
    ('queue_closeable', 'q_closeable'),
    ('queue_pop_parking', 'q_pop_parking'),
    ('queue_pop_cancellable', 'q_pop_cancellable'),
    ('queue_both_parking', 'q_both_parking'),
    ('queue_both_cancellable', 'q_both_cancellable'),
    ('queue_all', 'q_all'),
    ('queue_call', 'q_call'),
    ('pop_op_parking', 'connect_type< decltype( std::declval< q_both_parking& >().pop() ), pop_recv >'),
    ('pop_op_cancellable',
     'connect_type< decltype( std::declval< q_both_cancellable& >().pop() ), pop_recv >'),
    ('push_op_parking',
     'connect_type< decltype( std::declval< q_both_parking& >().push( e0{} ) ), sig_recv >'),
    ('push_op_cancellable',
     'connect_type< decltype( std::declval< q_both_cancellable& >().push( e0{} ) ), sig_recv >'),
    ('close_op', 'connect_type< decltype( std::declval< q_all& >().close() ), sig_recv >'),
    ('call_op', 'connect_type< decltype( std::declval< q_call& >().push_and_wait('
                ' std::declval< e0& >() ) ), dr >'),
    ('pump', 'event_pump< pump_default_cfg< task_ctx, imm_t >, q_default, h >'),
    ('pump_stoppable', 'event_pump< pcfg_stop, q_default, h >'),
    ('task_holder', 'task_holder< task_default_cfg, task_ctx, factory_t >'),
    ('async_arena', 'async_arena< task_ctx, sys_mem >'),
    ('async_ptr', 'async_ptr< obj, task_ctx, sys_mem >'),
    ('circular_buffer_memory_u16', 'circular_buffer_memory< uint16_t >'),
]


def probes(tier):
    return [Probe(NAME, f'type={name}', lambda expr=expr, name=name: source(name, expr))
            for name, expr in TYPES]


def source(name, expr):
    return (f'// sizeof probe: {name}\n' + PRELUDE
            + f'\nextern char const sz[];\nchar const sz[sizeof( {expr} )] = {{ 1 }};\n')


def report(results, headers, w):
    w('### sizeof, bytes on cortex-m4')
    w('')
    w('| type | bytes |')
    w('|---|---:|')
    for name, _ in TYPES:
        v = flash_of(results, f'type={name}')
        if v is None:
            continue
        c = cell(v)
        for h in headers[1:]:
            o = flash_of(results, f'type={name}', h)
            if o is not None:
                c += f' [{h}: {v - o:+d}]'
        w(f'| {name} | {c} |')
    w('')
