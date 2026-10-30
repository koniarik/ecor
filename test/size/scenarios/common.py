"""Shared pieces of the scenario families.

A family module exposes:

  NAME                       the family's name, a path component of every probe label
  probes(tier)               a list of Probe for 'quick' or 'full'; quick is a subset of full
  report(results, headers, w)
                             appends the family's tables through w(line). results maps
                             (header, key) to a Measurement; headers lists the header variant
                             names, 'cur' first.
  TOLERANCE                  optional (percent, floor in bytes) for the baseline comparison

A probe's `source()` returns the text of one translation unit. The unit declares every external
it needs (`sink`, `the_mem`, ...) and defines nothing with external linkage but `probe()`, so the
object holds the library's code and the probe's glue and nothing else. Every type under
measurement is constructed and every operation started: a type that is only referenced leaves
its constructor, its vtable and its virtuals out of the object.

An image probe has `sources()` instead, a list of (file name, text) linked into one executable
entered at `_start` with the library's startup code left out; its figure is the linked
program's.
"""
from dataclasses import dataclass, field
from typing import Callable, NamedTuple

CPU = 'cortex-m4'

TIERS = ('quick', 'full')


class Measurement(NamedTuple):
    flash: int   # .text + .rodata
    ram: int     # .data + .bss
    path: str    # the object or the ELF, for attribution tools


@dataclass
class Probe:
    family: str
    key: str
    source: Callable[[], str] = None
    defines: dict = field(default_factory=dict)
    sources: Callable[[], list] = None   # image probes: [(file name, text), ...]
    lto: bool = False

    @property
    def label(self):
        return f'{self.family}/{self.key}'

    @property
    def is_image(self):
        return self.sources is not None


def flash_of(results, key, header='cur'):
    m = results.get((header, key))
    return None if m is None else m.flash


def key_of(**kw):
    """`a=1,b=x`: the probe key, from the dimensions in the order given."""
    return ','.join(f'{k}={v}' for k, v in kw.items())


def params_of(key):
    """The inverse of key_of, values as strings."""
    return dict(p.split('=', 1) for p in key.split(','))


def delta(v, base):
    return '-' if v is None or base is None else f'{v - base:+d}'


def cell(v, base=None):
    """`1234 (+56)`: a total with its delta against a base when there is one."""
    if v is None:
        return '-'
    return str(v) if base is None else f'{v} ({v - base:+d})'


SYS_MEM = '''struct sys_mem
{
        void* allocate( std::size_t, std::size_t ) noexcept;
        void  deallocate( void*, std::size_t, std::size_t ) noexcept;
};
'''

# A move-only owner of a block, which makes an element type not trivially copyable.
BLK = '''struct blk
{
        void* p = nullptr;

        blk() = default;
        blk( blk&& o ) noexcept
          : p( o.p )
        {
                o.p = nullptr;
        }
        blk& operator=( blk&& o ) noexcept
        {
                void* t = p;
                p       = o.p;
                o.p     = t;
                return *this;
        }
        ~blk()
        {
                if ( p )
                        release( p );
        }
};
'''

# A receiver for a task< void >: every completion a task has.
TASK_RECV = '''struct dr
{
        using receiver_concept = receiver_t;
        void set_value() noexcept;
        void set_error( task_error ) noexcept;
        void set_stopped() noexcept;
        [[nodiscard]] empty_env get_env() const noexcept
        {
                return {};
        }
};
'''

# A sender that completes at start(); `Sigs` its completion signatures, `Reply` whether the
# value row carries a uint16_t.
IMM_SENDER = '''template < typename Sigs, bool Reply >
struct imm
{
        using sender_concept        = sender_t;
        using completion_signatures = Sigs;
        uint16_t v;

        template < receiver R >
        struct op
        {
                using operation_state_concept = operation_state_t;
                R        r;
                uint16_t v;
                void     start()
                {
                        if constexpr ( Reply )
                                r.set_value( v );
                        else
                                r.set_value();
                }
        };

        template < receiver R >
        op< R > connect( R r )
        {
                return { std::move( r ), v };
        }
};
'''
