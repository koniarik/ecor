# Size suite

Flash cost of ecor's abstractions on Cortex-M4, measured from generated probes compiled with
`arm-none-eabi-g++ -Os`. Needs the Arm GNU toolchain, CMake 3.25 and Ninja on the path.

```sh
python3 test/size/size.py                      # quick tier, report in test/size/build/report.md
python3 test/size/size.py --full               # every probe
python3 test/size/size.py --against HEAD       # the working tree's header against a commit's
python3 test/size/size.py update-baseline --full
python3 test/size/size.py explain queue/pop=parking,push=try_only,close=0,call=none,stop=1,types=4,queues=1,owning=0 --against HEAD
```

A probe's figure is the sum of its `.text*` and `.rodata*` sections from `size -A`; `.data*`
plus `.bss*` is recorded beside it. `size.py --help` lists the options.

## Families

Each module in `scenarios/` is a family with its dimensions in its docstring:

| family | sweeps |
|---|---|
| `queue` | every `async_queue` configuration; element types, owning types, queues on one memory |
| `pump` | pumps per program, stoppability, handler kinds, element types, queue flags |
| `task` | task configurations, value types, coroutines, awaits per coroutine, what is awaited |
| `source` | `ll_source` and `seq_source` count, stop polling, draining, awaiting |
| `holder` | holders per program, factory type, distinct coroutine bodies, stopping |
| `arena` | arenas, distinct memory resource types, object types |
| `ram` | `sizeof` of every abstraction and operation state, tolerance zero |
| `image` | linked programs of coroutines, holders, queues, pumps and sources, with and without LTO |

`--family NAME` runs one of them.

## Baselines and CI

`baseline/<toolchain>.json` holds the full tier's figures for one toolchain version, named by
`arm-none-eabi-g++ -dumpfullversion`. A run compares against the file for the toolchain in use
and exits with 1 when a probe grew by more than the tolerance, 1% with a floor of 8 bytes, or
zero for the `ram` family. Toolchains without a file report only. `update-baseline` rewrites
the file from the current run; commit it with the change that moved the numbers.

`--against REF --fail-on-growth` judges the working tree's header against REF's with the same
tolerance, both built in the same run, so it needs no baseline for the toolchain at hand. The
`size` workflow runs it on every push and pull request against the commit before, with the
Arm GNU toolchain release it downloads, and uploads the report as an artifact.

## Reading a report

Each family prints its own tables, then the probes above the baseline tolerance, then every
probe with the figure of each header variant. Deltas in parentheses are against a family's
reference row, such as a default configuration; deltas in brackets are the working tree's
header against `--against` or `--variant`.

For the cause of a difference, `explain PROBE --against REF` prints the sections that changed,
demangled, and `--extra-flags -fopt-info-inline-all` puts GCC's inlining decisions in the build
log, `test/size/build/.ninja_log` holding the commands.

## Adding probes

A family is a module in `scenarios/` listed in `scenarios/registry.py`, with `NAME`,
`probes(tier)` and `report(results, headers, w)`; `scenarios/common.py` states the
contract and holds the shared snippets. A probe is one translation unit returned as text by its
`source()`, keyed by its dimensions as `name=value,...` so the report and the baseline can find
it. The rules that keep a figure meaningful:

- Declare externals, define nothing but `probe()`: `sink( void const* )`, `the_mem()` and the
  like are declared and never defined, so the object holds only the library's code and the glue.
- Construct every type and start every operation. A type that is merely referenced leaves its
  constructor, vtable and virtuals out of the object; so does an operation state never started.
- Give receivers the environment the comparison is about. A stop token in the receiver pulls
  the cancellation path into every operation state it reaches.
- Sweep the dimension the question is about, and more than one point of it: per-instantiation
  costs are flat across a sweep, per-type costs grow with it, and a single point cannot tell
  them apart.
