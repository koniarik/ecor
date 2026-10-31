"""The scenario families of the size suite, in report order. A family module has NAME,
probes(tier) and report(results, headers, w); see common.py for the contract. `scenarios` is a
namespace package: the repository ignores `_*` paths, which would swallow an `__init__.py`."""
from . import (arena, awaited, co_await, coroutine, holder, program, pump, queue, ram, source,
               task)

FAMILIES = [queue, pump, task, coroutine, awaited, co_await, source, holder, arena, ram, program]
