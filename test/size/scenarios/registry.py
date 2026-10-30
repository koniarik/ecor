"""The scenario families of the size suite, in report order. A family module has NAME,
probes(tier) and report(results, headers, w); see common.py for the contract. `scenarios` is a
namespace package: the repository ignores `_*` paths, which would swallow an `__init__.py`."""
from . import arena, holder, image, pump, queue, ram, source, task

FAMILIES = [queue, pump, task, source, holder, arena, ram, image]
