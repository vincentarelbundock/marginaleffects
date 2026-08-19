"""Import-time invariants for the lazily-exported packages.

A submodule whose name matches one of its package's lazy exports is a trap.
Importing the submodule binds the *module* onto the package, `__getattr__`
never fires for that name again, and every later `from .pkg import <name>`
silently receives a module. `marginaleffects.sanitize` hit exactly that:
`datagrid()` imported `.sanitize.sanitize_model` directly, so the later
`from .sanitize import sanitize_model` in `prepare_base_inputs()` returned a
module and `predictions(model, newdata=datagrid(model=model))` died with
"'module' object is not callable".
"""

import importlib
import pkgutil
import subprocess
import sys


def test_sanitize_submodules_do_not_shadow_exports():
    package = importlib.import_module("marginaleffects.sanitize")
    exports = set(getattr(package, "_EXPORTS", {}))
    submodules = {name for _, name, _ in pkgutil.iter_modules(package.__path__)}
    assert exports, "the sanitize package must declare lazy exports"
    assert not (exports & submodules), (
        "these submodule names shadow lazy exports of the same name: "
        f"{sorted(exports & submodules)}. Name submodules after the noun they "
        "sanitize (`model`, not `sanitize_model`)."
    )


def test_datagrid_then_predictions_in_a_fresh_interpreter():
    """The original failure, end to end, with import order preserved."""
    script = """
import statsmodels.formula.api as smf
from marginaleffects import datagrid, predictions, get_dataset

mtcars = get_dataset("mtcars", "datasets").to_pandas()
model = smf.ols("mpg ~ hp + wt", data=mtcars).fit()
grid = datagrid(model=model)
out = predictions(model, newdata=grid)
assert out.data.height == 1

from marginaleffects.sanitize import sanitize_model
assert callable(sanitize_model), type(sanitize_model)
print("ok")
"""
    completed = subprocess.run(
        [sys.executable, "-c", script],
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0, completed.stderr
    assert "ok" in completed.stdout
