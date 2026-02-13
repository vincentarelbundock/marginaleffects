from importlib import import_module
import importlib.util
import sys


_EXPORTS = {
    "avg_comparisons": ("marginaleffects.comparisons", "avg_comparisons"),
    "comparisons": ("marginaleffects.comparisons", "comparisons"),
    "datagrid": ("marginaleffects.datagrid", "datagrid"),
    "hypotheses": ("marginaleffects.hypotheses", "hypotheses"),
    "plot_comparisons": ("marginaleffects.plot.comparisons", "plot_comparisons"),
    "plot_predictions": ("marginaleffects.plot.predictions", "plot_predictions"),
    "plot_slopes": ("marginaleffects.plot.slopes", "plot_slopes"),
    "avg_predictions": ("marginaleffects.predictions", "avg_predictions"),
    "predictions": ("marginaleffects.predictions", "predictions"),
    "avg_slopes": ("marginaleffects.slopes", "avg_slopes"),
    "slopes": ("marginaleffects.slopes", "slopes"),
    "fit_statsmodels": ("marginaleffects.statsmodels.model", "fit_statsmodels"),
    "fit_sklearn": ("marginaleffects.sklearn.model", "fit_sklearn"),
    "fit_linearmodels": ("marginaleffects.linearmodels.model", "fit_linearmodels"),
    "get_dataset": ("marginaleffects.utils", "get_dataset"),
    "MarginaleffectsResult": ("marginaleffects.result", "MarginaleffectsResult"),
    "autodiff": ("marginaleffects.settings", "autodiff"),
    "set_autodiff": ("marginaleffects.settings", "set_autodiff"),
    "get_autodiff": ("marginaleffects.settings", "get_autodiff"),
}

if importlib.util.find_spec("jax") is not None:
    _EXPORTS["autodiff_module"] = ("marginaleffects.autodiff", None)

__all__ = list(_EXPORTS.keys())


def __getattr__(name):
    if name not in _EXPORTS:
        raise AttributeError(f"module 'marginaleffects' has no attribute '{name}'")

    module_name, attr_name = _EXPORTS[name]
    module = import_module(module_name)

    # Rebind exported callables from all loaded modules so importlib-created
    # submodule attributes (e.g. `marginaleffects.comparisons`) do not shadow
    # the intended public API during `from marginaleffects import *`.
    for export_name, (export_module, export_attr) in _EXPORTS.items():
        if export_attr is None:
            continue

        export_mod = sys.modules.get(export_module)
        if export_mod is not None:
            globals()[export_name] = getattr(export_mod, export_attr)

    value = module if attr_name is None else globals()[name]
    globals()[name] = value
    return value


def __dir__():
    return sorted(set(globals().keys()) | set(__all__))
