from importlib import import_module


_EXPORTS = {
    "plot_predictions": ("marginaleffects.plot.predictions", "plot_predictions"),
    "plot_comparisons": ("marginaleffects.plot.comparisons", "plot_comparisons"),
    "plot_slopes": ("marginaleffects.plot.slopes", "plot_slopes"),
}

__all__ = list(_EXPORTS.keys())


def __getattr__(name):
    if name not in _EXPORTS:
        raise AttributeError(f"module 'marginaleffects.plot' has no attribute '{name}'")

    module_name, attr_name = _EXPORTS[name]
    module = import_module(module_name)
    value = getattr(module, attr_name)
    globals()[name] = value
    return value


def __dir__():
    return sorted(set(globals().keys()) | set(__all__))
