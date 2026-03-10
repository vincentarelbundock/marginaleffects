from importlib import import_module


_EXPORTS = {
    "ModelStatsmodels": ("marginaleffects.statsmodels.model", "ModelStatsmodels"),
    "fit_statsmodels": ("marginaleffects.statsmodels.model", "fit_statsmodels"),
}

__all__ = list(_EXPORTS.keys())


def __getattr__(name):
    if name not in _EXPORTS:
        raise AttributeError(
            f"module 'marginaleffects.statsmodels' has no attribute '{name}'"
        )

    module_name, attr_name = _EXPORTS[name]
    module = import_module(module_name)
    value = getattr(module, attr_name)
    globals()[name] = value
    return value


def __dir__():
    return sorted(set(globals().keys()) | set(__all__))
