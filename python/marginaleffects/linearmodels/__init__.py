from importlib import import_module


_EXPORTS = {
    "ModelLinearmodels": ("marginaleffects.linearmodels.model", "ModelLinearmodels"),
    "fit_linearmodels": ("marginaleffects.linearmodels.model", "fit_linearmodels"),
    "parse_linearmodels_formula": (
        "marginaleffects.linearmodels.model",
        "parse_linearmodels_formula",
    ),
}

__all__ = list(_EXPORTS.keys())


def __getattr__(name):
    if name not in _EXPORTS:
        raise AttributeError(
            f"module 'marginaleffects.linearmodels' has no attribute '{name}'"
        )

    module_name, attr_name = _EXPORTS[name]
    module = import_module(module_name)
    value = getattr(module, attr_name)
    globals()[name] = value
    return value


def __dir__():
    return sorted(set(globals().keys()) | set(__all__))
