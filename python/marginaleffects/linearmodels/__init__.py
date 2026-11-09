from .._lazy import lazy_exports

_EXPORTS = {
    "ModelLinearmodels": ("marginaleffects.linearmodels.model", "ModelLinearmodels"),
    "fit_linearmodels": ("marginaleffects.linearmodels.model", "fit_linearmodels"),
    "parse_linearmodels_formula": (
        "marginaleffects.linearmodels.model",
        "parse_linearmodels_formula",
    ),
}

__all__ = list(_EXPORTS.keys())


__getattr__, __dir__ = lazy_exports(__name__, _EXPORTS)
