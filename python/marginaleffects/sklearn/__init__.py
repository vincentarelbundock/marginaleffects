from .._lazy import lazy_exports

_EXPORTS = {
    "ModelSklearn": ("marginaleffects.sklearn.model", "ModelSklearn"),
    "fit_sklearn": ("marginaleffects.sklearn.model", "fit_sklearn"),
}

__all__ = list(_EXPORTS.keys())


__getattr__, __dir__ = lazy_exports(__name__, _EXPORTS)
