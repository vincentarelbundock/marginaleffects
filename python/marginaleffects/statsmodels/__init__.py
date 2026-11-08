from .._lazy import lazy_exports

_EXPORTS = {
    "ModelStatsmodels": ("marginaleffects.statsmodels.model", "ModelStatsmodels"),
    "fit_statsmodels": ("marginaleffects.statsmodels.model", "fit_statsmodels"),
}

__all__ = list(_EXPORTS.keys())


__getattr__, __dir__ = lazy_exports(__name__, _EXPORTS)
