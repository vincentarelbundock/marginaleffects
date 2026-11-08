from .._lazy import lazy_exports

_EXPORTS = {
    "plot_predictions": ("marginaleffects.plot.predictions", "plot_predictions"),
    "plot_comparisons": ("marginaleffects.plot.comparisons", "plot_comparisons"),
    "plot_slopes": ("marginaleffects.plot.slopes", "plot_slopes"),
}

__all__ = list(_EXPORTS.keys())


__getattr__, __dir__ = lazy_exports(__name__, _EXPORTS)
