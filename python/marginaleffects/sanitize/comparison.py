import inspect

import numpy as np
import polars as pl

from ..estimands import estimands


def _wrap_comparison_function(fn):
    """Wrap a user-supplied comparison function to accept the full (hi, lo, eps, x, y, w) signature."""
    sig = inspect.signature(fn)
    params = list(sig.parameters.keys())
    full_params = ["hi", "lo", "eps", "x", "y", "w"]
    if params == full_params:
        return fn

    def wrapper(hi, lo, eps, x, y, w):
        kwargs = {"hi": hi, "lo": lo, "eps": eps, "x": x, "y": y, "w": w}
        call_kwargs = {p: kwargs[p] for p in params if p in kwargs}
        result = fn(**call_kwargs)
        if isinstance(result, (float, int, np.integer, np.floating)):
            return pl.Series([result])
        elif isinstance(result, np.ndarray):
            return pl.Series(result)
        elif isinstance(result, list):
            return pl.Series(result)
        return result

    return wrapper


def sanitize_comparison(comparison, by, wts=None):
    # Handle callable comparison functions
    if callable(comparison):
        return (_wrap_comparison_function(comparison), "custom")

    out = comparison
    if by is not False:
        if f"{comparison}avg" in estimands.keys():
            out = comparison + "avg"

    if wts is not None:
        if f"{out}wts" in estimands.keys():
            out = out + "wts"

    lab = {
        "difference": "{hi} - {lo}",
        "differenceavg": "{hi} - {lo}",
        "differenceavgwts": "{hi} - {lo}",
        "dydx": "dY/dX",
        "eyex": "eY/eX",
        "eydx": "eY/dX",
        "dyex": "dY/eX",
        "dydxavg": "dY/dX",
        "eyexavg": "eY/eX",
        "eydxavg": "eY/dX",
        "dyexavg": "dY/eX",
        "dydxavgwts": "dY/dX",
        "eyexavgwts": "eY/eX",
        "eydxavgwts": "eY/dX",
        "dyexavgwts": "dY/eX",
        "ratio": "{hi} / {lo}",
        "ratioavg": "{hi} / {lo}",
        "ratioavgwts": "{hi} / {lo}",
        "lnratio": "ln({hi} / {lo})",
        "lnratioavg": "ln({hi} / {lo})",
        "lnratioavgwts": "ln({hi} / {lo})",
        "lnor": "ln(odds({hi}) / odds({lo}))",
        "lnoravg": "ln(odds({hi}) / odds({lo}))",
        "lnoravgwts": "ln(odds({hi}) / odds({lo}))",
        "lift": "lift",
        "liftavg": "liftavg",
        "expdydx": "exp(dY/dX)",
    }

    assert out in lab.keys(), (
        f"`comparison` must be one of: {', '.join(list(lab.keys()))}."
    )

    return (out, lab[out])
