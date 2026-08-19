"""Exact derivatives of recorded built-in comparison functions."""

import numpy as np

EXACT_COMPARISON_KEYS = {
    "difference",
    "differenceavg",
    "differenceavgwts",
    "ratio",
    "ratioavg",
    "ratioavgwts",
    "lift",
    "liftavg",
    "liftavgwts",
    "lnratio",
    "lnratioavg",
    "lnratioavgwts",
    "lnor",
    "lnoravg",
    "lnoravgwts",
    "dydx",
    "dydxavg",
    "dydxavgwts",
    "dyex",
    "dyexavg",
    "dyexavgwts",
    "expdydx",
    "expdydxavg",
    "expdydxavgwts",
}


def comparison_gradient_exact(fun_key, hi, lo, *, eps=None, x=None, w=None):
    n = len(hi)
    if w is None:
        a = np.full(n, 1.0 / n)
    else:
        w = np.asarray(w, dtype=float)
        if w.shape != (n,) or not np.isfinite(w).all() or w.sum() == 0:
            return None
        a = w / w.sum()

    def mean(z):
        return float(a @ z)

    if fun_key.startswith(("dydx", "dyex", "expdydx")):
        eps = np.asarray(eps, dtype=float)
        if eps.size != 1 or not np.isfinite(eps).all() or eps.item() == 0:
            return None
        eps = eps.item()

    if fun_key == "difference":
        return np.ones(n), -np.ones(n)
    if fun_key in {"differenceavg", "differenceavgwts"}:
        return a, -a
    if fun_key in {"ratio", "lift"}:
        return 1 / lo, -hi / lo**2
    if fun_key in {"ratioavg", "ratioavgwts", "liftavg", "liftavgwts"}:
        mh, ml = mean(hi), mean(lo)
        return a / ml, -a * mh / ml**2
    if fun_key == "lnratio":
        return 1 / hi, -1 / lo
    if fun_key in {"lnratioavg", "lnratioavgwts"}:
        mh, ml = mean(hi), mean(lo)
        return a / mh, -a / ml
    if fun_key == "lnor":
        return 1 / (hi * (1 - hi)), -1 / (lo * (1 - lo))
    if fun_key in {"lnoravg", "lnoravgwts"}:
        mh, ml = mean(hi), mean(lo)
        return a / (mh * (1 - mh)), -a / (ml * (1 - ml))
    if fun_key == "dydx":
        return np.full(n, 1 / eps), np.full(n, -1 / eps)
    if fun_key in {"dydxavg", "dydxavgwts"}:
        return a / eps, -a / eps
    if fun_key == "dyex" and x is not None:
        x = np.asarray(x, dtype=float)
        return x / eps, -x / eps
    if fun_key in {"dyexavg", "dyexavgwts"} and x is not None:
        x = np.asarray(x, dtype=float)
        return a * x / eps, -a * x / eps
    if fun_key == "expdydx":
        scale = np.exp(eps) * eps
        return np.exp(hi) / scale, -np.exp(lo) / scale
    if fun_key in {"expdydxavg", "expdydxavgwts"}:
        scale = np.exp(eps) * eps
        return a * np.exp(hi) / scale, -a * np.exp(lo) / scale
    return None
