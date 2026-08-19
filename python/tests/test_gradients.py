import numpy as np
import polars as pl
import pytest

from marginaleffects.estimands import estimands
from marginaleffects.inference.gradients import comparison_gradient_exact


@pytest.mark.parametrize(
    "key",
    [
        "difference",
        "differenceavg",
        "differenceavgwts",
        "ratio",
        "ratioavg",
        "ratioavgwts",
        "lnratio",
        "lnratioavg",
        "lnratioavgwts",
        "lnor",
        "lnoravg",
        "lnoravgwts",
        "lift",
        "liftavg",
        "dydx",
        "dydxavg",
        "dydxavgwts",
        "dyex",
        "dyexavg",
        "dyexavgwts",
        "expdydx",
        "expdydxavg",
        "expdydxavgwts",
    ],
)
def test_exact_comparison_gradients(key):
    hi = np.asarray([0.35, 0.55, 0.7])
    lo = np.asarray([0.25, 0.4, 0.6])
    x = np.asarray([1.0, 2.0, 3.0])
    w = np.asarray([1.0, 2.0, 4.0]) if key.endswith("wts") else None
    eps = 0.1
    exact = comparison_gradient_exact(key, hi, lo, eps=eps, x=x, w=w)
    assert exact is not None

    def evaluate(a, b):
        if key == "expdydxavg":
            return np.atleast_1d(np.mean((np.exp(a) - np.exp(b)) / np.exp(eps) / eps))
        a = pl.Series(a)
        b = pl.Series(b)
        x_arg = pl.Series(x)
        w_arg = None if w is None else pl.Series(w)
        return np.asarray(
            estimands[key](hi=a, lo=b, eps=eps, x=x_arg, y=None, w=w_arg),
            dtype=float,
        ).reshape(-1)

    step = 1e-6
    base = evaluate(hi, lo)
    numeric_hi = np.column_stack(
        [
            (evaluate(hi + np.eye(len(hi))[j] * step, lo) - base) / step
            for j in range(len(hi))
        ]
    )
    numeric_lo = np.column_stack(
        [
            (evaluate(hi, lo + np.eye(len(lo))[j] * step) - base) / step
            for j in range(len(lo))
        ]
    )
    grad_hi, grad_lo = exact
    expected_hi = np.diag(grad_hi) if base.size > 1 else grad_hi[None, :]
    expected_lo = np.diag(grad_lo) if base.size > 1 else grad_lo[None, :]
    np.testing.assert_allclose(expected_hi, numeric_hi, rtol=2e-5, atol=2e-5)
    np.testing.assert_allclose(expected_lo, numeric_lo, rtol=2e-5, atol=2e-5)
