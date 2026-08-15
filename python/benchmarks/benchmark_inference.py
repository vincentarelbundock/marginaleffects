"""End-to-end inference benchmark across observations and coefficient counts.

Run with: ``uv run --all-extras python benchmarks/benchmark_inference.py``.
"""

import time

import numpy as np
import polars as pl
import statsmodels.api as sm

from marginaleffects import avg_comparisons, avg_predictions, comparisons, predictions


def make_model(n_obs, n_coef, seed=42):
    rng = np.random.default_rng(seed)
    X = rng.normal(size=(n_obs, n_coef))
    beta = rng.normal(size=n_coef)
    data = pl.DataFrame({f"x{i}": X[:, i] for i in range(n_coef)})
    data = data.with_columns(y=pl.Series(X @ beta + rng.normal(size=n_obs)))
    formula = "y ~ " + " + ".join(f"x{i}" for i in range(n_coef))
    return sm.OLS.from_formula(formula, data.to_pandas()).fit()


def elapsed(call):
    start = time.perf_counter()
    call()
    return time.perf_counter() - start


if __name__ == "__main__":
    cases = (predictions, avg_predictions, comparisons, avg_comparisons)
    for n_obs in (100, 1_000, 10_000):
        for n_coef in (5, 25, 100):
            model = make_model(n_obs, n_coef)
            for func in cases:
                kwargs = {"variables": "x0"} if "comparisons" in func.__name__ else {}
                duration = elapsed(lambda: func(model, **kwargs))
                print(f"{func.__name__:18} n={n_obs:5} p={n_coef:3}: {duration:.4f}s")
