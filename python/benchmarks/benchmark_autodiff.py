"""
Benchmark comparing autodiff (JAX) vs finite differences.

Run with:
    uv run --all-extras python benchmarks/benchmark_autodiff.py
"""

import time

import numpy as np
import polars as pl
import statsmodels.api as sm
from marginaleffects import (
    autodiff,
    avg_comparisons,
    avg_predictions,
    comparisons,
    predictions,
)
from marginaleffects.uncertainty import get_jacobian, get_se


def generate_data(
    n_obs: int = 5000, n_predictors: int = 50, seed: int = 42
) -> pl.DataFrame:
    """Generate simulated data."""
    rng = np.random.default_rng(seed)
    X = rng.normal(size=(n_obs, n_predictors))
    beta = rng.normal(size=n_predictors + 1)
    y = beta[0] + X @ beta[1:] + rng.normal(size=n_obs) * 0.5
    data = {"y": y}
    for i in range(n_predictors):
        data[f"x{i}"] = X[:, i]
    return pl.DataFrame(data)


def _time_call(func, n_runs: int):
    out = []
    for _ in range(n_runs):
        start = time.perf_counter()
        func()
        out.append(time.perf_counter() - start)
    return out


def benchmark_public_api(func, model, n_runs: int = 10, **kwargs):
    """
    Benchmark public API calls with and without autodiff.

    This includes shared overhead such as model sanitization, formula/design
    matrix construction, vcov retrieval, confidence intervals, and result
    formatting.
    """
    results = {"autodiff": [], "finite_diff": []}

    autodiff(True)
    func(model, **kwargs)
    autodiff(False)
    func(model, **kwargs)

    autodiff(True)
    results["autodiff"] = _time_call(lambda: func(model, **kwargs), n_runs=n_runs)

    autodiff(False)
    results["finite_diff"] = _time_call(lambda: func(model, **kwargs), n_runs=n_runs)

    autodiff(None)
    return results


def _engine_cases(X: np.ndarray, beta: np.ndarray):
    from marginaleffects.autodiff import pipeline

    V = np.eye(beta.size)
    n_obs = X.shape[0]
    segments = np.zeros(n_obs, dtype=np.int32)

    X_hi = X.copy()
    X_lo = X.copy()
    X_hi[:, 1] += 0.5

    def jax_predictions():
        result = pipeline.compute(beta=beta, model_type="linear", X=X)
        return get_se(result["jacobian"], V)

    def fd_predictions():
        J = get_jacobian(lambda b: X @ b, beta)
        return get_se(J, V)

    def jax_avg_predictions():
        result = pipeline.compute(
            beta=beta,
            model_type="linear",
            X=X,
            agg_segments=segments,
            agg_num_segments=1,
        )
        return get_se(result["jacobian"], V)

    def fd_avg_predictions():
        J = get_jacobian(lambda b: np.array([(X @ b).mean()]), beta)
        return get_se(J, V)

    def jax_comparisons():
        result = pipeline.compute(
            beta=beta,
            model_type="linear",
            X_hi=X_hi,
            X_lo=X_lo,
            ops=[{"op": "difference", "n": n_obs, "w": None}],
        )
        return get_se(result["jacobian"], V)

    def fd_comparisons():
        J = get_jacobian(lambda b: (X_hi @ b) - (X_lo @ b), beta)
        return get_se(J, V)

    def jax_avg_comparisons():
        result = pipeline.compute(
            beta=beta,
            model_type="linear",
            X_hi=X_hi,
            X_lo=X_lo,
            ops=[{"op": "differenceavg", "n": n_obs, "w": None}],
        )
        return get_se(result["jacobian"], V)

    def fd_avg_comparisons():
        J = get_jacobian(
            lambda b: np.array([np.mean((X_hi @ b) - (X_lo @ b))]),
            beta,
        )
        return get_se(J, V)

    return {
        "OLS predictions()": (jax_predictions, fd_predictions),
        "OLS avg_predictions()": (jax_avg_predictions, fd_avg_predictions),
        "OLS comparisons()": (jax_comparisons, fd_comparisons),
        "OLS avg_comparisons()": (jax_avg_comparisons, fd_avg_comparisons),
    }


def benchmark_engine(X: np.ndarray, beta: np.ndarray, n_runs: int = 20):
    """
    Benchmark only the Jacobian engine on cached design matrices.

    This isolates the finite-difference loop from JAX autodiff, excluding public
    API setup and result formatting.
    """
    out = {}
    for name, (jax_func, fd_func) in _engine_cases(X, beta).items():
        # Warm-up. The JAX call also compiles the traced function.
        jax_func()
        fd_func()
        out[name] = {
            "autodiff": _time_call(jax_func, n_runs=n_runs),
            "finite_diff": _time_call(fd_func, n_runs=n_runs),
        }
    return out


def print_results(name: str, results: dict):
    """Print benchmark results."""
    ad_mean = np.mean(results["autodiff"]) * 1000
    fd_mean = np.mean(results["finite_diff"]) * 1000
    speedup = fd_mean / ad_mean
    print(f"{name:30s}  {ad_mean:6.1f} ms  {fd_mean:6.1f} ms  {speedup:.2f}x")


if __name__ == "__main__":
    try:
        from marginaleffects.autodiff import _JAX_AVAILABLE

        if not _JAX_AVAILABLE:
            print("JAX not available")
            exit(1)
    except ImportError:
        print("autodiff module not found")
        exit(1)

    data = generate_data()
    formula = "y ~ " + " + ".join([f"x{i}" for i in range(50)])
    X = data.select([f"x{i}" for i in range(50)]).to_numpy()
    X = np.column_stack([np.ones(X.shape[0]), X])
    beta = np.random.default_rng(42).normal(size=X.shape[1])

    print("Autodiff Public API Benchmark (5000 obs, 50 predictors)")
    print("Includes model wrapping, formula/design matrices, vcov, and formatting")
    print("=" * 78)
    print(f"{'Function':30s}  {'JAX':>8s}  {'Finite':>8s}  Speedup")
    print("-" * 78)

    ols = sm.OLS.from_formula(formula, data.to_pandas()).fit()
    print_results("OLS predictions()", benchmark_public_api(predictions, ols))
    print_results("OLS avg_predictions()", benchmark_public_api(avg_predictions, ols))
    print_results(
        "OLS comparisons()",
        benchmark_public_api(comparisons, ols, variables="x0"),
    )
    print_results(
        "OLS avg_comparisons()",
        benchmark_public_api(avg_comparisons, ols, variables="x0"),
    )

    glm = sm.GLM.from_formula(
        formula, data.to_pandas(), family=sm.families.Gaussian()
    ).fit()
    print_results("GLM predictions()", benchmark_public_api(predictions, glm))
    print_results("GLM avg_predictions()", benchmark_public_api(avg_predictions, glm))
    print_results(
        "GLM comparisons()",
        benchmark_public_api(comparisons, glm, variables="x0"),
    )
    print_results(
        "GLM avg_comparisons()",
        benchmark_public_api(avg_comparisons, glm, variables="x0"),
    )
    print("=" * 78)

    print()
    print("Autodiff Jacobian Engine Benchmark (cached 5000x51 design matrix)")
    print("Excludes shared public API setup; row-level quantities have many outputs")
    print("=" * 78)
    print(f"{'Function':30s}  {'JAX':>8s}  {'Finite':>8s}  Speedup")
    print("-" * 78)
    for name, results in benchmark_engine(X, beta).items():
        print_results(name, results)
    print("=" * 78)
