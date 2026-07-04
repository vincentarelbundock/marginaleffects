import importlib.util

import numpy as np
import pytest


pytestmark = pytest.mark.skipif(
    importlib.util.find_spec("jax") is None,
    reason="JAX not available",
)


def finite_difference_jacobian(fun, beta, eps=1e-6):
    beta = np.asarray(beta, dtype=float)
    base = np.asarray(fun(beta), dtype=float)
    out = np.empty((base.size, beta.size))
    for j in range(beta.size):
        step = np.zeros_like(beta)
        step[j] = eps
        hi = np.asarray(fun(beta + step), dtype=float)
        lo = np.asarray(fun(beta - step), dtype=float)
        out[:, j] = (hi - lo) / (2 * eps)
    return out


def test_pipeline_predictions_with_aggregation_and_hypothesis():
    from marginaleffects.autodiff import pipeline
    from marginaleffects.uncertainty import get_se

    beta = np.array([0.2, -0.4, 0.7])
    X = np.array(
        [
            [1.0, 0.0, 1.0],
            [1.0, 1.0, 0.0],
            [1.0, 2.0, 1.0],
            [1.0, 3.0, 0.0],
        ]
    )
    H = np.array([[1.0], [-1.0]])
    segments = np.array([0, 0, 1, 1], dtype=np.int32)
    weights = np.array([1.0, 3.0, 2.0, 2.0])

    result = pipeline.compute(
        beta=beta,
        model_type="linear",
        X=X,
        agg_segments=segments,
        agg_num_segments=2,
        agg_weights=weights,
        H=H,
    )

    pred = X @ beta
    agg = np.array(
        [
            np.average(pred[:2], weights=weights[:2]),
            np.average(pred[2:], weights=weights[2:]),
        ]
    )
    expected = agg @ H

    np.testing.assert_allclose(result["estimate"], expected)
    np.testing.assert_allclose(
        get_se(result["jacobian"], np.eye(beta.size)),
        np.sqrt(np.sum(result["jacobian"] ** 2, axis=1)),
    )
    np.testing.assert_allclose(
        result["jacobian"],
        finite_difference_jacobian(
            lambda b: pipeline.compute(
                beta=b,
                model_type="linear",
                X=X,
                agg_segments=segments,
                agg_num_segments=2,
                agg_weights=weights,
                H=H,
            )["estimate"],
            beta,
        ),
        rtol=1e-6,
        atol=1e-6,
    )


def test_pipeline_comparisons_ops_est_keep_and_hypothesis():
    from marginaleffects.autodiff import pipeline

    beta = np.array([0.3, -0.2])
    X_lo = np.array(
        [
            [1.0, 0.0],
            [1.0, 1.0],
            [1.0, 2.0],
            [1.0, 3.0],
            [1.0, 4.0],
        ]
    )
    X_hi = X_lo.copy()
    X_hi[:, 1] += 0.5
    ops = [
        {"op": "difference", "n": 2, "w": None},
        {"op": "ratioavg", "n": 3, "w": np.array([1.0, 2.0, 3.0])},
    ]
    est_keep = np.array([0, 2], dtype=np.int32)
    H = np.array([[1.0], [2.0]])

    result = pipeline.compute(
        beta=beta,
        model_type="glm",
        family="binomial",
        link="logit",
        X_hi=X_hi,
        X_lo=X_lo,
        ops=ops,
        est_keep=est_keep,
        H=H,
    )

    mu_hi = 1 / (1 + np.exp(-(X_hi @ beta)))
    mu_lo = 1 / (1 + np.exp(-(X_lo @ beta)))
    expected_raw = np.array(
        [
            mu_hi[0] - mu_lo[0],
            mu_hi[1] - mu_lo[1],
            np.average(mu_hi[2:5], weights=ops[1]["w"])
            / np.average(mu_lo[2:5], weights=ops[1]["w"]),
        ]
    )
    expected = expected_raw[est_keep] @ H

    np.testing.assert_allclose(result["estimate"], expected)
    np.testing.assert_allclose(
        result["jacobian"],
        finite_difference_jacobian(
            lambda b: pipeline.compute(
                beta=b,
                model_type="glm",
                family="binomial",
                link="logit",
                X_hi=X_hi,
                X_lo=X_lo,
                ops=ops,
                est_keep=est_keep,
                H=H,
            )["estimate"],
            beta,
        ),
        rtol=1e-5,
        atol=1e-6,
    )


def test_pipeline_uses_forward_mode_for_many_outputs(monkeypatch):
    import jax

    from marginaleffects.autodiff import pipeline

    def forbidden_jacrev(*args, **kwargs):
        raise RuntimeError("many-output pipeline should not call jacrev")

    monkeypatch.setattr(jax, "jacrev", forbidden_jacrev)

    beta = np.array([0.3, -0.2, 0.5])
    X = np.arange(15, dtype=float).reshape(5, 3)

    result = pipeline.compute(beta=beta, model_type="linear", X=X)

    assert result["jacobian"].shape == (5, 3)


def test_pipeline_uses_reverse_mode_for_few_outputs(monkeypatch):
    import jax

    from marginaleffects.autodiff import pipeline

    def forbidden_jacfwd(*args, **kwargs):
        raise RuntimeError("few-output pipeline should not call jacfwd")

    monkeypatch.setattr(jax, "jacfwd", forbidden_jacfwd)

    beta = np.array([0.3, -0.2, 0.5, 0.1, -0.4])
    X = np.arange(10, dtype=float).reshape(2, 5)

    result = pipeline.compute(beta=beta, model_type="linear", X=X)

    assert result["jacobian"].shape == (2, 5)
