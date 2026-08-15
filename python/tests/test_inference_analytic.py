import numpy as np

from marginaleffects.inference.analytic import analytic_try
from marginaleffects.inference.delta import get_jacobian
from marginaleffects.planning import (
    AggGroup,
    CompGroup,
    ComparisonPlan,
    PredictionPlan,
    comparison_plan_apply,
)
from marginaleffects.estimands import estimands


class LinearAdapter:
    def __init__(self, beta):
        self.beta = np.asarray(beta, dtype=float)

    def get_coef(self):
        return self.beta

    def get_autodiff_args(self):
        return {"model_type": "linear", "family": None, "link": None}


def test_analytic_prediction_jacobian_with_aggregation():
    model = LinearAdapter([1.0, 2.0])
    X = np.asarray([[1.0, 0.0], [1.0, 2.0], [1.0, 4.0]])
    plan = PredictionPlan(
        n_pred=3,
        exog=X,
        align=None,
        has_na=False,
        agg=[AggGroup(idx=np.arange(3), w=None)],
        hyp=None,
        n_out=1,
    )
    out = analytic_try(plan, model, np.eye(2), [5.0], "predictions")
    np.testing.assert_allclose(out.jacobian, [[1.0, 2.0]])


def test_analytic_prediction_falls_back_when_replay_disagrees():
    model = LinearAdapter([1.0, 2.0])
    X = np.asarray([[1.0, 0.0], [1.0, 2.0], [1.0, 4.0]])
    plan = PredictionPlan(
        n_pred=3,
        exog=X,
        align=None,
        has_na=False,
        agg=None,
        hyp=None,
        n_out=3,
    )
    estimate = X @ model.get_coef()
    assert analytic_try(plan, model, np.eye(2), estimate, "predictions") is not None
    # The design matrix no longer explains the estimates: refuse to differentiate
    # it and let the caller fall back to automatic or numerical differentiation.
    assert analytic_try(plan, model, np.eye(2), estimate + 1.0, "predictions") is None


def test_analytic_ratio_matches_finite_difference():
    model = LinearAdapter([2.0, 0.5])
    X_hi = np.asarray([[1.0, 2.0], [1.0, 4.0]])
    X_lo = np.asarray([[1.0, 1.0], [1.0, 3.0]])
    group = CompGroup(
        idx=np.arange(2),
        out_idx=np.arange(2),
        scalar=False,
        fun=estimands["ratio"],
        fun_key="ratio",
        x=np.asarray([2.0, 4.0]),
        w=None,
    )
    plan = ComparisonPlan(
        n_pred=2,
        exog_hi=X_hi,
        exog_lo=X_lo,
        exog_nd=None,
        need_y=False,
        align=None,
        eps=1e-4,
        groups=[group],
        n_comp=2,
        hyp=None,
    )

    def replay(beta):
        return comparison_plan_apply(plan, X_hi @ beta, X_lo @ beta)

    estimate = replay(model.get_coef())
    analytic = analytic_try(plan, model, np.eye(2), estimate, "comparisons")
    numeric = get_jacobian(replay, model.get_coef())
    np.testing.assert_allclose(analytic.jacobian, numeric, rtol=1e-6, atol=1e-6)

    assert analytic_try(plan, model, np.eye(2), estimate + 1.0, "comparisons") is None
