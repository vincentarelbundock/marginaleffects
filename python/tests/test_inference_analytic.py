import numpy as np

from marginaleffects.estimands import estimands
from marginaleffects.hypothesis_compile import hypothesis_compile
from marginaleffects.inference.analytic import analytic_try
from marginaleffects.inference.delta import get_jacobian
from marginaleffects.planning import (
    AggGroup,
    ComparisonPlan,
    CompGroup,
    Hyp,
    PredictionPlan,
    comparison_plan_apply,
)


class NamedMatrix(np.ndarray):
    def __new__(cls, value, columns):
        out = np.asarray(value, dtype=float).view(cls)
        out.columns = list(columns)
        return out


class LinearAdapter:
    def __init__(self, beta, coefnames=None):
        self.beta = np.asarray(beta, dtype=float)
        self.coefnames = coefnames or [f"b{i}" for i in range(len(beta))]

    def get_coef(self):
        return self.beta

    def get_coefnames(self):
        return np.asarray(self.coefnames)

    def get_exog_names(self, value):
        return value.columns

    def get_analytic_args(self):
        return {"model_type": "linear", "family": None, "link": None}


class LogAdapter(LinearAdapter):
    def get_analytic_args(self):
        return {"model_type": "glm", "family": "custom", "link": "custom"}

    def get_link_functions(self):
        return np.exp, np.exp


class LogitAdapter(LinearAdapter):
    def get_analytic_args(self):
        return {"model_type": "glm", "family": "custom", "link": "custom"}

    def get_link_functions(self):
        def inverse(value):
            return 1 / (1 + np.exp(-value))

        def derivative(value):
            mu = inverse(value)
            return mu * (1 - mu)

        return inverse, derivative


def test_analytic_prediction_jacobian_with_aggregation():
    model = LinearAdapter([1.0, 2.0])
    X = NamedMatrix([[1.0, 0.0], [1.0, 2.0], [1.0, 4.0]], ["b0", "b1"])
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
    X = NamedMatrix([[1.0, 0.0], [1.0, 2.0], [1.0, 4.0]], ["b0", "b1"])
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
    X_hi = NamedMatrix([[1.0, 2.0], [1.0, 4.0]], ["b0", "b1"])
    X_lo = NamedMatrix([[1.0, 1.0], [1.0, 3.0]], ["b0", "b1"])
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


def test_analytic_glm_response_scale_lnratio():
    model = LogAdapter([0.2, 0.4])
    X_hi = NamedMatrix([[1.0, 2.0], [1.0, 4.0]], ["b0", "b1"])
    X_lo = NamedMatrix([[1.0, 1.0], [1.0, 3.0]], ["b0", "b1"])
    plan = ComparisonPlan(
        n_pred=2,
        exog_hi=X_hi,
        exog_lo=X_lo,
        exog_nd=None,
        need_y=False,
        align=None,
        eps=1e-4,
        groups=[
            CompGroup(
                idx=np.arange(2),
                out_idx=np.arange(2),
                scalar=False,
                fun=estimands["lnratio"],
                fun_key="lnratio",
                x=None,
                w=None,
            )
        ],
        n_comp=2,
        hyp=None,
    )
    hi, lo = np.exp(X_hi @ model.get_coef()), np.exp(X_lo @ model.get_coef())
    estimate = comparison_plan_apply(plan, hi, lo)
    out = analytic_try(plan, model, np.eye(2), estimate, "comparisons")
    np.testing.assert_allclose(out.jacobian, X_hi - X_lo, atol=1e-12)


def test_analytic_composes_nonlinear_hypothesis():
    model = LinearAdapter([1.0, 2.0])
    X = NamedMatrix([[1.0, 0.0], [1.0, 2.0]], ["b0", "b1"])
    hyp = Hyp(kind="string", apply=lambda z: np.asarray([z[0] * z[1]]))
    plan = PredictionPlan(
        n_pred=2,
        exog=X,
        align=None,
        has_na=False,
        agg=None,
        hyp=hyp,
        n_out=1,
    )
    pred = X @ model.get_coef()
    estimate = hyp.apply(pred)
    out = analytic_try(plan, model, np.eye(2), estimate, "predictions")
    assert out.method == "analytic+numeric_stage"
    expected = pred[1] * X[0] + pred[0] * X[1]
    np.testing.assert_allclose(out.jacobian[0], expected, rtol=1e-9, atol=1e-9)


def test_analytic_reorders_named_design_columns():
    model = LinearAdapter([2.0, 3.0], coefnames=["a", "b"])
    X = NamedMatrix([[5.0, 1.0], [7.0, 1.0]], ["b", "a"])
    plan = PredictionPlan(
        n_pred=2,
        exog=X,
        align=None,
        has_na=False,
        agg=None,
        hyp=None,
        n_out=2,
    )
    expected_J = np.asarray([[1.0, 5.0], [1.0, 7.0]])
    estimate = expected_J @ model.get_coef()
    out = analytic_try(plan, model, np.eye(2), estimate, "predictions")
    np.testing.assert_array_equal(out.jacobian, expected_J)


def test_analytic_rejects_mismatched_or_duplicate_design_names():
    model = LinearAdapter([2.0, 3.0], coefnames=["a", "b"])
    for columns in (["a", "c"], ["a", "a"]):
        X = NamedMatrix([[1.0, 5.0]], columns)
        plan = PredictionPlan(1, X, None, False, None, None, 1)
        assert analytic_try(plan, model, np.eye(2), [17.0], "predictions") is None


def test_linear_string_hypothesis_compiles_to_matrix():
    import polars as pl

    frame = pl.DataFrame({"estimate": [2.0, 5.0]})
    _, hyp = hypothesis_compile(frame, "2 * b1 - b0 = 0")
    assert hyp.kind == "matrix"
    np.testing.assert_array_equal(hyp.H, [[-1.0], [2.0]])


def test_affine_and_nonlinear_hypotheses_remain_composable():
    import polars as pl

    frame = pl.DataFrame({"estimate": [2.0, 5.0]})
    # Affine maps promote to a matrix plus an offset: the derivative is H
    # alone, so a large constant can never cancel a numeric probe step.
    _, affine = hypothesis_compile(frame, "b1 - b0 = 1")
    _, nonlinear = hypothesis_compile(frame, "b1**2 - b0 = 0")
    assert affine.kind == "matrix"
    np.testing.assert_array_equal(np.asarray(affine.H).reshape(-1), [-1.0, 1.0])
    assert float(np.atleast_1d(affine.offset)[0]) == -1.0
    assert nonlinear.kind == "string"


def test_linear_formula_hypothesis_compiles_to_matrix():
    import polars as pl

    frame = pl.DataFrame({"term": ["a", "b", "c"], "estimate": [2.0, 5.0, 9.0]})
    _, hyp = hypothesis_compile(frame, "difference~sequential")
    assert hyp.kind == "matrix"
    np.testing.assert_array_equal(hyp.H, [[-1.0, 0.0], [1.0, -1.0], [0.0, 1.0]])


def test_logit_lnor_jacobian_collapses_to_design_difference():
    model = LogitAdapter([0.2, 0.4])
    X_hi = NamedMatrix([[1.0, 2.0], [1.0, 4.0]], ["b0", "b1"])
    X_lo = NamedMatrix([[1.0, 1.0], [1.0, 3.0]], ["b0", "b1"])
    group = CompGroup(
        idx=np.arange(2),
        out_idx=np.arange(2),
        scalar=False,
        fun=estimands["lnor"],
        fun_key="lnor",
        x=None,
        w=None,
    )
    plan = ComparisonPlan(2, X_hi, X_lo, None, False, None, 1e-4, [group], 2, None)
    inverse, _ = model.get_link_functions()
    estimate = comparison_plan_apply(
        plan, inverse(X_hi @ model.get_coef()), inverse(X_lo @ model.get_coef())
    )
    out = analytic_try(plan, model, np.eye(2), estimate, "comparisons")
    np.testing.assert_allclose(out.jacobian, X_hi - X_lo, atol=1e-12)


def test_missing_prediction_plan_falls_back():
    model = LinearAdapter([1.0, 2.0])
    X = NamedMatrix([[1.0, 0.0]], ["b0", "b1"])
    plan = PredictionPlan(1, X, None, True, None, None, 1)
    assert analytic_try(plan, model, np.eye(2), [1.0], "predictions") is None
