from dataclasses import replace

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


class GlmAdapter(LinearAdapter):
    """Logit adapter: predictions live on the response scale."""

    def get_autodiff_args(self):
        return {"model_type": "glm", "family": "binomial", "link": "logit"}

    def get_link_functions(self):
        def linkinv(eta):
            return 1.0 / (1.0 + np.exp(-eta))

        def mu_eta(eta):
            mu = linkinv(eta)
            return mu * (1.0 - mu)

        return linkinv, mu_eta


def _comparison_plan(X_hi, X_lo, fun_key, w=None, eps=1e-4, scalar=False):
    n = X_hi.shape[0]
    group = CompGroup(
        idx=np.arange(n),
        out_idx=np.arange(1) if scalar else np.arange(n),
        scalar=scalar,
        fun=estimands[fun_key],
        fun_key=fun_key,
        x=None,
        w=w,
    )
    return ComparisonPlan(
        n_pred=n,
        exog_hi=X_hi,
        exog_lo=X_lo,
        exog_nd=None,
        need_y=False,
        align=None,
        eps=eps,
        groups=[group],
        n_comp=1 if scalar else n,
        hyp=None,
    )


def _assert_matches_finite_difference(model, plan, X_hi, X_lo, link=None):
    def replay(beta):
        eta_hi, eta_lo = X_hi @ beta, X_lo @ beta
        if link is None:
            return comparison_plan_apply(plan, eta_hi, eta_lo)
        return comparison_plan_apply(plan, link(eta_hi), link(eta_lo))

    estimate = replay(model.get_coef())
    V = np.eye(len(model.get_coef()))
    analytic = analytic_try(plan, model, V, estimate, "comparisons")
    assert analytic is not None
    numeric = get_jacobian(replay, model.get_coef())
    # The finite-difference Jacobian is the reference here, so the tolerance
    # reflects its accuracy rather than that of the closed form.
    np.testing.assert_allclose(analytic.jacobian, numeric, rtol=1e-5, atol=1e-6)


X_HI = np.asarray([[1.0, 2.0], [1.0, 4.0], [1.0, 5.0]])
X_LO = np.asarray([[1.0, 1.0], [1.0, 3.0], [1.0, 3.5]])


def test_analytic_rowwise_keys_match_finite_difference():
    model = LinearAdapter([2.0, 0.5])
    for key in ("difference", "dydx", "ratio", "lnratio", "lift", "expdydx"):
        plan = _comparison_plan(X_HI, X_LO, key)
        _assert_matches_finite_difference(model, plan, X_HI, X_LO)


def test_analytic_average_keys_match_finite_difference():
    model = LinearAdapter([2.0, 0.5])
    # `expdydxavg` is omitted: its estimand is written against polars Series, so
    # it cannot be replayed from the numpy arrays these plans carry.
    keys = ("differenceavg", "dydxavg", "ratioavg", "lnratioavg", "liftavg")
    for key in keys:
        plan = _comparison_plan(X_HI, X_LO, key, scalar=True)
        _assert_matches_finite_difference(model, plan, X_HI, X_LO)


def test_analytic_weighted_average_keys_match_finite_difference():
    model = LinearAdapter([2.0, 0.5])
    w = np.asarray([1.0, 2.0, 3.0])
    for key in ("differenceavgwts", "dydxavgwts", "ratioavgwts", "lnratioavgwts"):
        plan = _comparison_plan(X_HI, X_LO, key, w=w, scalar=True)
        _assert_matches_finite_difference(model, plan, X_HI, X_LO)


def test_analytic_glm_response_scale_applies_the_chain_rule():
    model = GlmAdapter([0.2, 0.3])
    linkinv, mu_eta = model.get_link_functions()
    for key in ("difference", "dydx", "ratio", "lnratio", "lnor"):
        plan = _comparison_plan(X_HI, X_LO, key)
        _assert_matches_finite_difference(model, plan, X_HI, X_LO, link=linkinv)

    # The prediction Jacobian is the design matrix scaled row-wise by mu.eta().
    beta = model.get_coef()
    plan = PredictionPlan(
        n_pred=3,
        exog=X_HI,
        align=None,
        has_na=False,
        agg=None,
        hyp=None,
        n_out=3,
    )
    eta = X_HI @ beta
    out = analytic_try(plan, model, np.eye(2), linkinv(eta), "predictions")
    np.testing.assert_allclose(out.jacobian, X_HI * mu_eta(eta)[:, None])

    # Log-odds are linear in the coefficients, so `lnor` on a logit response
    # scale must collapse back to the plain design-matrix difference. This pins
    # the chain rule exactly, without a finite-difference reference.
    lnor_plan = _comparison_plan(X_HI, X_LO, "lnor")
    estimate = comparison_plan_apply(
        lnor_plan, linkinv(X_HI @ beta), linkinv(X_LO @ beta)
    )
    lnor = analytic_try(lnor_plan, model, np.eye(2), estimate, "comparisons")
    np.testing.assert_allclose(lnor.jacobian, X_HI - X_LO)


def test_analytic_declines_unsupported_and_nonfinite_cases():
    model = LinearAdapter([2.0, 0.5])
    V = np.eye(2)

    # Elasticities depend on the fitted response, which the plan flags.
    plan = _comparison_plan(X_HI, X_LO, "difference")
    estimate = comparison_plan_apply(
        plan, X_HI @ model.get_coef(), X_LO @ model.get_coef()
    )
    assert analytic_try(plan, model, V, estimate, "comparisons") is not None
    elasticity = replace(plan, need_y=True)
    assert analytic_try(elasticity, model, V, estimate, "comparisons") is None

    # A comparison function with no recorded key cannot be differentiated.
    unkeyed = replace(plan, groups=[replace(plan.groups[0], fun_key=None)])
    assert analytic_try(unkeyed, model, V, estimate, "comparisons") is None

    # A ratio through a zero prediction has a non-finite derivative.
    beta = np.asarray([0.0, 1.0])
    zero_lo = np.asarray([[1.0, 0.0], [1.0, 3.0], [1.0, 3.5]])
    plan = _comparison_plan(X_HI, zero_lo, "ratio")
    model_zero = LinearAdapter(beta)
    estimate = comparison_plan_apply(plan, X_HI @ beta, zero_lo @ beta)
    assert analytic_try(plan, model_zero, V, estimate, "comparisons") is None


def test_analytic_declines_glm_without_link_functions():
    class NoLink(GlmAdapter):
        def get_link_functions(self):
            return None

    model = NoLink([0.2, 0.3])
    plan = _comparison_plan(X_HI, X_LO, "difference")
    estimate = comparison_plan_apply(
        plan, X_HI @ model.get_coef(), X_LO @ model.get_coef()
    )
    assert analytic_try(plan, model, np.eye(2), estimate, "comparisons") is None
