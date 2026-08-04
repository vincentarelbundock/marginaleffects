"""Analytic Jacobians for model-matrix based linear predictions."""

from dataclasses import dataclass

import numpy as np

from ..planning import (
    comparison_plan_apply,
    plan_values_allclose,
    prediction_plan_apply,
)
from .delta import get_se


@dataclass(frozen=True)
class AnalyticResult:
    std_error: np.ndarray
    jacobian: np.ndarray


def _linear_design(model, value, n_pred):
    args = model.get_autodiff_args()
    if not isinstance(args, dict) or args.get("model_type") != "linear":
        return None
    X = np.asarray(value, dtype=float)
    coefs = np.asarray(model.get_coef()).reshape(-1)
    if X.ndim != 2 or X.shape != (n_pred, coefs.size):
        return None
    return X


def _align_rows(X, align):
    if align is None:
        return X
    align = np.asarray(align, dtype=int)
    if np.any(align < 0):
        return None
    return X[align]


def _apply_hypothesis(J, hyp):
    if hyp is None:
        return J
    if hyp.kind != "matrix" or hyp.H is None:
        return None
    return np.asarray(hyp.H, dtype=float).T @ J


def _weighted_rows(X, weights):
    if weights is None:
        return np.mean(X, axis=0)
    weights = np.asarray(weights, dtype=float)
    keep = ~np.isnan(weights)
    denominator = np.sum(weights[keep])
    if denominator == 0:
        return np.full(X.shape[1], np.nan)
    return np.sum(X[keep] * weights[keep, None], axis=0) / denominator


def _prediction_jacobian(plan, model):
    X = _linear_design(model, plan.exog, plan.n_pred)
    if X is None or plan.has_na:
        return None
    J = _align_rows(X, plan.align)
    if J is None:
        return None
    if plan.agg is not None:
        J = np.asarray([_weighted_rows(J[g.idx], g.w) for g in plan.agg])
    J = _apply_hypothesis(J, plan.hyp)
    if J is None:
        return None
    beta = np.asarray(model.get_coef(), dtype=float).reshape(-1)
    return J, prediction_plan_apply(plan, X @ beta)


def _comparison_jacobian(plan, model):
    raw_hi = _linear_design(model, plan.exog_hi, plan.n_pred)
    raw_lo = _linear_design(model, plan.exog_lo, plan.n_pred)
    if raw_hi is None or raw_lo is None or plan.need_y or plan.has_na:
        return None
    X_hi = _align_rows(raw_hi, plan.align)
    X_lo = _align_rows(raw_lo, plan.align)
    if X_hi is None or X_lo is None:
        return None

    beta = np.asarray(model.get_coef(), dtype=float).reshape(-1)
    hi = X_hi @ beta
    lo = X_lo @ beta
    J = np.empty((plan.n_comp, beta.size), dtype=float)
    for group in plan.groups:
        idx = np.asarray(group.idx, dtype=int)
        key = group.fun_key
        if key == "difference":
            value = X_hi[idx] - X_lo[idx]
        elif key == "ratio":
            value = (X_hi[idx] * lo[idx, None] - X_lo[idx] * hi[idx, None]) / np.square(
                lo[idx, None]
            )
        elif key in {"differenceavg", "differenceavgwts"}:
            value = _weighted_rows(X_hi[idx] - X_lo[idx], group.w)
        elif key in {"ratioavg", "ratioavgwts"}:
            mean_hi = _weighted_rows(X_hi[idx], group.w)
            mean_lo = _weighted_rows(X_lo[idx], group.w)
            pred_hi = float(mean_hi @ beta)
            pred_lo = float(mean_lo @ beta)
            value = (mean_hi * pred_lo - mean_lo * pred_hi) / pred_lo**2
        else:
            return None
        J[group.out_idx] = np.asarray(value).reshape(-1, beta.size)
    J = _apply_hypothesis(J, plan.hyp)
    if J is None:
        return None
    return J, comparison_plan_apply(plan, raw_hi @ beta, raw_lo @ beta)


def analytic_try(plan, model, V, estimate, kind):
    """Return analytic delta-method results when the full plan is supported.

    The closed-form Jacobian is only returned when replaying the plan from the
    linear predictor reproduces the point estimates it is meant to differentiate.
    Otherwise the caller falls back to automatic or numerical differentiation.
    """
    if plan is None or V is None:
        return None
    built = (
        _prediction_jacobian(plan, model)
        if kind == "predictions"
        else _comparison_jacobian(plan, model)
    )
    if built is None:
        return None
    J, replay = built
    estimate = np.asarray(estimate, dtype=float).reshape(-1)
    if J.shape[0] != estimate.size:
        return None
    if not plan_values_allclose(replay, estimate):
        return None
    se = get_se(J, V)
    se[se == 0] = np.nan
    return AnalyticResult(std_error=se, jacobian=J)
