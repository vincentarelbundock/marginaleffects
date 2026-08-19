"""Stage-composed analytic Jacobians for model-matrix predictions."""

from dataclasses import dataclass

import numpy as np

from ..planning import (
    comparison_plan_apply_stages,
    plan_values_allclose,
    prediction_plan_apply_stages,
)
from ..settings import is_autodiff_forced
from .delta import get_se
from .gradients import comparison_gradient_exact

STAGE_PROBE_MAX_DIM = 500


@dataclass(frozen=True)
class AnalyticResult:
    std_error: np.ndarray
    jacobian: np.ndarray


def _model_stage(model, value, n_pred):
    try:
        X = np.asarray(value, dtype=float)
        beta = np.asarray(model.get_coef(), dtype=float).reshape(-1)
    except (TypeError, ValueError):
        return None
    if X.ndim != 2 or X.shape != (n_pred, beta.size) or not np.isfinite(X).all():
        return None
    args = model.get_autodiff_args()
    if isinstance(args, dict) and args.get("model_type") == "linear":
        return X, X @ beta, None
    functions = model.get_link_functions()
    if functions is None:
        return None
    linkinv, mu_eta = functions
    try:
        eta = X @ beta
        pred = np.asarray(linkinv(eta), dtype=float).reshape(-1)
        deriv = np.asarray(mu_eta(eta), dtype=float).reshape(-1)
    except Exception:  # noqa: BLE001 -- adapters must fail closed
        return None
    if pred.shape != (n_pred,) or deriv.shape != (n_pred,):
        return None
    if not np.isfinite(pred).all() or not np.isfinite(deriv).all():
        return None
    return X, pred, deriv


def _align_rows(value, align):
    if align is None:
        return value
    align = np.asarray(align, dtype=int)
    if np.any(align < 0):
        return None
    return value[align]


def _weighted_columns(X, weights):
    if weights is None:
        return np.mean(X, axis=0)
    weights = np.asarray(weights, dtype=float)
    keep = ~np.isnan(weights)
    denominator = np.sum(weights[keep])
    if denominator == 0:
        return np.full(X.shape[1], np.nan)
    return weights[keep] @ X[keep] / denominator


def _try_stage(f, x):
    try:
        out = np.asarray(f(x), dtype=float).reshape(-1)
    except Exception:  # noqa: BLE001 -- arbitrary hypothesis callables fail closed
        return None
    return out if np.isfinite(out).all() else None


def stage_jacobian_dense(f, x):
    """Differentiate cheap downstream arithmetic without re-running the model."""
    x = np.asarray(x, dtype=float).reshape(-1)
    if x.size == 0 or x.size > STAGE_PROBE_MAX_DIM or not np.isfinite(x).all():
        return None
    base = _try_stage(f, x)
    if base is None:
        return None
    step = np.maximum(np.abs(x), 1.0) * np.finfo(float).eps ** (1 / 3)
    out = np.empty((base.size, x.size), dtype=float)
    for j in range(x.size):
        hi, lo = x.copy(), x.copy()
        hi[j] += step[j]
        lo[j] -= step[j]
        fhi, flo = _try_stage(f, hi), _try_stage(f, lo)
        if (
            fhi is None
            or flo is None
            or fhi.shape != base.shape
            or flo.shape != base.shape
        ):
            return None
        out[:, j] = (fhi - flo) / (2 * step[j])
    return out if np.isfinite(out).all() else None


def _apply_hypothesis(J, hyp, estimate_pre):
    if hyp is None:
        return J
    if hyp.kind == "matrix" and hyp.H is not None:
        H = np.asarray(hyp.H, dtype=float)
        if H.ndim == 2 and H.shape[0] == J.shape[0] and np.isfinite(H).all():
            return H.T @ J
        return None
    if not callable(hyp.apply) or len(estimate_pre) != J.shape[0]:
        return None
    G = stage_jacobian_dense(hyp.apply, estimate_pre)
    return None if G is None or G.shape[1] != J.shape[0] else G @ J


def _prediction_jacobian(plan, model):
    stage = _model_stage(model, plan.exog, plan.n_pred)
    if stage is None or plan.has_na:
        return None
    X, pred, deriv = stage
    J = X if deriv is None else X * deriv[:, None]
    J = _align_rows(J, plan.align)
    if J is None:
        return None
    if plan.agg is not None:
        J = np.asarray([_weighted_columns(J[g.idx], g.w) for g in plan.agg])
    replay = prediction_plan_apply_stages(plan, pred)
    J = _apply_hypothesis(J, plan.hyp, replay.pre)
    if J is None or not np.isfinite(J).all():
        return None
    return J, replay.post


def _comparison_jacobian(plan, model):
    if plan.has_na or any(group.uses_y for group in plan.groups):
        return None
    hi_stage = _model_stage(model, plan.exog_hi, plan.n_pred)
    lo_stage = _model_stage(model, plan.exog_lo, plan.n_pred)
    if hi_stage is None or lo_stage is None:
        return None
    X_hi, raw_hi, d_hi = hi_stage
    X_lo, raw_lo, d_lo = lo_stage
    X_hi, X_lo = _align_rows(X_hi, plan.align), _align_rows(X_lo, plan.align)
    hi, lo = _align_rows(raw_hi, plan.align), _align_rows(raw_lo, plan.align)
    d_hi = None if d_hi is None else _align_rows(d_hi, plan.align)
    d_lo = None if d_lo is None else _align_rows(d_lo, plan.align)
    if any(value is None for value in (X_hi, X_lo, hi, lo)):
        return None

    beta = np.asarray(model.get_coef(), dtype=float).reshape(-1)
    J = np.empty((plan.n_comp, beta.size), dtype=float)
    for group in plan.groups:
        idx = np.asarray(group.idx, dtype=int)
        gradients = comparison_gradient_exact(
            group.fun_key, hi[idx], lo[idx], eps=plan.eps, x=group.x, w=group.w
        )
        if gradients is None:
            return None
        grad_hi, grad_lo = gradients
        if d_hi is not None:
            grad_hi = grad_hi * d_hi[idx]
        if d_lo is not None:
            grad_lo = grad_lo * d_lo[idx]
        if group.scalar:
            value = grad_hi @ X_hi[idx] + grad_lo @ X_lo[idx]
        else:
            value = X_hi[idx] * grad_hi[:, None] + X_lo[idx] * grad_lo[:, None]
        J[group.out_idx] = np.asarray(value).reshape(-1, beta.size)

    replay = comparison_plan_apply_stages(plan, raw_hi, raw_lo)
    J = _apply_hypothesis(J, plan.hyp, replay.pre)
    if J is None or not np.isfinite(J).all():
        return None
    return J, replay.post


def analytic_try(plan, model, V, estimate, kind):
    """Compose exact upstream stages with cheap downstream derivatives."""
    if plan is None or V is None or is_autodiff_forced():
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
    if J.shape[0] != estimate.size or not plan_values_allclose(replay, estimate):
        return None
    return AnalyticResult(std_error=get_se(J, V), jacobian=J)
