"""Analytic Jacobians for model-matrix based predictions."""

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


# Comparison keys with a closed-form derivative with respect to the model
# coefficients. Elasticities are excluded on purpose: they depend on the fitted
# response, so their gradient needs a product rule over an extra prediction.
_ROWWISE_OPS = {
    "difference": lambda J_hi, J_lo, hi, lo, eps: J_hi - J_lo,
    "dydx": lambda J_hi, J_lo, hi, lo, eps: (J_hi - J_lo) / eps,
    "expdydx": lambda J_hi, J_lo, hi, lo, eps: (
        (J_hi * np.exp(hi)[:, None] - J_lo * np.exp(lo)[:, None]) / (np.exp(eps) * eps)
    ),
    "ratio": lambda J_hi, J_lo, hi, lo, eps: (
        (J_hi * lo[:, None] - J_lo * hi[:, None]) / np.square(lo)[:, None]
    ),
    # lift is hi / lo - 1, so it shares the ratio gradient.
    "lift": lambda J_hi, J_lo, hi, lo, eps: (
        (J_hi * lo[:, None] - J_lo * hi[:, None]) / np.square(lo)[:, None]
    ),
    "lnratio": lambda J_hi, J_lo, hi, lo, eps: J_hi / hi[:, None] - J_lo / lo[:, None],
    "lnor": lambda J_hi, J_lo, hi, lo, eps: (
        J_hi / (hi * (1 - hi))[:, None] - J_lo / (lo * (1 - lo))[:, None]
    ),
}

# Keys whose average form is the average of the row-level comparisons, so their
# gradient is the weighted mean of the row-level gradients.
_LINEAR_KEYS = ("difference", "dydx", "expdydx")

# Keys whose average form aggregates the predictions *before* applying a
# nonlinear function. `H` and `L` are the aggregated high and low predictions.
_SCALAR_OPS = {
    "ratioavg": lambda Jb_hi, Jb_lo, H, L: (Jb_hi * L - Jb_lo * H) / L**2,
    "liftavg": lambda Jb_hi, Jb_lo, H, L: (Jb_hi * L - Jb_lo * H) / L**2,
    "lnratioavg": lambda Jb_hi, Jb_lo, H, L: Jb_hi / H - Jb_lo / L,
    "lnoravg": lambda Jb_hi, Jb_lo, H, L: Jb_hi / (H * (1 - H)) - Jb_lo / (L * (1 - L)),
}

_SUPPORTED_KEYS = frozenset(
    list(_ROWWISE_OPS)
    + [k + suffix for k in _LINEAR_KEYS for suffix in ("avg", "avgwts")]
    + list(_SCALAR_OPS)
    + [k + "wts" for k in _SCALAR_OPS]
)


def _design(model, value, n_pred):
    """Return the cached design matrix when it lines up with the coefficients."""
    X = np.asarray(value, dtype=float)
    coefs = np.asarray(model.get_coef()).reshape(-1)
    if X.ndim != 2 or X.shape != (n_pred, coefs.size):
        return None
    return X


def _gradient(model, value, n_pred, link):
    """Prediction gradient with respect to the coefficients, and its values.

    On the link scale the gradient is the design matrix itself. On the response
    scale the chain rule scales each row by the inverse-link derivative.
    """
    X = _design(model, value, n_pred)
    if X is None:
        return None
    beta = np.asarray(model.get_coef(), dtype=float).reshape(-1)
    eta = X @ beta
    if link is None:
        return X, eta
    linkinv, mu_eta = link
    return X * np.asarray(mu_eta(eta), dtype=float)[:, None], np.asarray(
        linkinv(eta), dtype=float
    )


def _model_link(model):
    """Return `(linkinv, mu_eta)`, `None` for link-scale models, or False."""
    args = model.get_autodiff_args()
    if not isinstance(args, dict):
        return False
    model_type = args.get("model_type")
    if model_type == "linear":
        return None
    if model_type != "glm":
        return False
    # Adapters predating `get_link_functions()` keep the numerical fallback.
    get_link = getattr(model, "get_link_functions", None)
    link = get_link() if callable(get_link) else None
    if link is None:
        return False
    return link


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


def _group_weights(weights, n):
    """Normalized weights matching the averaging done by the estimands."""
    if weights is None:
        return np.full(n, 1.0 / n)
    weights = np.asarray(weights, dtype=float)
    if weights.shape != (n,):
        return None
    keep = ~np.isnan(weights)
    denominator = np.sum(weights[keep])
    if denominator == 0 or not np.isfinite(denominator):
        return None
    out = np.where(keep, weights, 0.0) / denominator
    if not np.all(np.isfinite(out)):
        return None
    return out


def _weighted_rows(X, weights):
    w = _group_weights(weights, X.shape[0])
    if w is None:
        return np.full(X.shape[1], np.nan)
    return w @ X


def _prediction_jacobian(plan, model, link):
    built = _gradient(model, plan.exog, plan.n_pred, link)
    if built is None or plan.has_na:
        return None
    J, values = built
    J = _align_rows(J, plan.align)
    if J is None:
        return None
    if plan.agg is not None:
        J = np.asarray([_weighted_rows(J[g.idx], g.w) for g in plan.agg])
    J = _apply_hypothesis(J, plan.hyp)
    if J is None:
        return None
    return J, prediction_plan_apply(plan, values)


def _group_jacobian(J_hi, J_lo, hi, lo, eps, group):
    """Closed-form gradient for one comparison group, or None if ineligible."""
    key = group.fun_key
    if key not in _SUPPORTED_KEYS:
        return None
    idx = np.asarray(group.idx, dtype=int)
    n = idx.size
    if n == 0:
        return None
    weighted = key.endswith("avgwts")
    base = key[: -len("wts")] if weighted else key
    if base.startswith(("dydx", "expdydx")) and not (
        isinstance(eps, (int, float)) and np.isfinite(eps) and eps != 0
    ):
        return None

    block_hi, block_lo = J_hi[idx], J_lo[idx]
    v_hi, v_lo = hi[idx], lo[idx]

    if base in _ROWWISE_OPS:
        return _ROWWISE_OPS[base](block_hi, block_lo, v_hi, v_lo, eps)

    w = _group_weights(group.w if weighted else None, n)
    if w is None:
        return None
    inner = base[: -len("avg")]
    if inner in _LINEAR_KEYS:
        grad = _ROWWISE_OPS[inner](block_hi, block_lo, v_hi, v_lo, eps)
        return (w @ grad).reshape(1, -1)
    if base in _SCALAR_OPS:
        return _SCALAR_OPS[base](
            w @ block_hi, w @ block_lo, float(w @ v_hi), float(w @ v_lo)
        ).reshape(1, -1)
    return None


def _comparison_jacobian(plan, model, link):
    built_hi = _gradient(model, plan.exog_hi, plan.n_pred, link)
    built_lo = _gradient(model, plan.exog_lo, plan.n_pred, link)
    if built_hi is None or built_lo is None or plan.need_y or plan.has_na:
        return None
    raw_hi, values_hi = built_hi
    raw_lo, values_lo = built_lo
    J_hi = _align_rows(raw_hi, plan.align)
    J_lo = _align_rows(raw_lo, plan.align)
    if J_hi is None or J_lo is None:
        return None
    hi = _align_rows(values_hi, plan.align)
    lo = _align_rows(values_lo, plan.align)

    J = np.empty((plan.n_comp, J_hi.shape[1]), dtype=float)
    for group in plan.groups:
        value = _group_jacobian(J_hi, J_lo, hi, lo, plan.eps, group)
        if value is None:
            return None
        value = np.asarray(value, dtype=float).reshape(-1, J_hi.shape[1])
        if value.shape[0] != np.asarray(group.out_idx).size:
            return None
        J[group.out_idx] = value
    J = _apply_hypothesis(J, plan.hyp)
    if J is None:
        return None
    return J, comparison_plan_apply(plan, values_hi, values_lo)


def analytic_try(plan, model, V, estimate, kind):
    """Return analytic delta-method results when the full plan is supported.

    The closed-form Jacobian is only returned when replaying the plan from the
    linear predictor reproduces the point estimates it is meant to differentiate.
    Otherwise the caller falls back to automatic or numerical differentiation.
    """
    if plan is None or V is None:
        return None
    link = _model_link(model)
    if link is False:
        return None
    built = (
        _prediction_jacobian(plan, model, link)
        if kind == "predictions"
        else _comparison_jacobian(plan, model, link)
    )
    if built is None:
        return None
    J, replay = built
    estimate = np.asarray(estimate, dtype=float).reshape(-1)
    if J.shape[0] != estimate.size:
        return None
    if not plan_values_allclose(replay, estimate):
        return None
    # Nonlinear comparisons can divide by a zero prediction. Fall back rather
    # than propagating a non-finite derivative into the variance.
    if not np.all(np.isfinite(J)):
        return None
    se = get_se(J, V)
    se[se == 0] = np.nan
    return AnalyticResult(std_error=se, jacobian=J)
