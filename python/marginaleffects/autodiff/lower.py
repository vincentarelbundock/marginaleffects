from __future__ import annotations

from dataclasses import dataclass
import warnings

import numpy as np

from ..settings import is_autodiff_enabled
from ..uncertainty import get_se
from .ops import COMPARISON_OPS


@dataclass
class Lowered:
    ok: bool
    kwargs: dict | None = None
    reason: str = ""


@dataclass
class AutodiffResult:
    std_error: np.ndarray
    jacobian: np.ndarray


def _fail(reason: str) -> Lowered:
    return Lowered(False, kwargs=None, reason=reason)


def _model_args(model):
    args = model.get_autodiff_args()
    if args is None:
        return None, _fail("")
    if isinstance(args, str):
        return None, _fail(args)
    return args, None


def _coefs_or_failure(model):
    coefs = np.asarray(model.get_coef(), dtype=float).reshape(-1)
    if np.isnan(coefs).any():
        return None, _fail("models with NA coefficients")
    return coefs, None


def _design_or_failure(X, coefs, n_pred):
    X = np.asarray(X, dtype=float)
    if X.ndim != 2 or X.shape[1] != coefs.size or X.shape[0] != n_pred:
        return None, _fail("this model/data configuration")
    return X, None


def _hypothesis_or_failure(plan):
    if plan.hyp is None:
        return None, None
    if plan.hyp.kind != "matrix":
        return None, _fail("this form of the `hypothesis` argument")
    return np.asarray(plan.hyp.H, dtype=float), None


def _has_nan(x) -> bool:
    return x is not None and np.isnan(np.asarray(x, dtype=float)).any()


def lower_predictions(plan, model) -> Lowered:
    args, failure = _model_args(model)
    if failure is not None:
        return failure
    coefs, failure = _coefs_or_failure(model)
    if failure is not None:
        return failure
    X, failure = _design_or_failure(plan.exog, coefs, plan.n_pred)
    if failure is not None:
        return failure
    if plan.align is not None:
        return _fail("models with grouped/multi-equation outcomes")
    if plan.has_na:
        return _fail("missing values in predictions")
    H, failure = _hypothesis_or_failure(plan)
    if failure is not None:
        return failure

    agg_segments = None
    agg_num_segments = None
    agg_weights = None
    if plan.agg is not None:
        agg_segments = np.empty(plan.n_pred, dtype=np.int32)
        agg_weights = np.ones(plan.n_pred, dtype=float)
        for i, group in enumerate(plan.agg):
            agg_segments[group.idx] = i
            if group.w is not None:
                if _has_nan(group.w):
                    return _fail("missing values in weights")
                agg_weights[group.idx] = np.asarray(group.w, dtype=float)
        agg_num_segments = len(plan.agg)

    kwargs = {
        **args,
        "X": X,
        "agg_segments": agg_segments,
        "agg_num_segments": agg_num_segments,
        "agg_weights": agg_weights if agg_segments is not None else None,
        "H": H,
    }
    return Lowered(True, kwargs=kwargs)


def lower_comparisons(plan, model) -> Lowered:
    args, failure = _model_args(model)
    if failure is not None:
        return failure
    coefs, failure = _coefs_or_failure(model)
    if failure is not None:
        return failure
    X_hi, failure = _design_or_failure(plan.exog_hi, coefs, plan.n_pred)
    if failure is not None:
        return failure
    X_lo, failure = _design_or_failure(plan.exog_lo, coefs, plan.n_pred)
    if failure is not None:
        return failure
    if plan.align is not None:
        return _fail("models with grouped/multi-equation outcomes")
    if plan.has_na:
        return _fail("missing values in predictions")

    if any(group.fun_key is None for group in plan.groups):
        return _fail("custom comparison functions")
    if plan.need_y:
        return _fail("elasticities")
    for group in plan.groups:
        if group.fun_key not in COMPARISON_OPS:
            return _fail(f"comparison='{group.fun_key}'")

    H, failure = _hypothesis_or_failure(plan)
    if failure is not None:
        return failure

    if not plan.groups:
        order = np.asarray([], dtype=int)
    else:
        order = np.concatenate([group.idx for group in plan.groups]).astype(int)
    if order.size != plan.n_pred:
        return _fail("this model/data configuration")

    ops = []
    for group in plan.groups:
        spec = COMPARISON_OPS[group.fun_key]
        w = None
        if spec.weighted:
            if _has_nan(group.w):
                return _fail("missing values in weights")
            w = None if group.w is None else np.asarray(group.w, dtype=float)
        ops.append({"op": spec.pipeline_op, "n": len(group.idx), "w": w})

    kwargs = {
        **args,
        "X_hi": X_hi[order],
        "X_lo": X_lo[order],
        "ops": ops,
        "H": H,
    }
    return Lowered(True, kwargs=kwargs)


def _warn_unsupported(reason):
    if reason:
        warnings.warn(
            "Automatic differentiation does not support "
            f"{reason}. Reverting to finite differences.",
            UserWarning,
            stacklevel=3,
        )


def autodiff_try(plan, model, V, estimate, kind):
    if plan is None or V is None or not is_autodiff_enabled():
        return None

    lowered = (
        lower_predictions(plan, model)
        if kind == "predictions"
        else lower_comparisons(plan, model)
    )
    if not lowered.ok:
        _warn_unsupported(lowered.reason)
        return None

    try:
        from . import pipeline

        result = pipeline.compute(beta=model.get_coef(), **lowered.kwargs)
    except Exception as exc:
        warnings.warn(
            "Automatic differentiation failed "
            f"({exc}). Reverting to finite differences.",
            UserWarning,
            stacklevel=3,
        )
        return None

    estimate = np.asarray(estimate, dtype=float).reshape(-1)
    if not np.allclose(result["estimate"], estimate, rtol=1e-8, atol=1e-8):
        warnings.warn(
            "Automatic differentiation estimates did not match the standard "
            "pipeline. Reverting to finite differences.",
            UserWarning,
            stacklevel=3,
        )
        return None

    # Coef/vcov positional alignment is guaranteed by each adapter vault.
    J = np.asarray(result["jacobian"], dtype=float)
    se = get_se(J, V)
    se[se == 0] = np.nan
    return AutodiffResult(std_error=se, jacobian=J)
