from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable

import numpy as np
import polars as pl


@dataclass
class AggGroup:
    idx: np.ndarray
    w: np.ndarray | None


@dataclass
class Hyp:
    kind: str
    apply: Callable[[np.ndarray], np.ndarray]
    H: np.ndarray | None = None


@dataclass
class PredictionPlan:
    n_pred: int
    exog: Any
    align: np.ndarray | None
    has_na: bool
    agg: list[AggGroup] | None
    hyp: Hyp | None
    n_out: int


@dataclass
class CompGroup:
    idx: np.ndarray
    out_idx: np.ndarray
    scalar: bool
    fun: Callable
    fun_key: str | None
    x: np.ndarray | None
    w: np.ndarray | None


@dataclass
class ComparisonPlan:
    n_pred: int
    exog_hi: Any
    exog_lo: Any
    exog_nd: Any | None
    need_y: bool
    align: np.ndarray | None
    eps: float | None
    groups: list[CompGroup]
    n_comp: int
    hyp: Hyp | None
    has_na: bool = False


def _as_float_array(x) -> np.ndarray:
    return np.asarray(x, dtype=float).reshape(-1)


def _apply_align(values: np.ndarray, align: np.ndarray | None) -> np.ndarray:
    values = _as_float_array(values)
    if align is None:
        return values

    align = np.asarray(align, dtype=int)
    out = np.full(align.shape[0], np.nan, dtype=float)
    keep = align >= 0
    out[keep] = values[align[keep]]
    return out


def _nan_weighted_mean(values: np.ndarray, weights: np.ndarray | None) -> float:
    values = _as_float_array(values)
    mask = ~np.isnan(values)
    if weights is not None:
        weights = _as_float_array(weights)
        mask = mask & ~np.isnan(weights)
    if not np.any(mask):
        return np.nan
    if weights is None:
        return float(np.mean(values[mask]))
    return float(np.sum(values[mask] * weights[mask]) / np.sum(weights[mask]))


def _series(x, name: str | None = None) -> pl.Series:
    if x is None:
        return None
    return pl.Series(name or "", np.asarray(x).reshape(-1))


def _estimand_to_numpy(x) -> np.ndarray:
    if isinstance(x, pl.Series):
        return _as_float_array(x.to_numpy())
    return _as_float_array(x)


def plan_values_allclose(replay, target) -> bool:
    """Compare plan replay values to baseline values at backend precision."""
    replay = np.asarray(replay)
    target = np.asarray(target)
    tol = 1e-12
    if any(
        np.issubdtype(x.dtype, np.floating) and x.dtype.itemsize <= 4
        for x in (replay, target)
    ):
        tol = 1e-6
    return np.allclose(replay, target, rtol=tol, atol=tol, equal_nan=True)


def prediction_plan_predict(plan: PredictionPlan, model, coefs) -> np.ndarray:
    out = model.get_predict(params=np.asarray(coefs), newdata=plan.exog)
    return _as_float_array(out["estimate"].to_numpy())


def prediction_plan_apply(plan: PredictionPlan, pred) -> np.ndarray:
    est = _apply_align(pred, plan.align)

    if plan.agg is not None:
        est = np.asarray(
            [_nan_weighted_mean(est[group.idx], group.w) for group in plan.agg],
            dtype=float,
        )

    if plan.hyp is not None:
        est = _as_float_array(plan.hyp.apply(est))

    if est.shape[0] != plan.n_out:
        raise RuntimeError(
            "marginaleffects internal error: prediction plan replay changed shape"
        )
    return est


def comparison_plan_predict(plan: ComparisonPlan, model, coefs):
    coefs = np.asarray(coefs)
    hi = model.get_predict(params=coefs, newdata=plan.exog_hi)["estimate"].to_numpy()
    lo = model.get_predict(params=coefs, newdata=plan.exog_lo)["estimate"].to_numpy()
    y = None
    if plan.need_y and plan.exog_nd is not None:
        y = model.get_predict(params=coefs, newdata=plan.exog_nd)["estimate"].to_numpy()
    return (
        _as_float_array(hi),
        _as_float_array(lo),
        (None if y is None else _as_float_array(y)),
    )


def comparison_plan_apply(plan: ComparisonPlan, hi, lo, y=None) -> np.ndarray:
    hi = _apply_align(hi, plan.align)
    lo = _apply_align(lo, plan.align)
    y = None if y is None else _apply_align(y, plan.align)

    out = np.empty(plan.n_comp, dtype=float)
    for group in plan.groups:
        idx = np.asarray(group.idx, dtype=int)
        yi = None if y is None else _series(y[idx], "predicted")
        est = group.fun(
            hi=_series(hi[idx], "predicted_hi"),
            lo=_series(lo[idx], "predicted_lo"),
            eps=plan.eps,
            x=_series(group.x, "x"),
            y=yi,
            w=_series(group.w, "w"),
        )
        est = _estimand_to_numpy(est)
        if group.scalar and est.shape[0] != 1:
            raise RuntimeError(
                "marginaleffects internal error: comparison plan scalar group changed shape"
            )
        if not group.scalar and est.shape[0] != group.out_idx.shape[0]:
            raise RuntimeError(
                "marginaleffects internal error: comparison plan group changed shape"
            )
        out[group.out_idx] = est

    if plan.hyp is not None:
        out = _as_float_array(plan.hyp.apply(out))

    return out
