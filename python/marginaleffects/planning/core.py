"""Core estimation plan types and replay operations."""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field
from typing import Any

import numpy as np
import polars as pl

__all__ = [
    "AggBlock",
    "AggGroup",
    "AggPlan",
    "CompGroup",
    "ComparisonPlan",
    "Hyp",
    "PredictionPlan",
    "Stages",
    "apply_plan_aggregation",
    "comparison_plan_apply",
    "comparison_plan_apply_stages",
    "comparison_plan_predict",
    "compile_agg_blocks",
    "plan_values_allclose",
    "prediction_plan_apply",
    "prediction_plan_apply_stages",
    "prediction_plan_predict",
]


@dataclass
class AggGroup:
    """One aggregate row: the source rows it averages, and their weights."""

    idx: np.ndarray
    w: np.ndarray | None


@dataclass(frozen=True)
class AggBlock:
    """Equal-length aggregate rows stacked column-wise for vectorized replay.

    `idx` and `w` are (group_length, n_groups) matrices, so a whole block is
    reduced with one column-wise sum instead of a Python loop per group.
    """

    cols: np.ndarray
    idx: np.ndarray
    w: np.ndarray | None


@dataclass(frozen=True)
class AggPlan:
    """Recorded aggregation stage: which source rows feed which output row."""

    blocks: tuple[AggBlock, ...]
    n: int
    weighted: bool


def compile_agg_blocks(groups) -> AggPlan | None:
    """Stack equal-length aggregate rows so replay avoids ragged padding."""
    if groups is None:
        return None
    if isinstance(groups, AggPlan):
        return groups
    groups = list(groups)
    if not groups:
        return None

    weighted = any(group.w is not None for group in groups)
    lengths = np.asarray([np.asarray(group.idx).size for group in groups])

    def make_block(cols):
        length = int(lengths[cols[0]])
        idx = np.stack(
            [np.asarray(groups[col].idx, dtype=int).reshape(-1) for col in cols],
            axis=1,
        )
        w = None
        if weighted:
            w = np.stack(
                [
                    (
                        np.ones(length, dtype=float)
                        if groups[col].w is None
                        else np.asarray(groups[col].w, dtype=float).reshape(-1)
                    )
                    for col in cols
                ],
                axis=1,
            )
        return AggBlock(cols=np.asarray(cols, dtype=int), idx=idx, w=w)

    order = np.arange(len(groups))
    if np.unique(lengths).size == 1:
        blocks = (make_block(order),)
    else:
        blocks = tuple(
            make_block(order[lengths == length]) for length in np.unique(lengths)
        )
    return AggPlan(blocks=blocks, n=len(groups), weighted=weighted)


def apply_plan_aggregation(agg, est) -> np.ndarray:
    """Replay a recorded aggregation stage on a vector of estimates.

    Missing-value handling mirrors the R implementation: an unweighted mean
    drops missing entries, a weighted mean zeroes both the estimate and its
    weight wherever the estimate is missing, and a missing weight propagates
    to the aggregate rather than silently dropping the row it belongs to.
    """
    agg = compile_agg_blocks(agg)
    if agg is None:
        return _as_float_array(est)

    est = _as_float_array(est)
    out = np.empty(agg.n, dtype=float)
    for block in agg.blocks:
        e = est[block.idx]
        if agg.weighted:
            w = np.array(block.w, dtype=float)
            missing = np.isnan(e)
            e = np.where(missing, 0.0, e)
            w = np.where(missing, 0.0, w)
            # 0 * Inf is NaN, so a zero weight must blank the estimate it
            # multiplies rather than poisoning the whole group.
            e = np.where(~np.isnan(w) & (w == 0), 0.0, e)
            with np.errstate(invalid="ignore", divide="ignore"):
                out[block.cols] = (e * w).sum(axis=0) / w.sum(axis=0)
        else:
            keep = ~np.isnan(e)
            counts = keep.sum(axis=0)
            totals = np.where(keep, e, 0.0).sum(axis=0)
            out[block.cols] = np.divide(
                totals,
                counts,
                out=np.full(counts.shape, np.nan, dtype=float),
                where=counts > 0,
            )
    return out


@dataclass
class Hyp:
    kind: str
    apply: Callable[[np.ndarray], np.ndarray]
    H: np.ndarray | None = None


@dataclass(frozen=True)
class Stages:
    pre: np.ndarray
    post: np.ndarray


@dataclass
class PredictionPlan:
    n_pred: int
    exog: Any
    align: np.ndarray | None
    has_na: bool
    agg: list[AggGroup] | None
    hyp: Hyp | None
    n_out: int
    rowid: np.ndarray | None = None
    source: Any | None = None
    agg_blocks: AggPlan | None = field(default=None, repr=False, compare=False)

    def __post_init__(self):
        self.agg_blocks = compile_agg_blocks(self.agg)


@dataclass
class CompGroup:
    idx: np.ndarray
    out_idx: np.ndarray
    scalar: bool
    fun: Callable
    fun_key: str | None
    x: np.ndarray | None
    w: np.ndarray | None
    uses_y: bool = False


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
    agg: list[AggGroup] | None = None
    n_out: int | None = None
    rowid: np.ndarray | None = None
    source: Any | None = None
    agg_blocks: AggPlan | None = field(default=None, repr=False, compare=False)

    def __post_init__(self):
        self.agg_blocks = compile_agg_blocks(self.agg)
        if self.n_out is None:
            self.n_out = self.n_comp if self.agg_blocks is None else self.agg_blocks.n


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
    denominator = np.sum(weights[mask])
    if denominator == 0:
        return np.nan
    return float(np.sum(values[mask] * weights[mask]) / denominator)


def _series(x, name: str | None = None) -> pl.Series:
    if x is None:
        return None
    return pl.Series(name or "", np.asarray(x).reshape(-1))


def _estimand_to_numpy(x) -> np.ndarray:
    if isinstance(x, pl.Series):
        return _as_float_array(x.to_numpy())
    return _as_float_array(x)


def _weighted_mean(values, weights):
    return _nan_weighted_mean(values, weights)


def _builtin_comparison(key, hi, lo, eps, x, y, w):
    """NumPy fast paths used repeatedly during Jacobian replay."""
    delta = hi - lo
    if key == "difference":
        return delta
    if key == "ratio":
        return hi / lo
    if key == "lnratio":
        return np.log(hi / lo)
    if key == "lnor":
        return np.log((hi / (1 - hi)) / (lo / (1 - lo)))
    if key == "lift":
        return delta / lo
    if key == "dydx":
        return delta / eps
    if key == "eyex":
        return delta / eps * (x / y)
    if key == "eydx":
        return (delta / eps) / y
    if key == "dyex":
        return delta / eps * x

    weighted = key.endswith("avgwts")
    weights = w if weighted else None
    base = key.removesuffix("wts") if weighted else key
    if base == "differenceavg":
        return np.asarray([_weighted_mean(hi, weights) - _weighted_mean(lo, weights)])
    if base == "ratioavg":
        return np.asarray([_weighted_mean(hi, weights) / _weighted_mean(lo, weights)])
    if base == "lnratioavg":
        return np.asarray(
            [np.log(_weighted_mean(hi, weights) / _weighted_mean(lo, weights))]
        )
    if base == "lnoravg":
        hi_mean = _weighted_mean(hi, weights)
        lo_mean = _weighted_mean(lo, weights)
        return np.asarray(
            [np.log((hi_mean / (1 - hi_mean)) / (lo_mean / (1 - lo_mean)))]
        )
    if base == "liftavg":
        hi_mean = _weighted_mean(hi, weights)
        lo_mean = _weighted_mean(lo, weights)
        return np.asarray([(hi_mean - lo_mean) / lo_mean])

    slope = base.removesuffix("avg")
    if slope == "dydx":
        values = delta / eps
    elif slope == "eyex":
        values = delta / eps * (x / y)
    elif slope == "eydx":
        values = (delta / eps) / y
    elif slope == "dyex":
        values = delta / eps * x
    else:
        return None
    return np.asarray([_weighted_mean(values, weights)])


def plan_values_allclose(replay, target) -> bool:
    """Compare plan replay values to baseline values at backend precision."""
    replay = np.asarray(replay)
    target = np.asarray(target)
    tol = 1e-8
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
    return prediction_plan_apply_stages(plan, pred).post


def prediction_plan_apply_stages(plan: PredictionPlan, pred) -> Stages:
    est = _apply_align(pred, plan.align)

    if plan.agg is not None:
        est = apply_plan_aggregation(plan.agg_blocks or plan.agg, est)

    pre = est
    if plan.hyp is not None:
        est = _as_float_array(plan.hyp.apply(est))

    if est.shape[0] != plan.n_out:
        raise RuntimeError(
            "marginaleffects internal error: prediction plan replay changed shape"
        )
    return Stages(pre=pre, post=est)


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
    return comparison_plan_apply_stages(plan, hi, lo, y).post


def comparison_plan_apply_stages(plan: ComparisonPlan, hi, lo, y=None) -> Stages:
    hi = _apply_align(hi, plan.align)
    lo = _apply_align(lo, plan.align)
    y = None if y is None else _apply_align(y, plan.align)

    out = np.empty(plan.n_comp, dtype=float)
    for group in plan.groups:
        idx = np.asarray(group.idx, dtype=int)
        yi = None if y is None else y[idx]
        est = None
        if group.fun_key is not None:
            est = _builtin_comparison(
                group.fun_key, hi[idx], lo[idx], plan.eps, group.x, yi, group.w
            )
        if est is None:
            est = group.fun(
                hi=_series(hi[idx], "predicted_hi"),
                lo=_series(lo[idx], "predicted_lo"),
                eps=plan.eps,
                x=_series(group.x, "x"),
                y=None if yi is None else _series(yi, "predicted"),
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

    if plan.agg is not None:
        out = apply_plan_aggregation(plan.agg_blocks or plan.agg, out)

    pre = out
    if plan.hyp is not None:
        out = _as_float_array(plan.hyp.apply(out))

    if plan.n_out is not None and out.shape[0] != plan.n_out:
        raise RuntimeError(
            "marginaleffects internal error: comparison plan replay changed shape"
        )
    return Stages(pre=pre, post=out)
