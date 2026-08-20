import warnings

import polars as pl

from .planning import AggGroup
from .sanitize.by import by_frame_keys, by_is_frame

BY_FRAME_COVERAGE_WARNING = (
    "The `by` data frame does not cover all combinations of response levels "
    "and/or predictors. Some estimates will not be included in the aggregation."
)


def _mean_expr() -> pl.Expr:
    """Unweighted group mean, dropping missing estimates as R's `na.rm` does.

    Polars propagates NaN through `mean()` while skipping nulls. Treating the
    two alike keeps this displayed estimate identical to the one the plan
    replay recomputes, which the replay baseline check requires.
    """
    return pl.col("estimate").fill_nan(None).mean().alias("estimate")


def _weighted_mean_expr(wts: str) -> pl.Expr:
    """Weighted group mean under R's missing-value rules.

    A missing estimate zeroes both itself and its weight, so it leaves the
    aggregate untouched. A zero weight blanks the estimate it multiplies,
    because `0 * Inf` is NaN and would otherwise poison the whole group. A
    missing weight propagates: dropping the row it belongs to would silently
    report a different estimand than the one requested.
    """
    est = pl.col("estimate")
    w = pl.col(wts).cast(pl.Float64).fill_null(float("nan"))
    missing = est.is_null() | est.is_nan()
    numerator = pl.when(missing | (w == 0)).then(0.0).otherwise(est * w).sum()
    denominator = pl.when(missing).then(0.0).otherwise(w).sum()
    return (numerator / denominator).alias("estimate")


def harmonize_by_types(estimates: pl.DataFrame, by: pl.DataFrame) -> pl.DataFrame:
    """Reconcile join-key dtypes between the estimates and a `by` data frame.

    A label table is usually typed by hand, so its keys routinely disagree with
    the estimates on numeric-versus-string. Joining on mismatched dtypes finds
    nothing and would silently drop every row, so the keys are cast to the
    estimates' side before the join rather than after it fails.
    """
    casts = []
    for column in by.columns:
        if column == "by" or column not in estimates.columns:
            continue
        target = estimates[column].dtype
        if by[column].dtype == target:
            continue
        if target.is_numeric() or target in (pl.String, pl.Utf8):
            casts.append(pl.col(column).cast(target, strict=False))
        else:
            casts.append(pl.col(column).cast(pl.String).cast(target, strict=False))
    return by.with_columns(casts) if casts else by


def attach_by_labels(estimates: pl.DataFrame, by: pl.DataFrame, verbose: bool = True):
    """Left-join a `by` data frame's labels onto the estimates.

    Returns the labelled estimates and a boolean mask of the rows the label
    table covers. Uncovered rows are dropped rather than aggregated under a
    missing label, matching R, and the caller is warned because a partial
    label table is far more often a typo than an intent.
    """
    keys = [x for x in by_frame_keys(by) if x in estimates.columns]
    if not keys:
        raise ValueError(
            "None of the columns in the `by` data frame match the estimates. "
            f"Available columns: {', '.join(estimates.columns)}."
        )
    labels = harmonize_by_types(estimates, by.select([*keys, "by"])).unique(subset=keys)
    if "by" in estimates.columns:
        estimates = estimates.drop("by")
    out = estimates.join(labels, on=keys, how="left")
    keep = out["by"].is_not_null()
    if verbose and not keep.all():
        warnings.warn(BY_FRAME_COVERAGE_WARNING, UserWarning, stacklevel=2)
    return out, keep


def get_by(model, estimand, newdata, by=None, wts=None):
    out, _ = _get_by_internal(
        model=model,
        estimand=estimand,
        newdata=newdata,
        by=by,
        wts=wts,
    )
    return out


def get_by_plan(model, estimand, newdata, by=None, wts=None):
    out, agg_groups = _get_by_internal(
        model=model,
        estimand=estimand,
        newdata=newdata,
        by=by,
        wts=wts,
        return_plan_groups=True,
    )
    return out, agg_groups


def _get_by_internal(
    model,
    estimand,
    newdata,
    by=None,
    wts=None,
    return_plan_groups: bool = False,
) -> tuple[pl.DataFrame, list[AggGroup] | None]:
    # for predictions
    if (
        isinstance(by, list)
        and len(by) == 1
        and by[0] == "group"
        and "group" not in estimand.columns
    ):
        by = True

    agg_groups: list[AggGroup] | None = None

    if by_is_frame(by):
        return _get_by_frame(
            estimand=estimand,
            newdata=newdata,
            by=by,
            wts=wts,
            return_plan_groups=return_plan_groups,
        )

    if by is True:
        # A grand mean must respect user weights exactly like the named-group
        # path below does; ignoring `wts` here would silently report a
        # different estimand than the one requested, with a Jacobian to match.
        w_arr = None
        if wts is not None:
            tmp = estimand
            if (
                wts not in tmp.columns
                and "rowid" in tmp.columns
                and "rowid" in newdata.columns
            ):
                # The recorded weights must line up with the Jacobian rows,
                # which follow the estimand's row order. A join does not
                # promise to preserve that order, so carry an explicit
                # position column through it and restore the order afterward.
                tmp = tmp.with_columns(
                    pl.Series(
                        "_marginaleffects_wpos",
                        range(tmp.height),
                        dtype=pl.Int32,
                    )
                )
                tmp = tmp.join(newdata.select(["rowid", wts]), on="rowid", how="left")
                tmp = tmp.sort("_marginaleffects_wpos").drop("_marginaleffects_wpos")
            if wts not in tmp.columns:
                raise ValueError(f"Weights column '{wts}' not found for aggregation.")
            if tmp.height != estimand.height:
                raise ValueError(
                    "Joining the weights column changed the number of rows; "
                    "`newdata` has duplicated `rowid` values."
                )
            if tmp[wts].null_count() > 0:
                raise ValueError(
                    f"Weights column '{wts}' has missing values after joining "
                    "on `rowid`."
                )
            result = tmp.select(_weighted_mean_expr(wts))
            w_arr = tmp[wts].cast(pl.Float64).to_numpy()
        else:
            result = estimand.select(_mean_expr())
        if return_plan_groups:
            agg_groups = [
                AggGroup(idx=pl.Series(range(estimand.height)).to_numpy(), w=w_arr)
            ]
        return result, agg_groups
    elif by is False:
        return estimand, agg_groups

    if "group" in estimand.columns:
        by = ["group"] + by

    if return_plan_groups:
        estimand = estimand.with_columns(
            pl.Series("_marginaleffects_pos", range(estimand.height), dtype=pl.Int32)
        )

    if "rowid" in estimand.columns and "rowid" in newdata.columns:
        out = estimand.join(newdata, on="rowid", how="left")
    else:
        out = pl.DataFrame({"estimate": estimand["estimate"]})

    by = list(dict.fromkeys(x for x in by if x in out.columns))

    if isinstance(by, list) and len(by) == 0:
        return out, agg_groups

    agg_exprs: list[pl.Expr] = []
    if wts is None:
        agg_exprs.append(_mean_expr())
    else:
        agg_exprs.append(_weighted_mean_expr(wts))
    if return_plan_groups and "_marginaleffects_pos" in out.columns:
        agg_exprs.append(pl.col("_marginaleffects_pos").alias("_positions"))
        if wts is not None:
            agg_exprs.append(pl.col(wts).cast(pl.Float64).alias("_plan_weights"))

    out = out.group_by(by, maintain_order=True).agg(agg_exprs)

    # Sort by 'by' columns ONLY if they are Enum type to ensure consistent categorical ordering
    # For Enum columns, sort() respects the enum order (not lexical order)
    # For other types (strings, numbers), maintain the group_by order to preserve existing behavior
    if isinstance(by, str):
        by_cols = [by]
    else:
        by_cols = list(by)

    should_sort = any(
        out[col].dtype == pl.Enum for col in by_cols if col in out.columns
    )
    if should_sort:
        out = out.sort(by)

    if return_plan_groups and "_positions" in out.columns:
        weights = (
            out["_plan_weights"].to_list()
            if wts is not None and "_plan_weights" in out.columns
            else None
        )
        agg_groups = []
        for i, positions in enumerate(out["_positions"].to_list()):
            w = None if weights is None else pl.Series(weights[i]).to_numpy()
            agg_groups.append(AggGroup(idx=pl.Series(positions).to_numpy(), w=w))
        drop_cols = ["_positions"]
        if "_plan_weights" in out.columns:
            drop_cols.append("_plan_weights")
        out = out.drop(drop_cols)

    return out, agg_groups


def _get_by_frame(estimand, newdata, by, wts, return_plan_groups: bool):
    """Aggregate estimates under labels supplied as a `by` data frame.

    The label table can map several predictor combinations onto one label, so
    grouping happens on the joined label rather than on the estimates' own
    columns. Row positions are recorded before the join because the plan's
    prediction vector is indexed by the *unfiltered* estimand order, which
    dropping uncovered rows would otherwise shift.
    """
    position = "_marginaleffects_pos"
    estimand = estimand.with_columns(
        pl.Series(position, range(estimand.height), dtype=pl.Int64)
    )

    keys = by_frame_keys(by)
    missing = [x for x in keys if x not in estimand.columns]
    if missing and "rowid" in estimand.columns and "rowid" in newdata.columns:
        payload = [x for x in missing if x in newdata.columns]
        if payload:
            estimand = estimand.join(
                newdata.select(["rowid", *payload]), on="rowid", how="left"
            )

    labelled, keep = attach_by_labels(estimand, by)
    labelled = labelled.filter(keep)

    group_cols = [x for x in ("term", "by") if x in labelled.columns]
    weighted = wts is not None and wts in labelled.columns
    agg_exprs = [_weighted_mean_expr(wts) if weighted else _mean_expr()]
    if return_plan_groups:
        agg_exprs.append(pl.col(position).alias("_positions"))
        if weighted:
            agg_exprs.append(pl.col(wts).cast(pl.Float64).alias("_plan_weights"))

    out = labelled.group_by(group_cols, maintain_order=True).agg(agg_exprs)

    agg_groups = None
    if return_plan_groups:
        weights = out["_plan_weights"].to_list() if weighted else None
        agg_groups = [
            AggGroup(
                idx=pl.Series(positions).to_numpy(),
                w=None if weights is None else pl.Series(weights[i]).to_numpy(),
            )
            for i, positions in enumerate(out["_positions"].to_list())
        ]
        out = out.drop([x for x in ("_positions", "_plan_weights") if x in out.columns])

    return out.select([*group_cols, "estimate"]), agg_groups
