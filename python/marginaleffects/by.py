import polars as pl
from typing import List, Optional

from .plan import AggGroup


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
) -> tuple[pl.DataFrame, Optional[List[AggGroup]]]:
    # for predictions
    if (
        isinstance(by, list)
        and len(by) == 1
        and by[0] == "group"
        and "group" not in estimand.columns
    ):
        by = True

    agg_groups: Optional[List[AggGroup]] = None

    if by is True:
        result = estimand.select(["estimate"]).mean()
        if return_plan_groups:
            agg_groups = [
                AggGroup(idx=pl.Series(range(estimand.height)).to_numpy(), w=None)
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

    agg_exprs: List[pl.Expr] = []
    if wts is None:
        agg_exprs.append(pl.col("estimate").mean().alias("estimate"))
    else:
        agg_exprs.append(
            ((pl.col("estimate") * pl.col(wts)).sum() / pl.col(wts).sum()).alias(
                "estimate"
            )
        )
    if return_plan_groups and "_marginaleffects_pos" in out.columns:
        agg_exprs.append(pl.col("_marginaleffects_pos").alias("_positions"))
        if wts is not None:
            agg_exprs.append(pl.col(wts).alias("_plan_weights"))

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
