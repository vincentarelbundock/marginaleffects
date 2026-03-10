import polars as pl
import numpy as np
from typing import List, Optional, Tuple


def get_by(model, estimand, newdata, by=None, wts=None):
    out, _ = _get_by_internal(
        model=model,
        estimand=estimand,
        newdata=newdata,
        by=by,
        wts=wts,
        return_groups=False,
    )
    return out


def get_by_groups(model, estimand, newdata, by=None, wts=None):
    """
    Return grouped estimates plus the rowids contributing to each group.
    """
    return _get_by_internal(
        model=model,
        estimand=estimand,
        newdata=newdata,
        by=by,
        wts=wts,
        return_groups=True,
    )


def _get_by_internal(
    model,
    estimand,
    newdata,
    by=None,
    wts=None,
    return_groups: bool = False,
) -> Tuple[pl.DataFrame, Optional[List[Tuple[int, ...]]]]:
    # for predictions
    if (
        isinstance(by, list)
        and len(by) == 1
        and by[0] == "group"
        and "group" not in estimand.columns
    ):
        by = True

    row_groups: Optional[List[Tuple[int, ...]]] = None

    if by is True:
        result = estimand.select(["estimate"]).mean()
        if return_groups and "rowid" in estimand.columns:
            row_groups = [tuple(int(x) for x in estimand["rowid"].to_list())]
        return result, row_groups
    elif by is False:
        if return_groups and "rowid" in estimand.columns:
            row_groups = [tuple([int(x)]) for x in estimand["rowid"].to_list()]
        return estimand, row_groups

    if "group" in estimand.columns:
        by = ["group"] + by

    if "rowid" in estimand.columns and "rowid" in newdata.columns:
        out = estimand.join(newdata, on="rowid", how="left")
    else:
        out = pl.DataFrame({"estimate": estimand["estimate"]})

    by = [x for x in by if x in out.columns]
    by = np.unique(by)

    if isinstance(by, list) and len(by) == 0:
        if return_groups and "rowid" in out.columns:
            row_groups = [tuple([int(x)]) for x in out["rowid"].to_list()]
        return out, row_groups

    agg_exprs: List[pl.Expr] = []
    if wts is None:
        agg_exprs.append(pl.col("estimate").mean().alias("estimate"))
    else:
        agg_exprs.append(
            ((pl.col("estimate") * pl.col(wts)).sum() / pl.col(wts).sum()).alias(
                "estimate"
            )
        )
    if return_groups and "rowid" in out.columns:
        agg_exprs.append(pl.col("rowid").alias("_rowids"))

    out = out.group_by(by, maintain_order=True).agg(agg_exprs)

    if return_groups and "_rowids" in out.columns:
        row_groups = [
            tuple(int(r) for r in row_ids) for row_ids in out["_rowids"].to_list()
        ]
        out = out.drop("_rowids")

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

    return out, row_groups
