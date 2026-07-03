from collections import defaultdict, deque

import numpy as np
import polars as pl

from .by import get_by, get_by_groups
from .test import get_hypothesis
from .uncertainty import get_se, add_standard_errors
from .classes import MarginaleffectsResult
from .utils import prepare_base_inputs, finalize_result, call_avg
from .sanitize import handle_deprecated_hypotheses_argument
from .docstrings import doc


def _prepare_newdata(newdata, modeldata, variables):
    if not variables:
        return newdata, []

    if isinstance(variables, str):
        normalized = {variables: None}
    elif isinstance(variables, list):
        for v in variables:
            if not isinstance(v, str):
                raise TypeError("All entries in the `variables` list must be strings.")
        normalized = {v: None for v in variables}
    elif isinstance(variables, dict):
        normalized = variables
    else:
        raise TypeError("`variables` argument must be a dictionary")

    if "rowid" in newdata.columns and "rowidcf" not in newdata.columns:
        newdata = newdata.rename({"rowid": "rowidcf"})

    for variable, spec in normalized.items():
        if callable(spec):
            val = spec()
        elif spec is None:
            val = modeldata[variable].unique()
        elif spec == "sd":
            std = modeldata[variable].std()
            mean = modeldata[variable].mean()
            val = [mean - std / 2, mean + std / 2]
        elif spec == "2sd":
            std = modeldata[variable].std()
            mean = modeldata[variable].mean()
            val = [mean - std, mean + std]
        elif spec == "iqr":
            val = [
                np.percentile(newdata[variable], 75),
                np.percentile(newdata[variable], 25),
            ]
        elif spec == "minmax":
            val = [np.max(newdata[variable]), np.min(newdata[variable])]
        elif spec == "threenum":
            std = modeldata[variable].std()
            mean = modeldata[variable].mean()
            val = [mean - std / 2, mean, mean + std / 2]
        elif spec == "fivenum":
            val = np.percentile(
                modeldata[variable], [0, 25, 50, 75, 100], method="midpoint"
            )
        else:
            val = spec

        newdata = newdata.drop(variable)
        newdata = newdata.join(pl.DataFrame({variable: val}), how="cross")
        newdata = newdata.sort(variable)

    if "rowidcf" in newdata.columns:
        newdata = newdata.with_columns(
            pl.Series(range(newdata.height), dtype=pl.Int32).alias("rowid")
        )

    return newdata, list(normalized.keys())


def _prediction_group_info(newdata, by):
    id_col = "rowidcf" if "rowidcf" in newdata.columns else "rowid"

    if (
        isinstance(by, list)
        and len(by) == 1
        and by[0] == "group"
        and "group" not in newdata.columns
    ):
        return None, [tuple(newdata[id_col].to_list())], id_col

    by_cols = list(by) if isinstance(by, list) else []
    if "group" in newdata.columns and "group" not in by_cols:
        by_cols = ["group"] + by_cols
    by_cols = list(dict.fromkeys(col for col in by_cols if col in newdata.columns))

    if not by_cols:
        row_groups = [tuple([rid]) for rid in newdata[id_col].to_list()]
        return newdata.drop("rowid"), row_groups, id_col

    grouped = (
        newdata.with_columns(pl.lit(0.0).alias("estimate"))
        .group_by(by_cols, maintain_order=True)
        .agg(
            pl.col("estimate").mean().alias("estimate"),
            pl.col(id_col).alias("_rowids"),
        )
    )

    should_sort = any(
        grouped[col].dtype == pl.Enum for col in by_cols if col in grouped.columns
    )
    if should_sort:
        grouped = grouped.sort(by_cols)

    row_groups = [
        tuple(row_id for row_id in row_ids) for row_ids in grouped["_rowids"].to_list()
    ]
    metadata = grouped.drop(["estimate", "_rowids"])
    return metadata, row_groups, id_col


def _groups_from_row_groups(newdata, row_groups, id_col):
    positions = defaultdict(deque)
    for idx, row_id in enumerate(newdata[id_col].to_list()):
        positions[int(row_id)].append(idx)

    groups = np.full(newdata.height, -1, dtype=np.int32)
    for group_id, row_group in enumerate(row_groups):
        for row_id in row_group:
            row_positions = positions[int(row_id)]
            if not row_positions:
                raise ValueError("Mismatch between group rows and prediction rows.")
            groups[row_positions.popleft()] = group_id

    if np.any(groups < 0):
        raise ValueError("Failed to assign all prediction rows to groups.")

    return groups


def _predictions_jax(
    model,
    exog,
    newdata,
    by,
    wts,
    hypothesis,
    V,
):
    from .autodiff.dispatch import try_jax_predictions

    groups = None
    num_groups = None
    metadata = None

    if by is not False:
        metadata, row_groups, id_col = _prediction_group_info(newdata, by)
        if not row_groups:
            return None, None

        groups = _groups_from_row_groups(newdata, row_groups, id_col)
        num_groups = len(row_groups)

    jax_result = try_jax_predictions(
        model=model,
        exog=exog,
        vcov=V,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
        groups=groups,
        num_groups=num_groups,
    )

    if jax_result is None:
        return None, None

    try:
        if jax_result.get("aggregation") == "grouped":
            J = np.atleast_2d(jax_result["jacobian"])
            estimate = np.atleast_1d(jax_result["estimate"])
            std_error = np.atleast_1d(jax_result["std_error"])
            if metadata is None:
                out = pl.DataFrame({"estimate": estimate, "std_error": std_error})
            else:
                out = metadata.with_columns(
                    pl.Series(estimate).alias("estimate"),
                    pl.Series(std_error).alias("std_error"),
                )
            return out, J

        base = pl.DataFrame(
            {
                "rowid": newdata["rowid"],
                "estimate": jax_result["estimate"],
            }
        )
        cols = [x for x in newdata.columns if x not in base.columns]
        base = pl.concat([base, newdata.select(cols)], how="horizontal")

        if by is False:
            J = jax_result["jacobian"]
            out = base.with_columns(
                pl.Series(jax_result["std_error"]).alias("std_error")
            )
        else:
            grouped, row_groups = get_by_groups(
                model, base, newdata=newdata, by=by, wts=wts
            )
            if not row_groups:
                raise ValueError("Failed to compute autodiff groups for `by`.")

            rowid_lookup = {
                int(rid): idx for idx, rid in enumerate(base["rowid"].to_list())
            }
            jac_rows = []
            for group in row_groups:
                idxs = [rowid_lookup[rid] for rid in group if rid in rowid_lookup]
                if not idxs:
                    raise ValueError("Mismatch between group rows and Jacobian rows.")
                jac_rows.append(np.mean(jax_result["jacobian"][idxs], axis=0))

            J = np.vstack(jac_rows)
            se = get_se(J, V)
            out = grouped.with_columns(pl.Series(se).alias("std_error"))

        return out, J
    except Exception:
        return None, None


def _predictions_fd(
    model,
    exog,
    newdata,
    by,
    wts,
    hypothesis,
    V,
    eps_vcov,
):
    def inner(x):
        out = model.get_predict(params=np.array(x), newdata=exog)

        if out.shape[0] == newdata.shape[0]:
            cols = [x for x in newdata.columns if x not in out.columns]
            out = pl.concat([out, newdata.select(cols)], how="horizontal")

        elif "group" in out.columns:
            meta = newdata.join(out.select("group").unique(), how="cross")
            cols = [x for x in meta.columns if x in out.columns]
            out = meta.join(out, on=cols, how="left")

        else:
            raise ValueError("Something went wrong")

        out = get_by(model, out, newdata=newdata, by=by, wts=wts)
        out = get_hypothesis(out, hypothesis=hypothesis, by=by)
        return out

    out = inner(model.get_coef())
    return add_standard_errors(out, inner, model, V, eps_vcov)


@doc("""
# `predictions()`

`predictions()` and `avg_predictions()` predict outcomes using a fitted model on a specified scale for given combinations of values of predictor variables, such as their observed values, means, or factor levels (reference grid).

- `predictions()`: unit-level (conditional) estimates.
- `avg_predictions()`: average (marginal) estimates.

See the package website and vignette for examples:

- https://marginaleffects.com/chapters/predictions.html
- https://marginaleffects.com

## Parameters

{param_model}

{param_variables_prediction}

{param_newdata_prediction}

{param_by}

{param_transform}

{param_hypothesis}

{param_wts}

{param_vcov}

{param_equivalence}

{param_conf_level}

{param_eps_vcov}

{returns}

## Examples
```py
from marginaleffects import *

import statsmodels.api as sm
import statsmodels.formula.api as smf
data = get_dataset("thornton")

mod = smf.ols("outcome ~ incentive + distance", data).fit()

predictions(mod)

avg_predictions(mod)

predictions(mod, by = "village")

avg_predictions(mod, by = "village")

predictions(mod, hypothesis = 3)

avg_predictions(mod, hypothesis = 3)
```

## Details
{details_tost}

{details_order_of_operations}""")
def predictions(
    model,
    variables=None,
    conf_level=0.95,
    vcov=True,
    by=False,
    newdata=None,
    hypothesis=None,
    equivalence=None,
    transform=None,
    wts=None,
    eps_vcov=None,
    **kwargs,
) -> MarginaleffectsResult:
    hypothesis = handle_deprecated_hypotheses_argument(hypothesis, kwargs, stacklevel=2)
    if kwargs:
        unexpected = ", ".join(sorted(kwargs.keys()))
        raise TypeError(
            f"predictions() got unexpected keyword argument(s): {unexpected}"
        )
    (
        model,
        by,
        V,
        newdata,
        hypothesis_null,
        modeldata,
    ) = prepare_base_inputs(
        model=model,
        vcov=vcov,
        by=by,
        newdata=newdata,
        wts=wts,
        hypothesis=hypothesis,
    )

    newdata, datagrid = _prepare_newdata(newdata, modeldata, variables)
    if datagrid:
        newdata.datagrid_explicit = datagrid

    exog = model.get_exog(newdata)

    out, J = _predictions_jax(
        model=model,
        exog=exog,
        newdata=newdata,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
        V=V,
    )

    if out is None:
        out, J = _predictions_fd(
            model=model,
            exog=exog,
            newdata=newdata,
            by=by,
            wts=wts,
            hypothesis=hypothesis,
            V=V,
            eps_vcov=eps_vcov,
        )

    return finalize_result(
        out,
        model=model,
        by=by,
        transform=transform,
        equivalence=equivalence,
        newdata=newdata,
        conf_level=conf_level,
        J=J,
        hypothesis_null=hypothesis_null,
    )


def avg_predictions(
    model,
    variables=None,
    conf_level=0.95,
    vcov=True,
    by=True,
    newdata=None,
    hypothesis=None,
    equivalence=None,
    transform=None,
    wts=None,
    **kwargs,
) -> MarginaleffectsResult:
    return call_avg(
        predictions,
        model=model,
        newdata=newdata,
        variables=variables,
        conf_level=conf_level,
        vcov=vcov,
        by=by,
        hypothesis=hypothesis,
        equivalence=equivalence,
        transform=transform,
        wts=wts,
        **kwargs,
    )


avg_predictions.__doc__ = predictions.__doc__
