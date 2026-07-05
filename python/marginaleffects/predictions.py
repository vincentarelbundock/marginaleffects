import numpy as np
import polars as pl

from .autodiff.lower import autodiff_try
from .by import get_by_plan
from .hypothesis_compile import hypothesis_compile
from .uncertainty import get_jacobian, get_se
from .classes import MarginaleffectsResult
from .plan import (
    PredictionPlan,
    plan_values_allclose,
    prediction_plan_apply,
    prediction_plan_predict,
)
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
    out, plan = _predictions_build(
        model=model,
        exog=exog,
        newdata=newdata,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
    )
    if V is None:
        return out, None

    J = get_jacobian(
        lambda beta: prediction_plan_apply(
            plan, prediction_plan_predict(plan, model, beta)
        ),
        model.get_coef(),
        eps_vcov,
    )
    se = get_se(J, V)
    return out.with_columns(pl.Series(se).alias("std_error")), J


def _predictions_build(model, exog, newdata, by, wts, hypothesis):
    raw = model.get_predict(params=np.asarray(model.get_coef()), newdata=exog)
    n_pred = raw.shape[0]
    align = None

    if raw.shape[0] == newdata.shape[0]:
        cols = [x for x in newdata.columns if x not in raw.columns]
        out = pl.concat([raw, newdata.select(cols)], how="horizontal_extend")

    elif "group" in raw.columns:
        raw = raw.with_columns(
            pl.Series("_pred_row", range(raw.height), dtype=pl.Int32)
        )
        meta = newdata.join(raw.select("group").unique(), how="cross")
        cols = [x for x in meta.columns if x in raw.columns]
        out = meta.join(raw, on=cols, how="left")
        align = out["_pred_row"].fill_null(-1).cast(pl.Int64).to_numpy()
        out = out.drop("_pred_row")

    else:
        raise ValueError("Something went wrong")

    aligned_baseline = out["estimate"].to_numpy()
    has_na = np.isnan(np.asarray(aligned_baseline, dtype=float)).any()

    out, agg = get_by_plan(model, out, newdata=newdata, by=by, wts=wts)
    out, hyp = hypothesis_compile(out, hypothesis=hypothesis, by=by)
    plan = PredictionPlan(
        n_pred=n_pred,
        exog=exog,
        align=align,
        has_na=bool(has_na),
        agg=agg,
        hyp=hyp,
        n_out=out.height,
    )

    replay = prediction_plan_apply(
        plan, prediction_plan_predict(plan, model, model.get_coef())
    )
    if not plan_values_allclose(
        replay,
        out["estimate"].to_numpy(),
    ):
        raise RuntimeError(
            "marginaleffects internal error: prediction plan baseline check failed"
        )
    return out, plan


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

    out, plan = _predictions_build(
        model=model,
        exog=exog,
        newdata=newdata,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
    )

    J = None
    if V is not None:
        ad = autodiff_try(
            plan=plan,
            model=model,
            V=V,
            estimate=out["estimate"].to_numpy(),
            kind="predictions",
        )
        if ad is not None:
            J = ad.jacobian
            out = out.with_columns(pl.Series(ad.std_error).alias("std_error"))
        else:
            J = get_jacobian(
                lambda beta: prediction_plan_apply(
                    plan, prediction_plan_predict(plan, model, beta)
                ),
                model.get_coef(),
                eps_vcov,
            )
            se = get_se(J, V)
            out = out.with_columns(pl.Series(se).alias("std_error"))

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
