import numpy as np
import polars as pl

from .by import get_by, get_by_groups
from .hypothesis import get_hypothesis
from .uncertainty import get_jacobian, get_se, get_z_p_ci
from .pyfixest import ModelPyfixest
from .linearmodels import ModelLinearmodels
from .formulaic_utils import model_matrices
from ._input_utils import prepare_base_inputs
from .utils import finalize_result, call_avg
from warnings import warn
from .docs import (
    DocsDetails,
    DocsParameters,
    docstring_returns,
)


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

    return newdata, list(normalized.keys())


def _prepare_exog(model, newdata):
    if isinstance(model, ModelPyfixest):
        return newdata.to_pandas()
    if isinstance(model, ModelLinearmodels):
        return newdata
    if hasattr(model, "design_info_patsy"):
        f = model.design_info_patsy
    else:
        f = model.get_formula()

    if callable(f):
        _, exog = f(newdata)
    else:
        _, exog = model_matrices(f, newdata, formula_engine=model.get_formula_engine())

    return exog


def _predictions_jax(
    model,
    exog,
    newdata,
    by,
    wts,
    hypothesis,
    V,
    conf_level,
    hypothesis_null,
):
    from .jax_dispatch import try_jax_predictions

    jax_result = try_jax_predictions(
        model=model,
        exog=exog,
        vcov=V,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
    )

    if jax_result is None:
        return None, None

    try:
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

        out = get_z_p_ci(
            out, model, conf_level=conf_level, hypothesis_null=hypothesis_null
        )
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
    conf_level,
    hypothesis_null,
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

    if V is not None:
        J = get_jacobian(inner, model.get_coef(), eps_vcov=eps_vcov)
        se = get_se(J, V)
        out = out.with_columns(pl.Series(se).alias("std_error"))
        out = get_z_p_ci(
            out, model, conf_level=conf_level, hypothesis_null=hypothesis_null
        )
    else:
        J = None

    return out, J


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
):
    """
    `predictions()` and `avg_predictions()` predict outcomes using a fitted model on a specified scale for given combinations of values of predictor variables, such as their observed values, means, or factor levels (reference grid).

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(predictions)`
    """
    if "hypotheses" in kwargs:
        if hypothesis is not None:
            raise ValueError("Specify at most one of `hypothesis` or `hypotheses`.")
        hypotheses = kwargs.pop("hypotheses")
        warn(
            "`hypotheses` is deprecated; use `hypothesis` instead.",
            DeprecationWarning,
            stacklevel=2,
        )
        hypothesis = hypotheses
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

    exog = _prepare_exog(model, newdata)

    out, J = _predictions_jax(
        model=model,
        exog=exog,
        newdata=newdata,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
        V=V,
        conf_level=conf_level,
        hypothesis_null=hypothesis_null,
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
            conf_level=conf_level,
            hypothesis_null=hypothesis_null,
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
):
    """
    `predictions()` and `avg_predictions()` predict outcomes using a fitted model on a specified scale for given combinations of values of predictor variables, such as their observed values, means, or factor levels (reference grid).

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(avg_predictions)`
    """
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


docs_predictions = (
    """
# `predictions()`

`predictions()` and `avg_predictions()` predict outcomes using a fitted model on a specified scale for given combinations of values of predictor variables, such as their observed values, means, or factor levels (reference grid).
    
* `predictions()`: unit-level (conditional) estimates.
* `avg_predictions()`: average (marginal) estimates.

See the package website and vignette for examples:

- https://marginaleffects.com/chapters/predictions.html
- https://marginaleffects.com

## Parameters
"""
    + DocsParameters.docstring_model
    + DocsParameters.docstring_variables("prediction")
    + DocsParameters.docstring_newdata("prediction")
    + DocsParameters.docstring_by
    + DocsParameters.docstring_transform
    + DocsParameters.docstring_hypothesis
    + DocsParameters.docstring_wts
    + DocsParameters.docstring_vcov
    + DocsParameters.docstring_equivalence
    + DocsParameters.docstring_conf_level
    + DocsParameters.docstring_eps_vcov
    + docstring_returns
    + """ 
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
"""
    + DocsDetails.docstring_tost
    + DocsDetails.docstring_order_of_operations
)


predictions.__doc__ = docs_predictions

avg_predictions.__doc__ = predictions.__doc__
