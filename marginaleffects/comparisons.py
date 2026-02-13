import re
from functools import reduce

import numpy as np
import polars as pl

from .estimands import estimands
from .sanitize_model import sanitize_model
from .sanity import (
    sanitize_variables,
    handle_deprecated_hypotheses_argument,
    handle_pyfixest_vcov_limitation,
)
from .uncertainty import get_jacobian, get_se, get_z_p_ci
from .utils import (
    get_pad,
    upcast,
    validate_string_columns,
    finalize_result,
    call_avg,
)
from ._input_utils import prepare_base_inputs
from .docs import (
    DocsDetails,
    DocsParameters,
    docstring_returns,
)


def _cross_postprocess(cross):
    if not cross:
        return None

    def add_term(df):
        return df.with_columns(pl.lit("cross").alias("term"))

    return add_term


def _prepare_counterfactual_frames(
    model,
    modeldata,
    newdata,
    variables,
    comparison,
    eps,
    by,
    wts,
    cross,
):
    if cross and variables is None:
        raise ValueError(
            "The `variables` argument must be specified when `cross=True`."
        )

    variables = sanitize_variables(
        variables=variables,
        model=model,
        newdata=newdata,
        comparison=comparison,
        eps=eps,
        by=by,
        wts=wts,
        cross=cross,
    )

    nd_frames, hi_frames, lo_frames = _build_comparison_frames(
        newdata, variables, cross
    )
    pad_frames = _build_padding_frames(model, modeldata, newdata)
    nd, hi, lo, pad_rows = _finalize_counterfactual_frames(
        nd_frames,
        hi_frames,
        lo_frames,
        pad_frames,
        modeldata,
    )
    return variables, nd, hi, lo, pad_rows


def _build_comparison_frames(newdata, variables, cross):
    hi = []
    lo = []
    nd = []
    if not cross:
        for v in variables:
            vcomp = "custom" if callable(v.comparison) else v.comparison
            nd.append(
                newdata.with_columns(
                    pl.lit(v.variable).alias("term"),
                    pl.lit(v.lab).alias("contrast"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )
            )
            hi.append(
                newdata.with_columns(
                    pl.lit(v.hi).alias(v.variable),
                    pl.lit(v.variable).alias("term"),
                    pl.lit(v.lab).alias("contrast"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )
            )
            lo.append(
                newdata.with_columns(
                    pl.lit(v.lo).alias(v.variable),
                    pl.lit(v.variable).alias("term"),
                    pl.lit(v.lab).alias("contrast"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )
            )
    else:
        if variables and isinstance(variables[0].variable, tuple):
            for v in variables:
                vcomp = "custom" if callable(v.comparison) else v.comparison

                var_names = v.variable
                nd_row = newdata.clone()
                hi_row = newdata.clone()
                lo_row = newdata.clone()

                for k, var_name in enumerate(var_names):
                    hi_row = hi_row.with_columns(pl.lit(v.hi[k]).alias(var_name))
                    lo_row = lo_row.with_columns(pl.lit(v.lo[k]).alias(var_name))
                    contrast_label = f"{v.hi[k]} - {v.lo[k]}"
                    nd_row = nd_row.with_columns(
                        pl.lit(contrast_label).alias(f"contrast_{var_name}")
                    )
                    hi_row = hi_row.with_columns(
                        pl.lit(contrast_label).alias(f"contrast_{var_name}")
                    )
                    lo_row = lo_row.with_columns(
                        pl.lit(contrast_label).alias(f"contrast_{var_name}")
                    )

                nd_row = nd_row.with_columns(
                    pl.lit(var_names[0]).alias("term"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )
                hi_row = hi_row.with_columns(
                    pl.lit(var_names[0]).alias("term"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )
                lo_row = lo_row.with_columns(
                    pl.lit(var_names[0]).alias("term"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )

                nd.append(nd_row)
                hi.append(hi_row)
                lo.append(lo_row)
        else:
            hi.append(newdata)
            lo.append(newdata)
            nd.append(newdata)
            for v in variables:
                vcomp = "custom" if callable(v.comparison) else v.comparison
                nd[0] = nd[0].with_columns(
                    pl.lit(v.variable).alias("term"),
                    pl.lit(v.lab).alias(f"contrast_{v.variable}"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )
                hi[0] = hi[0].with_columns(
                    pl.lit(v.hi).alias(v.variable),
                    pl.lit(v.variable).alias("term"),
                    pl.lit(v.lab).alias(f"contrast_{v.variable}"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )
                lo[0] = lo[0].with_columns(
                    pl.lit(v.lo).alias(v.variable),
                    pl.lit(v.variable).alias("term"),
                    pl.lit(v.lab).alias(f"contrast_{v.variable}"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                )
    return nd, hi, lo


def _build_padding_frames(model, modeldata, newdata):
    pads = []
    vars = model.find_variables()
    if vars is not None:
        vars = [re.sub(r"\[.*", "", x) for x in vars]
        vars = list(set(vars))
        for v in vars:
            if v in modeldata.columns:
                if model.get_variable_type(v) not in ["numeric", "integer"]:
                    pads.append(get_pad(newdata, v, modeldata[v].unique()))
    return pads


def _finalize_counterfactual_frames(
    nd_frames, hi_frames, lo_frames, pad_frames, modeldata
):
    nd = pl.concat(nd_frames, how="vertical_relaxed")
    hi = pl.concat(hi_frames, how="vertical_relaxed")
    lo = pl.concat(lo_frames, how="vertical_relaxed")
    pad_frames = [x for x in pad_frames if x is not None]
    if len(pad_frames) == 0:
        pad_df = pl.DataFrame()
    else:
        for i, v in enumerate(pad_frames):
            pad_frames[i] = upcast(v, pad_frames[i - 1])
        pad_df = pl.concat(pad_frames, how="diagonal").unique()
        pad_df = pad_df.with_columns(pl.lit(-1).alias("rowid"))

    lo = upcast(lo, modeldata)
    hi = upcast(hi, modeldata)
    pad_df = upcast(pad_df, modeldata)
    nd = upcast(nd, modeldata)

    pad_df = upcast(pad_df, hi)
    nd = upcast(nd, hi)

    dfs_to_align = [("nd", nd), ("hi", hi), ("lo", lo)]

    for df_name, df in dfs_to_align:
        common_cols = set(pad_df.columns) & set(df.columns)
        for col in common_cols:
            pad_dtype = str(pad_df[col].dtype)
            df_dtype = str(df[col].dtype)
            if pad_dtype != df_dtype:
                if pad_dtype.startswith("List(") and df_dtype.startswith("List("):
                    try:
                        if col in pad_df.columns:
                            pad_df = pad_df.with_columns(
                                pad_df[col]
                                .list.eval(pl.element().cast(pl.String))
                                .alias(col)
                            )
                        if col in df.columns:
                            df = df.with_columns(
                                df[col]
                                .list.eval(pl.element().cast(pl.String))
                                .alias(col)
                            )
                    except Exception as e:
                        print(
                            f"Warning: Could not convert List column {col} to strings: {e}"
                        )
                        try:
                            if col in pad_df.columns and pad_df.height > 0:
                                pad_df = pad_df.explode(col)
                            if col in df.columns and df.height > 0:
                                df = df.explode(col)
                        except Exception as e2:
                            print(f"Warning: Could not explode List column {col}: {e2}")
                            if col in pad_df.columns:
                                pad_df = pad_df.with_columns(
                                    pad_df[col].cast(pl.String).alias(col)
                                )
                            if col in df.columns:
                                df = df.with_columns(df[col].cast(pl.String).alias(col))

        if df_name == "nd":
            nd = df
        elif df_name == "hi":
            hi = df
        elif df_name == "lo":
            lo = df

    nd = pl.concat([pad_df, nd], how="diagonal")
    hi = pl.concat([pad_df, hi], how="diagonal")
    lo = pl.concat([pad_df, lo], how="diagonal")

    list_cols = [col for col in nd.columns if str(nd[col].dtype).startswith("List(")]
    categorical_list_cols = []
    for col in list_cols:
        dtype_str = str(nd[col].dtype)
        if (
            "Enum(" in dtype_str or "String" in dtype_str or "UInt32" in dtype_str
        ) and col in ["Region"]:
            categorical_list_cols.append(col)

    if categorical_list_cols:
        for col in categorical_list_cols:
            nd = nd.explode(col)
            hi = hi.explode(col)
            lo = lo.explode(col)

    pad_rows = pad_df.shape[0]
    return nd, hi, lo, pad_rows


def _prepare_design_matrices(model, nd, hi, lo, pad_rows):
    package = model.get_package() if hasattr(model, "get_package") else None
    typename = type(model).__name__.lower()
    uses_native_design = package in {"pyfixest", "linearmodels", "sklearn"} or any(
        x in typename for x in ("modelpyfixest", "modellinearmodels", "modelsklearn")
    )

    if uses_native_design:
        hi_X = hi
        lo_X = lo
        nd_X = nd
    else:
        import patsy

        fml = re.sub(r".*~", "", model.get_formula())
        hi_X = patsy.dmatrix(fml, hi.to_pandas())
        lo_X = patsy.dmatrix(fml, lo.to_pandas())
        nd_X = patsy.dmatrix(fml, nd.to_pandas())

    if pad_rows >= 0:
        nd_X = nd_X[pad_rows:]
        hi_X = hi_X[pad_rows:]
        lo_X = lo_X[pad_rows:]
        nd = nd[pad_rows:]
        hi = hi[pad_rows:]
        lo = lo[pad_rows:]

    return nd, hi, lo, nd_X, hi_X, lo_X


def _collect_comparison_functions(variables):
    comparison_functions = {}
    for v in variables:
        if callable(v.comparison):
            key = f"{v.variable}_{v.lab}"
            comparison_functions[key] = v.comparison
    return comparison_functions


def comparisons(
    model,
    variables=None,
    newdata=None,
    comparison="difference",
    vcov=True,
    conf_level=0.95,
    by=False,
    wts=None,
    hypothesis=None,
    equivalence=None,
    cross=False,
    transform=None,
    eps=1e-4,
    eps_vcov=None,
    **kwargs,
):
    """
    `comparisons()` and `avg_comparisons()` are functions for predicting the outcome variable at different regressor values and comparing those predictions by computing a difference, ratio, or some other function. These functions can return many quantities of interest, such as contrasts, differences, risk ratios, changes in log odds, lift, slopes, elasticities, average treatment effect (on the treated or untreated), etc.

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(comparisons)`
    """
    hypothesis = handle_deprecated_hypotheses_argument(hypothesis, kwargs, stacklevel=2)
    if kwargs:
        unexpected = ", ".join(sorted(kwargs.keys()))
        raise TypeError(
            f"comparisons() got unexpected keyword argument(s): {unexpected}"
        )

    if callable(newdata):
        newdata = newdata(model)

    model = sanitize_model(model)
    vcov = handle_pyfixest_vcov_limitation(model, vcov, stacklevel=2)

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
        enforce_pyfixest_warning=False,
    )

    postprocess_cross = _cross_postprocess(cross)

    # Validate that columns used in by and variables are not String type
    validate_string_columns(by, modeldata, context="the 'by' parameter")
    validate_string_columns(variables, modeldata, context="the 'variables' parameter")

    (
        variables,
        nd,
        hi,
        lo,
        pad_rows,
    ) = _prepare_counterfactual_frames(
        model=model,
        modeldata=modeldata,
        newdata=newdata,
        variables=variables,
        comparison=comparison,
        eps=eps,
        by=by,
        wts=wts,
        cross=cross,
    )

    nd, hi, lo, nd_X, hi_X, lo_X = _prepare_design_matrices(model, nd, hi, lo, pad_rows)

    comparison_functions = _collect_comparison_functions(variables)

    # === TRY JAX EARLY EXIT ===
    # Only attempt JAX if all contrasts use the same comparison type
    from .jax_dispatch import try_jax_comparisons

    jax_result = try_jax_comparisons(
        model=model,
        hi_X=hi_X,
        lo_X=lo_X,
        vcov=V,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
        comparison=comparison,
        cross=cross,
        nd=nd,
    )

    if jax_result is not None:
        # SUCCESS: Use JAX results
        J = jax_result["jacobian"]

        estimate = np.atleast_1d(jax_result["estimate"])
        std_error = np.atleast_1d(jax_result["std_error"])
        metadata = jax_result.get("metadata")

        if metadata is not None:
            out = metadata.with_columns(
                pl.Series(estimate).alias("estimate"),
                pl.Series(std_error).alias("std_error"),
            )
        else:
            out = pl.DataFrame(
                {
                    "rowid": nd["rowid"].to_list(),
                    "term": nd["term"].to_list(),
                    "contrast": nd["contrast"].to_list(),
                    "estimate": estimate,
                    "std_error": std_error,
                }
            )
            cols = [
                x
                for x in nd.columns
                if x not in out.columns and x != "marginaleffects_comparison"
            ]
            if cols:
                out = pl.concat([out, nd.select(cols)], how="horizontal")

        out = get_z_p_ci(
            out, model, conf_level=conf_level, hypothesis_null=hypothesis_null
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
            equivalence_df=np.inf,
            postprocess=postprocess_cross,
        )

    # === END JAX EARLY EXIT ===

    from .hypothesis import get_hypothesis

    # inner() takes the `hi` and `lo` matrices, computes predictions, compares
    # them, and aggregates the results based on the `by` argument. This gives us
    # the final quantity of interest. We wrap this in a function because it will
    # be called multiple times with slightly different values of the `coefs`.
    # This is necessary to compute the numerical derivatives in the Jacobian
    # that we use to compute standard errors, where individual entries are
    # derivatives of a contrast with respect to one of the model coefficients.
    def inner(coefs, by, hypothesis, wts, nd):
        if hasattr(coefs, "to_numpy"):
            coefs = coefs.to_numpy()

        # main unit-level estimates
        tmp = [
            # fitted values
            model.get_predict(params=coefs, newdata=nd_X).rename(
                {"estimate": "predicted"}
            ),
            # predictions for the "lo" counterfactual
            model.get_predict(params=coefs, newdata=lo_X)
            .rename({"estimate": "predicted_lo"})
            .select("predicted_lo"),
            # predictions for the "hi" counterfactual
            model.get_predict(params=coefs, newdata=hi_X)
            .rename({"estimate": "predicted_hi"})
            .select("predicted_hi"),
        ]
        tmp = reduce(lambda x, y: pl.concat([x, y], how="horizontal"), tmp)
        if "rowid" in nd.columns and tmp.shape[0] == nd.shape[0]:
            tmp = tmp.with_columns(nd["rowid"].alias("rowid"))

        # no group
        if tmp.shape[0] == nd.shape[0]:
            cols = [x for x in nd.columns if x not in tmp.columns]
            tmp = pl.concat([tmp, nd.select(cols)], how="horizontal")

        # group (categorical outcome models)
        elif "group" in tmp.columns:
            meta = nd.join(tmp.select("group").unique(), how="cross")
            cols = [x for x in meta.columns if x in tmp.columns]
            tmp = meta.join(tmp, on=cols, how="left")

        # not sure what happens here
        else:
            raise ValueError("Something went wrong")

        # column names on which we will aggregate results
        if isinstance(by, str):
            by = ["term", "contrast"] + [by]
        elif isinstance(by, list):
            by = ["term", "contrast"] + by
        else:
            by = ["term", "contrast"]

        by = by + [x for x in tmp.columns if x.startswith("contrast_")]

        # apply a function to compare the predicted_hi and predicted_lo columns
        # ex: hi-lo, mean(hi-lo), hi/lo, mean(hi)/mean(lo), etc.
        def applyfun(x, by, wts=None):
            comp = x["marginaleffects_comparison"][0]
            xvar = x[x["term"][0]]
            if wts is not None:
                xwts = x[wts]
            else:
                xwts = None

            # Check if this is a custom callable comparison
            term_val = x["term"][0] if "term" in x.columns else None
            contrast_val = x["contrast"][0] if "contrast" in x.columns else None
            key = f"{term_val}_{contrast_val}"

            if comp == "custom" and key in comparison_functions:
                # Use the callable comparison function
                estimand = comparison_functions[key]
            else:
                # Use the predefined estimand
                estimand = estimands[comp]

            est = estimand(
                hi=x["predicted_hi"],
                lo=x["predicted_lo"],
                eps=eps,
                x=xvar,
                y=x["predicted"],
                w=xwts,
            )
            if est.shape[0] == 1:
                est = est.item()
                tmp = x.select(by).unique().with_columns(pl.lit(est).alias("estimate"))
            else:
                tmp = x.with_columns(pl.lit(est).alias("estimate"))
            return tmp

        # maintain_order is extremely important
        by = [x for x in by if x in tmp.columns]
        tmp = tmp.group_by(*by, maintain_order=True).map_groups(
            function=lambda x: applyfun(x, by=by, wts=wts)
        )

        tmp = get_hypothesis(tmp, hypothesis=hypothesis, by=by)

        return tmp

    # outer() is a wrapper with a single argument `x`, the model coefficients.
    # Just for convenience when taking derivatives with respect to the
    # coefficients.
    def outer(x):
        return inner(x, by=by, hypothesis=hypothesis, wts=wts, nd=nd)

    out = outer(model.get_coef())

    # Compute standard errors and confidence intervals
    if vcov is not None and vcov is not False and V is not None:
        J = get_jacobian(func=outer, coefs=model.get_coef(), eps_vcov=eps_vcov)
        se = get_se(J, V)
        out = out.with_columns(pl.Series(se).alias("std_error"))
        out = get_z_p_ci(
            out, model, conf_level=conf_level, hypothesis_null=hypothesis_null
        )
    else:
        J = None

    return finalize_result(
        out,
        model=model,
        by=by,
        transform=transform,
        equivalence=equivalence,
        newdata=newdata,
        conf_level=conf_level,
        J=J,
        equivalence_df=np.inf,
        postprocess=postprocess_cross,
    )


def avg_comparisons(
    model,
    variables=None,
    newdata=None,
    comparison="difference",
    vcov=True,
    conf_level=0.95,
    by=True,
    wts=None,
    hypothesis=None,
    equivalence=None,
    cross=False,
    transform=None,
    eps=1e-4,
    **kwargs,
):
    """
    `comparisons()` and `avg_comparisons()` are functions for predicting the outcome variable at different regressor values and comparing those predictions by computing a difference, ratio, or some other function. These functions can return many quantities of interest, such as contrasts, differences, risk ratios, changes in log odds, lift, slopes, elasticities, average treatment effect (on the treated or untreated), etc.

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(avg_comparisons)`
    """
    return call_avg(
        comparisons,
        model=model,
        newdata=newdata,
        variables=variables,
        comparison=comparison,
        vcov=vcov,
        conf_level=conf_level,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
        equivalence=equivalence,
        cross=cross,
        transform=transform,
        eps=eps,
        **kwargs,
    )


docs_comparisons = (
    """

# `comparisons()`

`comparisons()` and `avg_comparisons()` are functions for predicting the outcome variable at different regressor values and comparing those predictions by computing a difference, ratio, or some other function. These functions can return many quantities of interest, such as contrasts, differences, risk ratios, changes in log odds, lift, slopes, elasticities, average treatment effect (on the treated or untreated), etc.

* `comparisons()`: unit-level (conditional) estimates.
* `avg_comparisons()`: average (marginal) estimates.

See the package website and vignette for examples:

* https://marginaleffects.com/chapters/comparisons.html
* https://marginaleffects.com

## Parameters
"""
    + DocsParameters.docstring_model
    + DocsParameters.docstring_variables("comparison")
    + DocsParameters.docstring_newdata("comparison")
    + """
* `comparison`: (str or callable) String specifying how pairs of predictions should be compared, or a callable function to compute custom estimates. See the Comparisons section below for definitions of each transformation.

  * Acceptable strings: difference, differenceavg, differenceavgwts, dydx, eyex, eydx, dyex, dydxavg, eyexavg, eydxavg, dyexavg, dydxavgwts, eyexavgwts, eydxavgwts, dyexavgwts, ratio, ratioavg, ratioavgwts, lnratio, lnratioavg, lnratioavgwts, lnor, lnoravg, lnoravgwts, lift, liftavg, liftavg, expdydx, expdydxavg, expdydxavgwts

  * Callable: A function that takes `hi`, `lo`, `eps`, `x`, `y`, and `w` as arguments and returns a numeric array. This allows computing custom comparisons like `lambda hi, lo, eps, x, y, w: hi / lo` for ratios or `lambda hi, lo, eps, x, y, w: (hi - lo) / lo * 100` for percent changes.
"""
    + DocsParameters.docstring_by
    + DocsParameters.docstring_transform
    + DocsParameters.docstring_hypothesis
    + DocsParameters.docstring_wts
    + DocsParameters.docstring_vcov
    + DocsParameters.docstring_equivalence
    + DocsParameters.docstring_cross
    + DocsParameters.docstring_conf_level
    + DocsParameters.docstring_eps
    + DocsParameters.docstring_eps_vcov
    + docstring_returns
    + """
## Examples
```py
from marginaleffects import *
import numpy as np

import statsmodels.api as sm
import statsmodels.formula.api as smf
data = get_dataset("thornton")
model = smf.ols("outcome ~ distance + incentive", data=data).fit()

# Basic comparisons
comparisons(model)

avg_comparisons(model)

comparisons(model, hypothesis=0)

avg_comparisons(model, hypothesis=0)

comparisons(model, by="agecat")

avg_comparisons(model, by="agecat")

# Custom comparisons with lambda functions
# Ratio comparison using lambda
comparisons(model, variables="distance",
            comparison=lambda hi, lo, eps, x, y, w: hi / lo)

# Percent change using lambda
comparisons(model, variables="distance",
            comparison=lambda hi, lo, eps, x, y, w: (hi - lo) / lo * 100)

# Log ratio using lambda
comparisons(model, variables="distance",
            comparison=lambda hi, lo, eps, x, y, w: np.log(hi / lo))
```

## Details
"""
    + DocsDetails.docstring_tost
    + DocsDetails.docstring_order_of_operations
    + ""  # add comparisons argument functions section as in R at https://marginaleffects.com/man/r/comparisons.html
)


comparisons.__doc__ = docs_comparisons

avg_comparisons.__doc__ = comparisons.__doc__
