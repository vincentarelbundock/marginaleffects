import re
from functools import reduce

import numpy as np
import polars as pl

from .estimands import estimands
from .sanitize import sanitize_model
from .sanitize import (
    sanitize_variables,
    handle_deprecated_hypotheses_argument,
    handle_pyfixest_vcov_limitation,
)
from .classes import MarginaleffectsResult
from .uncertainty import add_standard_errors
from .utils import (
    get_pad,
    upcast,
    validate_string_columns,
    finalize_result,
    call_avg,
    prepare_base_inputs,
)
from .docstrings import doc


def _cross_postprocess(cross):
    if not cross:
        return None

    def add_term(df):
        return df.with_columns(pl.lit("cross").alias("term"))

    return add_term


def _tag_frame(df, variable, lab, comparison, **extra_cols):
    """Add term, contrast, marginaleffects_comparison columns to a frame."""
    vcomp = "custom" if callable(comparison) else comparison
    cols = {
        "term": pl.lit(variable if not isinstance(variable, tuple) else variable[0]),
        "marginaleffects_comparison": pl.lit(vcomp),
    }
    if lab is not None:
        cols["contrast"] = pl.lit(lab)
    cols.update({k: pl.lit(v) for k, v in extra_cols.items()})
    return df.with_columns(**cols)


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
    hi_X = model.get_exog(hi)
    lo_X = model.get_exog(lo)
    nd_X = model.get_exog(nd)

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


def _assemble_prediction_table(model, coefs, nd, nd_X, hi_X, lo_X):
    """
    Compute predictions at nd/hi/lo counterfactuals and merge with metadata from nd.

    Returns a DataFrame with columns: rowid, term, contrast, predicted,
    predicted_hi, predicted_lo, plus all metadata columns from nd.

    Handles two cases:
    - Simple (1D predict): tmp rows == nd rows -> horizontal concat of metadata
    - Grouped (2D predict, e.g. multinomial): "group" in tmp -> cross-join alignment
    """
    tmp = [
        model.get_predict(params=coefs, newdata=nd_X).rename({"estimate": "predicted"}),
        model.get_predict(params=coefs, newdata=lo_X)
        .rename({"estimate": "predicted_lo"})
        .select("predicted_lo"),
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
        # tmp has rowid 0..N-1 from get_predict, matching nd row positions.
        # nd may already have a rowid column (from sanitize_newdata) with
        # non-unique values across variables, so we create a unique
        # positional index for the join.
        nd_m = nd.with_columns(
            pl.Series("_merge_id", range(nd.shape[0]), dtype=pl.Int32)
        )
        tmp = tmp.rename({"rowid": "_merge_id"})
        meta = nd_m.join(tmp.select("group").unique(), how="cross")
        cols = [x for x in meta.columns if x in tmp.columns]
        tmp = meta.join(tmp, on=cols, how="left")
        tmp = tmp.drop("_merge_id")

    else:
        raise ValueError("Something went wrong")

    return tmp


def _resolve_grouping_keys(by, tmp):
    """
    Normalize `by` into a list of grouping column names.

    Always includes ["term", "contrast"]. Prepends "group" if present in tmp.
    Appends any "contrast_*" columns. Filters to columns that exist in tmp.
    """
    if isinstance(by, str):
        by = ["term", "contrast"] + [by]
    elif isinstance(by, list):
        by = ["term", "contrast"] + by
    else:
        by = ["term", "contrast"]

    if "group" in tmp.columns and "group" not in by:
        by = ["group"] + by

    by = by + [x for x in tmp.columns if x.startswith("contrast_")]
    by = [x for x in by if x in tmp.columns]
    return by


def _apply_comparison_estimands(tmp, by, wts, eps, comparison_functions):
    """
    Apply comparison functions to predicted_hi/predicted_lo within groups.

    Uses group_by with maintain_order=True (critical for Jacobian alignment).
    """

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
            estimand = comparison_functions[key]
        else:
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
            result = x.select(by).unique().with_columns(pl.lit(est).alias("estimate"))
        else:
            result = x.with_columns(pl.lit(est).alias("estimate"))
        return result

    # maintain_order is extremely important
    tmp = tmp.group_by(*by, maintain_order=True).map_groups(
        function=lambda x: applyfun(x, by=by, wts=wts)
    )
    return tmp


def _compute_fd_estimates(
    coefs,
    *,
    model,
    nd,
    nd_X,
    hi_X,
    lo_X,
    by,
    hypothesis,
    wts,
    eps,
    comparison_functions,
):
    """
    Full coefficient-to-estimate pipeline for finite differences.

    Called once at true coefficients to get point estimates, then once per
    perturbed coefficient by get_jacobian() to compute numerical derivatives.
    """
    from .test import get_hypothesis

    if hasattr(coefs, "to_numpy"):
        coefs = coefs.to_numpy()

    tmp = _assemble_prediction_table(model, coefs, nd, nd_X, hi_X, lo_X)
    by_keys = _resolve_grouping_keys(by, tmp)
    tmp = _apply_comparison_estimands(tmp, by_keys, wts, eps, comparison_functions)
    tmp = get_hypothesis(tmp, hypothesis=hypothesis, by=by_keys)
    return tmp


def _normalize_jax_result(jax_result, nd):
    """
    Convert JAX dispatch result dict into a DataFrame matching the FD output schema.
    """
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

    return out


@doc("""

# `comparisons()`

`comparisons()` and `avg_comparisons()` are functions for predicting the outcome variable at different regressor values and comparing those predictions by computing a difference, ratio, or some other function. These functions can return many quantities of interest, such as contrasts, differences, risk ratios, changes in log odds, lift, slopes, elasticities, average treatment effect (on the treated or untreated), etc.

- `comparisons()`: unit-level (conditional) estimates.
- `avg_comparisons()`: average (marginal) estimates.

See the package website and vignette for examples:

- https://marginaleffects.com/chapters/comparisons.html
- https://marginaleffects.com

## Parameters

{param_model}

{param_variables_comparison}

{param_newdata_comparison}

- `comparison`: (str or callable) String specifying how pairs of predictions should be compared, or a callable function to compute custom estimates. See the Comparisons section below for definitions of each transformation.
    - Acceptable strings: difference, differenceavg, differenceavgwts, dydx, eyex, eydx, dyex, dydxavg, eyexavg, eydxavg, dyexavg, dydxavgwts, eyexavgwts, eydxavgwts, dyexavgwts, ratio, ratioavg, ratioavgwts, lnratio, lnratioavg, lnratioavgwts, lnor, lnoravg, lnoravgwts, lift, liftavg, liftavg, expdydx, expdydxavg, expdydxavgwts
    - Callable: A function that accepts any subset of the named arguments `hi`, `lo`, `eps`, `x`, `y`, and `w`, and returns a numeric value or array. For example: `lambda hi, lo: hi / lo` for ratios, `lambda hi, lo: (hi - lo) / lo * 100` for percent changes, or a named function like `def lnor(hi, lo): return np.log((hi.mean() / (1 - hi.mean())) / (lo.mean() / (1 - lo.mean())))`.

{param_by}

{param_transform}

{param_hypothesis}

{param_wts}

{param_vcov}

{param_equivalence}

{param_cross}

{param_conf_level}

{param_eps}

{param_eps_vcov}

{returns}

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

# Custom comparisons with functions
# Ratio comparison using lambda
comparisons(model, variables="distance",
            comparison=lambda hi, lo: hi / lo)

# Percent change using lambda
comparisons(model, variables="distance",
            comparison=lambda hi, lo: (hi - lo) / lo * 100)

# Custom function with flexible signature
def lnor(hi, lo):
    hi = np.asarray(hi)
    lo = np.asarray(lo)
    return np.log((hi.mean() / (1 - hi.mean())) / (lo.mean() / (1 - lo.mean())))

comparisons(model, variables="distance", comparison=lnor)
```

## Details
{details_tost}

{details_order_of_operations}""")
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
) -> MarginaleffectsResult:
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
    pad_frames = []
    model_vars = model.find_variables()
    if model_vars is not None:
        model_vars = list(set(re.sub(r"\[.*", "", x) for x in model_vars))
        for v in model_vars:
            if v in modeldata.columns:
                if model.get_variable_type(v) not in ["numeric", "integer"]:
                    pad_frames.append(get_pad(newdata, v, modeldata[v].unique()))
    nd, hi, lo, pad_rows = _finalize_counterfactual_frames(
        nd_frames,
        hi_frames,
        lo_frames,
        pad_frames,
        modeldata,
    )

    nd, hi, lo, nd_X, hi_X, lo_X = _prepare_design_matrices(model, nd, hi, lo, pad_rows)

    comparison_functions = _collect_comparison_functions(variables)

    # === JAX fast path ===
    from .autodiff.dispatch import try_jax_comparisons

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
        out = _normalize_jax_result(jax_result, nd)
        return finalize_result(
            out,
            model=model,
            by=by,
            transform=transform,
            equivalence=equivalence,
            newdata=newdata,
            conf_level=conf_level,
            J=jax_result["jacobian"],
            hypothesis_null=hypothesis_null,
            equivalence_df=np.inf,
            postprocess=postprocess_cross,
        )

    # === Finite-difference path ===
    def outer(x):
        return _compute_fd_estimates(
            x,
            model=model,
            nd=nd,
            nd_X=nd_X,
            hi_X=hi_X,
            lo_X=lo_X,
            by=by,
            hypothesis=hypothesis,
            wts=wts,
            eps=eps,
            comparison_functions=comparison_functions,
        )

    out = outer(model.get_coef())

    if vcov is not None and vcov is not False and V is not None:
        out, J = add_standard_errors(out, outer, model, V, eps_vcov)
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
        hypothesis_null=hypothesis_null,
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
) -> MarginaleffectsResult:
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


avg_comparisons.__doc__ = comparisons.__doc__
