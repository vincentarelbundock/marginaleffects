import re
import warnings

import numpy as np
import polars as pl

from .autodiff.lower import autodiff_try
from .estimands import estimands
from .hypothesis_compile import hypothesis_compile
from .sanitize import sanitize_model
from .sanitize import (
    sanitize_variables,
    handle_deprecated_hypotheses_argument,
    handle_pyfixest_vcov_limitation,
)
from .classes import MarginaleffectsResult
from .uncertainty import get_jacobian, get_se
from .plan import (
    CompGroup,
    ComparisonPlan,
    comparison_plan_apply,
    comparison_plan_predict,
)
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
            nd_row = newdata.clone()
            hi_row = newdata.clone()
            lo_row = newdata.clone()
            for v in variables:
                vcomp = "custom" if callable(v.comparison) else v.comparison
                shared = [
                    pl.lit(v.variable).alias("term"),
                    pl.lit(v.lab).alias(f"contrast_{v.variable}"),
                    pl.lit(vcomp).alias("marginaleffects_comparison"),
                ]
                nd_row = nd_row.with_columns(*shared)
                hi_row = hi_row.with_columns(pl.lit(v.hi).alias(v.variable), *shared)
                lo_row = lo_row.with_columns(pl.lit(v.lo).alias(v.variable), *shared)
            nd.append(nd_row)
            hi.append(hi_row)
            lo.append(lo_row)
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

    dfs = {"nd": nd, "hi": hi, "lo": lo}

    for df_name in dfs:
        df = dfs[df_name]
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
                        warnings.warn(
                            f"Could not convert List column {col} to strings: {e}"
                        )
                        try:
                            if col in pad_df.columns and pad_df.height > 0:
                                pad_df = pad_df.explode(col)
                            if col in df.columns and df.height > 0:
                                df = df.explode(col)
                        except Exception as e2:
                            warnings.warn(f"Could not explode List column {col}: {e2}")
                            if col in pad_df.columns:
                                pad_df = pad_df.with_columns(
                                    pad_df[col].cast(pl.String).alias(col)
                                )
                            if col in df.columns:
                                df = df.with_columns(df[col].cast(pl.String).alias(col))

        dfs[df_name] = df

    nd, hi, lo = dfs["nd"], dfs["hi"], dfs["lo"]

    nd = pl.concat([pad_df, nd], how="diagonal")
    hi = pl.concat([pad_df, hi], how="diagonal")
    lo = pl.concat([pad_df, lo], how="diagonal")

    list_cols = [col for col in nd.columns if str(nd[col].dtype).startswith("List(")]
    categorical_list_cols = []
    for col in list_cols:
        dtype_str = str(nd[col].dtype)
        if "Enum(" in dtype_str or "String" in dtype_str or "UInt32" in dtype_str:
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

    if pad_rows > 0:
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


ELASTICITY_KEYS = {
    "eyex",
    "eydx",
    "dyex",
    "eyexavg",
    "eydxavg",
    "dyexavg",
    "eyexavgwts",
    "eydxavgwts",
    "dyexavgwts",
}


def _assemble_prediction_table(model, coefs, nd, nd_X, hi_X, lo_X, capture_align=False):
    """
    Compute predictions at nd/hi/lo counterfactuals and merge with metadata from nd.

    Returns a DataFrame with columns: rowid, term, contrast, predicted,
    predicted_hi, predicted_lo, plus all metadata columns from nd.

    Handles two cases:
    - Simple (1D predict): tmp rows == nd rows -> horizontal concat of metadata
    - Grouped (2D predict, e.g. multinomial): "group" in tmp -> cross-join alignment
    """
    pred = model.get_predict(params=coefs, newdata=nd_X).rename(
        {"estimate": "predicted"}
    )
    pred_lo = (
        model.get_predict(params=coefs, newdata=lo_X)
        .rename({"estimate": "predicted_lo"})
        .select("predicted_lo")
    )
    pred_hi = (
        model.get_predict(params=coefs, newdata=hi_X)
        .rename({"estimate": "predicted_hi"})
        .select("predicted_hi")
    )
    tmp = [pred, pred_lo, pred_hi]
    tmp = pl.concat(tmp, how="horizontal_extend")
    align = None
    if "rowid" in nd.columns and tmp.shape[0] == nd.shape[0]:
        tmp = tmp.with_columns(nd["rowid"].alias("rowid"))

    # no group
    if tmp.shape[0] == nd.shape[0]:
        cols = [x for x in nd.columns if x not in tmp.columns]
        tmp = pl.concat([tmp, nd.select(cols)], how="horizontal_extend")

    # group (categorical outcome models)
    elif "group" in tmp.columns:
        # tmp has rowid 0..N-1 from get_predict, matching nd row positions.
        # nd may already have a rowid column (from sanitize_newdata) with
        # non-unique values across variables, so we create a unique
        # positional index for the join.
        nd_m = nd.with_columns(
            pl.Series("_merge_id", range(nd.shape[0]), dtype=pl.Int32)
        )
        if capture_align:
            tmp = tmp.with_columns(
                pl.Series("_pred_row", range(tmp.height), dtype=pl.Int32)
            )
        tmp = tmp.rename({"rowid": "_merge_id"})
        meta = nd_m.join(tmp.select("group").unique(), how="cross")
        cols = [x for x in meta.columns if x in tmp.columns]
        tmp = meta.join(tmp, on=cols, how="left")
        if capture_align:
            align = tmp["_pred_row"].fill_null(-1).cast(pl.Int64).to_numpy()
            tmp = tmp.drop("_pred_row")
        tmp = tmp.drop("_merge_id")

    else:
        raise ValueError("Something went wrong")

    if capture_align:
        plan_predictions = (
            np.asarray(pred_hi["predicted_hi"].to_numpy(), dtype=float).reshape(-1),
            np.asarray(pred_lo["predicted_lo"].to_numpy(), dtype=float).reshape(-1),
            np.asarray(pred["predicted"].to_numpy(), dtype=float).reshape(-1),
        )
        return tmp, align, plan_predictions
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


def _apply_comparison_estimands(tmp, by, wts, eps, comparison_functions, capture=None):
    """
    Apply comparison functions to predicted_hi/predicted_lo within groups.

    Uses group_by with maintain_order=True (critical for Jacobian alignment).
    """

    if capture is not None:
        tmp = tmp.with_columns(
            pl.Series("_plan_est_id", range(tmp.height), dtype=pl.Int32)
        )

    def applyfun(x, by, wts=None, capture=None):
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
        if capture is not None:
            capture.append(
                {
                    "idx": x["_plan_est_id"].to_numpy(),
                    "fun": estimand,
                    "fun_key": None if comp == "custom" else comp,
                    "x": xvar.to_numpy(),
                    "w": None if xwts is None else xwts.to_numpy(),
                    "scalar": est.shape[0] == 1,
                }
            )
        if est.shape[0] == 1:
            est = est.item()
            result = x.select(by).unique().with_columns(pl.lit(est).alias("estimate"))
        else:
            result = x.with_columns(pl.lit(est).alias("estimate"))
        if "_plan_est_id" in result.columns:
            result = result.drop("_plan_est_id")
        return result

    # maintain_order is extremely important
    tmp = tmp.group_by(*by, maintain_order=True).map_groups(
        function=lambda x: applyfun(x, by=by, wts=wts, capture=capture)
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


def _comparisons_build(
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
    coefs = model.get_coef()
    tmp, align, plan_predictions = _assemble_prediction_table(
        model,
        coefs,
        nd,
        nd_X,
        hi_X,
        lo_X,
        capture_align=True,
    )
    has_na = any(
        np.isnan(np.asarray(tmp[col].to_numpy(), dtype=float)).any()
        for col in ["predicted", "predicted_hi", "predicted_lo"]
        if col in tmp.columns
    )
    by_keys = _resolve_grouping_keys(by, tmp)
    captured = []
    tmp = _apply_comparison_estimands(
        tmp,
        by_keys,
        wts,
        eps,
        comparison_functions,
        capture=captured,
    )
    tmp, hyp = hypothesis_compile(tmp, hypothesis=hypothesis, by=by_keys)

    groups = []
    start = 0
    for item in captured:
        n_out = 1 if item["scalar"] else len(item["idx"])
        out_idx = np.arange(start, start + n_out, dtype=int)
        groups.append(
            CompGroup(
                idx=np.asarray(item["idx"], dtype=int),
                out_idx=out_idx,
                scalar=bool(item["scalar"]),
                fun=item["fun"],
                fun_key=item["fun_key"],
                x=None if item["x"] is None else np.asarray(item["x"]),
                w=None if item["w"] is None else np.asarray(item["w"], dtype=float),
            )
        )
        start += n_out

    if groups:
        actual = np.concatenate([group.out_idx for group in groups])
        expected = np.arange(start, dtype=int)
        if not np.array_equal(actual, expected):
            raise RuntimeError(
                "marginaleffects internal error: comparison plan output order failed"
            )

    need_y = any(
        group.fun_key is None or group.fun_key in ELASTICITY_KEYS for group in groups
    )
    plan = ComparisonPlan(
        n_pred=hi_X.shape[0],
        exog_hi=hi_X,
        exog_lo=lo_X,
        exog_nd=nd_X if need_y else None,
        need_y=need_y,
        align=align,
        eps=eps,
        groups=groups,
        n_comp=start,
        hyp=hyp,
        has_na=bool(has_na),
    )

    hi, lo, y = plan_predictions
    replay = comparison_plan_apply(plan, hi, lo, y if plan.need_y else None)
    if not np.allclose(
        replay,
        tmp["estimate"].to_numpy(),
        rtol=1e-12,
        atol=1e-12,
        equal_nan=True,
    ):
        raise RuntimeError(
            "marginaleffects internal error: comparison plan baseline check failed"
        )
    return tmp, plan


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

    out, plan = _comparisons_build(
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
    if vcov is not None and vcov is not False and V is not None:
        ad = autodiff_try(
            plan=plan,
            model=model,
            V=V,
            estimate=out["estimate"].to_numpy(),
            kind="comparisons",
        )
        if ad is not None:
            J = ad.jacobian
            out = out.with_columns(pl.Series(ad.std_error).alias("std_error"))
        else:

            def replay(beta):
                hi_pred, lo_pred, y_pred = comparison_plan_predict(plan, model, beta)
                return comparison_plan_apply(plan, hi_pred, lo_pred, y_pred)

            J = get_jacobian(replay, model.get_coef(), eps_vcov)
            se = get_se(J, V)
            out = out.with_columns(pl.Series(se).alias("std_error"))
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
