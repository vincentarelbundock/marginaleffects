from collections import namedtuple
from warnings import warn

import numpy as np
import polars as pl

from .datagrid import datagrid
from .estimands import estimands
from .utils import ingest, upcast
from .formulaic_utils import listwise_deletion


# ============================================================================
# Unified Argument Handling Functions
# ============================================================================


def handle_deprecated_hypotheses_argument(hypothesis, kwargs, stacklevel=2):
    """Handle migration from 'hypotheses' to 'hypothesis' parameter.

    Parameters
    ----------
    hypothesis : object
        Current hypothesis value
    kwargs : dict
        Keyword arguments dict that may contain 'hypotheses'
    stacklevel : int
        Stack level for warning

    Returns
    -------
    object
        The hypothesis value (either from kwargs or original parameter)

    Raises
    ------
    ValueError
        If both hypothesis and hypotheses are specified
    """
    if "hypotheses" in kwargs:
        if hypothesis is not None:
            raise ValueError("Specify at most one of `hypothesis` or `hypotheses`.")
        hypotheses = kwargs.pop("hypotheses")
        warn(
            "`hypotheses` is deprecated; use `hypothesis` instead.",
            DeprecationWarning,
            stacklevel=stacklevel,
        )
        return hypotheses
    return hypothesis


def handle_pyfixest_vcov_limitation(model, vcov, stacklevel=2):
    """Handle pyfixest vcov constraints with appropriate warnings.

    PyFixest models have specific limitations with standard error computation
    for non-linear models with fixed effects.

    Parameters
    ----------
    model : object
        Fitted model (after sanitization)
    vcov : bool or array-like
        Variance-covariance specification
    stacklevel : int
        Stack level for warning

    Returns
    -------
    bool or array-like
        Modified vcov value if PyFixest limitation applies, otherwise unchanged
    """
    # Import here to avoid circular dependency
    from .pyfixest import ModelPyfixest

    if isinstance(model, ModelPyfixest) and vcov is not False:
        has_fixef = getattr(model.model, "_has_fixef", False)
        if has_fixef and not model.is_linear_model():
            warn(
                "For this pyfixest model, marginaleffects cannot take into account the "
                "uncertainty in fixed-effects parameters. Standard errors are disabled "
                "and vcov=False is enforced.",
                UserWarning,
                stacklevel=stacklevel,
            )
            return False

    return vcov


def sanitize_vcov(vcov, model):
    V = model.get_vcov(vcov)
    if V is not None:
        assert isinstance(V, np.ndarray), "vcov must be True or a square NumPy array"
    return V


def sanitize_by(by):
    if by is True:
        by = ["group"]
    elif isinstance(by, str):
        by = ["group", by]
    elif isinstance(by, list):
        by = ["group"] + by
    elif by is False:
        by = False
    else:
        raise ValueError(
            "The `by` argument must be True, False, a string, or a list of strings."
        )
    return by


def sanitize_newdata(model, newdata, wts, by=[]):
    modeldata = model.get_modeldata()

    if newdata is None:
        out = modeldata
        newdata = modeldata

    if isinstance(newdata, pl.DataFrame):
        predictors = model.find_predictors()
        # sklearn without known predictor names
        if predictors is not None:
            predictors = newdata.select(predictors)
            any_missing = any(predictors.select(pl.all().is_null().any()).row(0))
            if any_missing:
                raise ValueError(
                    "Please supply a data frame with no missing value to the `newdata` argument."
                )

    # if newdata is a string, then we need to treat `by` as unique entries.
    args = {"model": model}
    if isinstance(by, list) and len(by) > 0:
        for col in by:
            if isinstance(col, str):
                if col in modeldata.columns:
                    args[col] = modeldata[col].unique()

    if isinstance(newdata, str) and newdata == "mean":
        out = datagrid(**args)

    elif isinstance(newdata, str) and newdata == "median":
        args["FUN_numeric"] = lambda x: x.median()
        args["newdata"] = modeldata
        out = datagrid(**args)

    elif isinstance(newdata, str) and newdata == "balanced":
        args["FUN_other"] = lambda x: np.unique(x)
        args["grid_type"] = "balanced"
        newdata_columns = model.find_variables()
        newdata_columns = np.unique(newdata_columns)
        args["newdata"] = modeldata.select(newdata_columns)
        out = datagrid(**args)

    else:
        try:
            out = ingest(newdata)
        except Exception as e:
            raise e

    # user-supplied newdata may include missing values
    if model is not None and isinstance(out, pl.DataFrame):
        out = listwise_deletion(model.get_formula(), out)

    reserved_names = {
        "rowid",
        "type",
        "group",
        "estimate",
        "std_error",
        "p_value",
        "s_value",
        "conf_low",
        "conf_high",
        "term",
        "contrast",
        "statistic",
    }
    assert not (set(out.columns) & reserved_names), (
        f"Input data contain reserved column name(s) : {set(out.columns).intersection(reserved_names)}"
    )

    datagrid_explicit = None
    if isinstance(newdata, pl.DataFrame) and hasattr(newdata, "datagrid_explicit"):
        datagrid_explicit = newdata.datagrid_explicit

    if isinstance(by, list) and len(by) > 0:
        by = [x for x in by if x in out.columns]
        if len(by) > 0:
            out = out.sort(by)

    out = out.with_columns(pl.Series(range(out.height), dtype=pl.Int32).alias("rowid"))

    if wts is not None:
        if (isinstance(wts, str) is False) or (wts not in out.columns):
            raise ValueError(f"`newdata` does not have a column named '{wts}'.")

    if any([isinstance(out[x], pl.Categorical) for x in out.columns]):
        raise ValueError("Categorical type columns are not supported in `newdata`.")

    # ensure all enum levels are in modeldata
    for c in out.columns:
        if c in modeldata.columns and modeldata[c].dtype in [pl.Categorical, pl.Enum]:
            try:
                cat_modeldata = modeldata[c].unique()
                cat_out = out[c].unique()
                cat_out = [x for x in cat_out if x not in cat_modeldata]
                if len(cat_out) > 0:
                    raise ValueError(
                        f"Column `{c}` in `newdata` has levels not in the model data: {', '.join(cat_out)}"
                    )
            except pl.exceptions.InvalidOperationError:
                # Skip validation for columns that don't support unique() operation
                continue

    out = upcast(out, modeldata)

    if datagrid_explicit is not None:
        out.datagrid_explicit = datagrid_explicit

    return out


def sanitize_comparison(comparison, by, wts=None):
    # Handle callable comparison functions
    if callable(comparison):
        return (comparison, "custom")

    out = comparison
    if by is not False:
        if f"{comparison}avg" in estimands.keys():
            out = comparison + "avg"

    if wts is not None:
        if f"{out}wts" in estimands.keys():
            out = out + "wts"

    lab = {
        "difference": "{hi} - {lo}",
        "differenceavg": "{hi} - {lo}",
        "differenceavgwts": "{hi} - {lo}",
        "dydx": "dY/dX",
        "eyex": "eY/eX",
        "eydx": "eY/dX",
        "dyex": "dY/eX",
        "dydxavg": "dY/dX",
        "eyexavg": "eY/eX",
        "eydxavg": "eY/dX",
        "dyexavg": "dY/eX",
        "dydxavgwts": "dY/dX",
        "eyexavgwts": "eY/eX",
        "eydxavgwts": "eY/dX",
        "dyexavgwts": "dY/eX",
        "ratio": "{hi} / {lo}",
        "ratioavg": "{hi} / {lo}",
        "ratioavgwts": "{hi} / {lo}",
        "lnratio": "ln({hi} / {lo})",
        "lnratioavg": "ln({hi} / {lo})",
        "lnratioavgwts": "ln({hi} / {lo})",
        "lnor": "ln(odds({hi}) / odds({lo}))",
        "lnoravg": "ln(odds({hi}) / odds({lo}))",
        "lnoravgwts": "ln(odds({hi}) / odds({lo}))",
        "lift": "lift",
        "liftavg": "liftavg",
        "expdydx": "exp(dY/dX)",
    }

    assert out in lab.keys(), (
        f"`comparison` must be one of: {', '.join(list(lab.keys()))}."
    )

    return (out, lab[out])


HiLo = namedtuple("HiLo", ["variable", "hi", "lo", "lab", "pad", "comparison"])


def clean_global(k, n):
    if (
        not isinstance(k, list)
        and not isinstance(k, pl.Series)
        and not isinstance(k, np.ndarray)
    ):
        out = [k]
    if not isinstance(k, list) or len(k) == 1:
        out = pl.Series(np.repeat(k, n))
    else:
        out = pl.Series(k)
    return out


def get_one_variable_hi_lo(
    model, variable, value, newdata, comparison, eps, by, wts=None, modeldata=None
):
    msg = "`value` must be a numeric, a list of length two, or 'sd'"
    vartype = model.get_variable_type(variable)

    def clean(k):
        return clean_global(k, newdata.shape[0])

    elasticities = [
        "eyexavg",
        "dyexavg",
        "eydxavg",
        "dydxavg",
        "eyex",
        "dyex",
        "eydx",
        "dydx",
    ]

    # default
    if value is None:
        # derivatives are not supported for character or boolean variables
        if vartype not in ["integer", "numeric"]:
            value = "reference"
            if comparison in ["eyexavg", "dyexavg", "eydxavg", "dydxavg"]:
                comparison = "differenceavg"
            elif comparison in ["eyex", "dyex", "eydx", "dydx"]:
                comparison = "difference"
        else:
            if comparison in elasticities:
                value = eps
            else:
                value = 1

    comparison, lab = sanitize_comparison(comparison, by, wts)

    if vartype == "boolean":
        hi = clean(True)
        lo = clean(False)
        lab = lab.format(hi="True", lo="False")
        out = HiLo(
            variable=variable, hi=hi, lo=lo, lab=lab, comparison=comparison, pad=None
        )
        return [out]

    if vartype == "binary":
        # allows 0 - 1 when manually specified
        if isinstance(value, list) and len(value) == 2:
            hi = clean(value[1])
            lo = clean(value[0])
        else:
            hi = clean(1)
            lo = clean(0)
        lab = lab.format(hi=str(hi[0]), lo=str(lo[0]))
        out = HiLo(
            variable=variable, hi=hi, lo=lo, lab=lab, comparison=comparison, pad=None
        )
        return [out]

    if vartype == "character":
        if isinstance(value, list) and len(value) == 2:
            hi = clean([value[1]])
            lo = clean([value[0]])
            lab = lab.format(hi=value[1], lo=value[0])
            out = HiLo(
                variable=variable,
                hi=hi,
                lo=lo,
                lab=lab,
                comparison=comparison,
                pad=None,
            )
            return [out]

        elif isinstance(value, str):
            out = get_categorical_combinations(
                variable=variable,
                uniqs=modeldata[variable].unique().sort(),
                newdata=newdata,
                lab=lab,
                combo=value,
                comparison=comparison,
            )
            return out

        else:
            raise ValueError(msg)

    if vartype in ["integer", "numeric"]:
        if isinstance(value, str):
            if value == "sd":
                value = modeldata[variable].std()
                hi = newdata[variable] + value / 2
                lo = newdata[variable] - value / 2
                lab = lab.format(hi="(x+sd/2)", lo="(x-sd/2)")
            elif value == "2sd":
                value = modeldata[variable].std()
                hi = newdata[variable] + value
                lo = newdata[variable] - value
                lab = lab.format(hi="(x+sd)", lo="(x-sd)")
            elif value == "iqr":
                hi = np.percentile(modeldata[variable], 75)
                lo = np.percentile(modeldata[variable], 25)
                lab = lab.format(hi="Q3", lo="Q1")
            elif value == "minmax":
                hi = modeldata[variable].max()
                lo = modeldata[variable].min()
                lab = lab.format(hi="Max", lo="Min")
            else:
                raise ValueError(msg)

        elif isinstance(value, list):
            if len(value) != 2:
                raise ValueError(msg)
            hi = clean([value[1]])
            lo = clean([value[0]])
            lab = lab.format(hi=value[1], lo=value[0])

        elif isinstance(value, (int, float)):
            if comparison not in elasticities:
                lab = f"+{value}"
            hi = newdata[variable] + value / 2
            lo = newdata[variable] - value / 2

        elif callable(value):
            tmp = value(newdata[variable])
            assert tmp.shape[1] == 2, (
                f"The function passed to `variables` must return a DataFrame with two columns. Got {tmp.shape[1]}."
            )
            lo = tmp[:, 0]
            hi = tmp[:, 1]
            lab = "custom"

        else:
            raise ValueError(msg)

        out = [
            HiLo(
                variable=variable,
                lo=lo,
                hi=hi,
                lab=lab,
                pad=None,
                comparison=comparison,
            )
        ]
        return out

    raise ValueError(msg)


def get_categorical_combinations(
    variable, uniqs, newdata, comparison, lab, combo="reference"
):
    def clean(k):
        return clean_global(k, newdata.shape[0])

    if not isinstance(combo, str):
        raise ValueError("The 'variables' value must be a string.")

    if len(uniqs) > 25:
        raise ValueError("There are too many unique categories to compute comparisons.")

    out = []

    if combo == "reference":
        for u in uniqs:
            if u != uniqs[0]:
                hl = HiLo(
                    variable=variable,
                    hi=clean([u]),
                    lo=clean([uniqs[0]]),
                    lab=lab.format(hi=u, lo=uniqs[0]),
                    pad=uniqs,
                    comparison=comparison,
                )
                out.append(hl)
    elif combo == "revreference":
        last_element = uniqs[-1]
        for u in uniqs:
            if u != last_element:
                hl = HiLo(
                    variable=variable,
                    hi=clean([u]),
                    lo=clean([last_element]),
                    lab=lab.format(hi=u, lo=last_element),
                    comparison=comparison,
                    pad=uniqs,
                )
                out.append(hl)
    elif combo == "sequential":
        for i in range(len(uniqs) - 1):
            hl = HiLo(
                variable=variable,
                hi=clean([uniqs[i + 1]]),
                lo=clean([uniqs[i]]),
                lab=lab.format(hi=uniqs[i + 1], lo=uniqs[i]),
                comparison=comparison,
                pad=uniqs,
            )
            out.append(hl)
    elif combo == "revsequential":
        for i in range(len(uniqs) - 1, 0, -1):
            hl = HiLo(
                variable=variable,
                hi=clean([uniqs[i - 1]]),
                lo=clean([uniqs[i]]),
                lab=lab.format(hi=uniqs[i - 1], lo=uniqs[i]),
                comparison=comparison,
                pad=uniqs,
            )
            out.append(hl)
    elif combo == "pairwise":
        for i in range(len(uniqs)):
            for j in range(i + 1, len(uniqs)):
                hl = HiLo(
                    variable=variable,
                    hi=clean([uniqs[j]]),
                    lo=clean([uniqs[i]]),
                    lab=lab.format(hi=uniqs[j], lo=uniqs[i]),
                    comparison=comparison,
                    pad=uniqs,
                )
                out.append(hl)
    elif combo == "all":
        for i in range(len(uniqs)):
            for j in range(len(uniqs)):
                if i == j:
                    continue
                hl = HiLo(
                    variable=variable,
                    hi=clean([uniqs[j]]),
                    lo=clean([uniqs[i]]),
                    lab=lab.format(hi=uniqs[j], lo=uniqs[i]),
                    comparison=comparison,
                    pad=uniqs,
                )
                out.append(hl)
    elif combo == "revpairwise":
        for i in range(len(uniqs)):
            for j in range(i + 1, len(uniqs)):
                hl = HiLo(
                    variable=variable,
                    hi=clean([uniqs[i]]),
                    lo=clean([uniqs[j]]),
                    lab=lab.format(hi=uniqs[i], lo=uniqs[j]),
                    comparison=comparison,
                    pad=uniqs,
                )
                out.append(hl)
    else:
        raise ValueError(
            "The supported comparisons are: 'reference', 'revreference', 'sequential', "
            "'revsequential', 'pairwise', 'revpairwise', and 'all'."
        )

    return out


def _get_cross_factorial_combinations(
    variables, model, newdata, comparison, eps, by, wts, modeldata
):
    """Create HiLo objects for cross comparisons with factorial grid combinations."""
    from itertools import product

    def clean(k):
        return clean_global(k, newdata.shape[0])

    # Get unique levels for each variable
    var_levels = {}
    var_names = []
    var_combos = {}  # Store the comparison type for each variable

    for var_name, var_value in variables.items():
        if var_name not in newdata.columns:
            continue

        var_names.append(var_name)
        var_combos[var_name] = var_value

        # Get unique levels from modeldata
        uniqs = modeldata[var_name].unique().sort()
        var_levels[var_name] = uniqs.to_list()

    # Create factorial grid
    level_lists = [var_levels[vn] for vn in var_names]
    grid_combinations = list(product(*level_lists))

    comparison_obj, lab = sanitize_comparison(comparison, by, wts)

    out = []

    # For factorial grids, use pairwise logic (C(n,2) unique unordered pairs)
    # This applies to both "pairwise" and "all" to keep comparisons manageable
    for i in range(len(grid_combinations)):
        for j in range(i + 1, len(grid_combinations)):
            hi_vals = grid_combinations[j]
            lo_vals = grid_combinations[i]

            # Create label showing which variables differ
            lab_parts = [
                f"{var_names[k]}{hi_vals[k]} - {var_names[k]}{lo_vals[k]}"
                for k in range(len(var_names))
            ]
            combined_lab = " ".join(lab_parts)

            # Create HiLo with all variables set
            # Store var_names as a tuple in the variable field to signal this is a grid comparison
            hl = HiLo(
                variable=tuple(
                    var_names
                ),  # Tuple of variable names signals factorial grid
                hi=clean(list(hi_vals)),
                lo=clean(list(lo_vals)),
                lab=combined_lab,
                comparison=comparison_obj,
                pad=None,
            )
            out.append(hl)

    return out


def sanitize_variables(
    variables, model, newdata, comparison, eps, by, wts=None, cross=False
):
    out = []

    modeldata = model.get_modeldata()

    # For cross=True with dict variables using "all" or "pairwise",
    # create factorial grid combinations
    if cross and isinstance(variables, dict):
        # Check if any variables use "all" or "pairwise"
        has_multi_comparisons = any(
            v in ["all", "pairwise", "revpairwise", "sequential", "revsequential"]
            for v in variables.values()
            if isinstance(v, str)
        )

        if has_multi_comparisons and len(variables) > 1:
            return _get_cross_factorial_combinations(
                variables, model, newdata, comparison, eps, by, wts, modeldata
            )

    if variables is None:
        vlist = model.find_predictors()
        if vlist is None:
            raise ValueError(
                "No predictors could be extracted from the model object. Please specify the `variables` argument."
            )
        vlist.sort()
        for v in vlist:
            out.append(
                get_one_variable_hi_lo(
                    model,
                    v,
                    None,
                    newdata,
                    comparison,
                    eps,
                    by,
                    wts,
                    modeldata=modeldata,
                )
            )

    elif isinstance(variables, dict):
        for v in variables:
            if v not in newdata.columns:
                del variables[v]
                warn(f"Variable {v} is not in newdata.")
            else:
                out.append(
                    get_one_variable_hi_lo(
                        model,
                        v,
                        variables[v],
                        newdata,
                        comparison,
                        eps,
                        by,
                        wts,
                        modeldata=modeldata,
                    )
                )

    elif isinstance(variables, str):
        if variables not in newdata.columns:
            raise ValueError(f"Variable {variables} is not in newdata.")
        out.append(
            get_one_variable_hi_lo(
                model,
                variables,
                None,
                newdata,
                comparison,
                eps,
                by,
                wts,
                modeldata=modeldata,
            )
        )

    elif isinstance(variables, list):
        for v in variables:
            if v not in newdata.columns:
                warn(f"Variable {v} is not in newdata.")
            else:
                out.append(
                    get_one_variable_hi_lo(
                        model,
                        v,
                        None,
                        newdata,
                        comparison,
                        eps,
                        by,
                        wts,
                        modeldata=modeldata,
                    )
                )

    # unnest list of list of HiLo
    out = [item for sublist in out for item in sublist]

    return out


def _is_ratio_formula(hypothesis):
    lhs = None
    if isinstance(hypothesis, str) and "~" in hypothesis:
        lhs = hypothesis.split("~", 1)[0]
    elif hasattr(hypothesis, "lhs"):
        lhs = hypothesis.lhs
    if lhs is None:
        return False
    if not isinstance(lhs, str):
        lhs = str(lhs)
    return lhs.strip().lower() == "ratio"


def sanitize_hypothesis_null(hypothesis):
    if isinstance(hypothesis, (int, float)):
        hypothesis_null = hypothesis
    elif _is_ratio_formula(hypothesis):
        hypothesis_null = 1
    elif isinstance(hypothesis, (list, tuple)):
        # Align with the R behavior: only switch the null to 1 when *all* formula-style
        # hypotheses in the sequence are ratio contrasts. Mixed contrast types retain
        # the default null of 0 to avoid shifting difference-based tests.
        formula_like = [
            hyp
            for hyp in hypothesis
            if hasattr(hyp, "lhs") or (isinstance(hyp, str) and "~" in hyp)
        ]
        ratio_formulas = [hyp for hyp in hypothesis if _is_ratio_formula(hyp)]
        if ratio_formulas and len(ratio_formulas) == len(formula_like):
            hypothesis_null = 1
        else:
            hypothesis_null = 0
    else:
        hypothesis_null = 0
    return hypothesis_null
