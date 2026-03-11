from collections import namedtuple
from warnings import warn

import numpy as np
import polars as pl

from .comparison import sanitize_comparison


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
