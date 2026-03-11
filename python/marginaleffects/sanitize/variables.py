from collections import namedtuple
from warnings import warn

import numpy as np
import polars as pl

from .comparison import sanitize_comparison


HiLo = namedtuple("HiLo", ["variable", "hi", "lo", "lab", "pad", "comparison"])


def _clean_global(k, n):
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


def _get_one_variable_hi_lo(
    model, variable, value, newdata, comparison, eps, by, wts=None, modeldata=None
):
    msg = "`value` must be a numeric, a list of length two, or 'sd'"
    vartype = model.get_variable_type(variable)

    def clean(k):
        return _clean_global(k, newdata.shape[0])

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
        from .categorical import get_categorical_combinations

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


def sanitize_variables(
    variables, model, newdata, comparison, eps, by, wts=None, cross=False
):
    out = []

    modeldata = model.get_modeldata()

    # For cross=True with dict variables using "all" or "pairwise",
    # create factorial grid combinations
    if cross and isinstance(variables, dict):
        from .categorical import _get_cross_factorial_combinations

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
                _get_one_variable_hi_lo(
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
                    _get_one_variable_hi_lo(
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
            _get_one_variable_hi_lo(
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
                    _get_one_variable_hi_lo(
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
