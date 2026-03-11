import numpy as np
import polars as pl

from ..datagrid import datagrid
from ..utils import ingest, upcast
from ..formula import listwise_deletion


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
