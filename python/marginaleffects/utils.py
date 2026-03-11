import narwhals as nw
import numpy as np
import polars as pl
from typing import Callable, Optional, Protocol, runtime_checkable, Union, List

from .sanitize.utils import validate_string_columns  # noqa: F401
from .sanitize.utils import get_type_dictionary  # noqa: F401
from .sanitize.utils import validate_types  # noqa: F401
from .sanitize.utils import sanitize_datagrid_factor  # noqa: F401


@runtime_checkable
class ArrowStreamExportable(Protocol):
    def __arrow_c_stream__(self, requested_schema: object | None = None) -> object: ...


def ingest(df: ArrowStreamExportable):
    """
    Convert any DataFrame to a Polars DataFrame.

    Parameters
    ----------
    df : ArrowStreamExportable
        The DataFrame to convert.

    Returns
    -------
    pl.DataFrame

    Notes
    -----

    If the original DataFrame was a pandas DataFrame, the index will
    be reset to ensure compatibility with linearmodels.
    """

    try:
        import pandas as pd

        if isinstance(df, pd.DataFrame):
            df = df.reset_index()
            # Note: String column validation happens later in validation.py
            # only for columns used in the model formula

    except ImportError:
        raise ValueError("Please install pandas to handle Pandas DataFrame as input.")

    return nw.from_arrow(df, backend=pl).to_native()


def sort_columns(df, by=None, newdata=None):
    cols = [
        "rowid",
        "group",
        "term",
        "contrast",
        "estimate",
        "std_error",
        "statistic",
        "p_value",
        "s_value",
        "conf_low",
        "conf_high",
    ] + df.columns

    if by is not None:
        if isinstance(by, list):
            cols = by + cols
        else:
            cols = [by] + cols

    if isinstance(newdata, pl.DataFrame) and hasattr(newdata, "datagrid_explicit"):
        cols = newdata.datagrid_explicit + cols

    cols = [x for x in cols if x in df.columns]
    cols_unique = []
    for item in cols:
        if item not in cols_unique:
            cols_unique.append(item)
    out = df.select(cols_unique)
    if "marginaleffects_comparison" in out.columns:
        out = out.drop("marginaleffects_comparison")
    return out


def pad_array(arr, n):
    if len(arr) == 1:
        out = np.repeat(arr[0], n)
    elif len(arr) < n:
        out = np.concatenate([np.repeat(arr[0], n - len(arr)), arr])
    else:
        out = arr
    return pl.Series(out)


def get_pad(df, colname, uniqs):
    if uniqs is None:
        return None
    first = [df.slice(0, 1)] * len(uniqs)
    first = pl.concat(first)
    first = first.with_columns(uniqs.alias(colname))
    return first


def upcast(df, reference):
    numtypes = [
        pl.UInt8,
        pl.UInt16,
        pl.UInt32,
        pl.Int8,
        pl.Int16,
        pl.Int32,
        pl.Int64,
        pl.UInt64,
        pl.Float32,
        pl.Float64,
    ]
    for col in df.columns:
        if col in df.columns and col in reference.columns:
            good = reference[col].dtype
            bad = df[col].dtype
            if good != bad:
                # numeric
                if good in numtypes and bad in numtypes:
                    idx = max(numtypes.index(good), numtypes.index(bad))
                    df = df.with_columns(pl.col(col).cast(numtypes[idx]))

                # string & cat
                elif good in [pl.Categorical, pl.Enum]:
                    categories = reference[col].cat.get_categories()
                    df = df.with_columns(pl.col(col).cast(pl.Enum(categories)))
                    reference = reference.with_columns(
                        pl.col(col).cast(pl.Enum(categories))
                    )

                else:
                    try:
                        df = df.with_columns(pl.col(col).cast(good))
                    except (
                        pl.exceptions.InvalidOperationError,
                        pl.exceptions.SchemaError,
                        pl.exceptions.ComputeError,
                    ) as e:
                        error_str = str(e)
                        # Handle various casting issues
                        if (
                            (
                                ("List type" in error_str or "List(" in str(good))
                                and ("Enum" in error_str or "UInt32" in error_str)
                            )
                            or ("cannot cast 'Object' type" in error_str)
                            or (
                                "cannot cast List type" in error_str
                                and "to: '" in error_str
                            )
                        ):
                            # Skip problematic type conversions
                            continue
                        else:
                            # Re-raise for all other errors
                            raise

    return df


def get_mode(series: pl.Series) -> Union[str, int, float, bool]:
    """
    Get the mode (most frequent value) of a Polars Series.

    Parameters:
    -----------
    series : pl.Series
        The series to find the mode for

    Returns:
    --------
    Union[str, int, float, bool]
        The most frequent value
    """
    if len(series) == 0:
        return None

    # Remove nulls and get value counts
    non_null = series.drop_nulls()
    if len(non_null) == 0:
        # If all values are null, return None but don't let it propagate
        # This is a fallback case
        return series.dtype.base_type() if hasattr(series.dtype, "base_type") else None

    mode_result = non_null.mode()
    if len(mode_result) > 0:
        return mode_result[0]
    else:
        # Fallback to first non-null value if mode fails
        return non_null[0] if len(non_null) > 0 else None


def mean_i(series: pl.Series) -> int:
    """
    Calculate mean and round to integer, similar to R's mean_i.

    Parameters:
    -----------
    series : pl.Series
        The series to calculate mean for

    Returns:
    --------
    int
        Rounded mean value
    """
    return int(round(series.drop_nulls().mean()))


def mean_na(series: pl.Series) -> float:
    """
    Calculate mean with NA removal, similar to R's mean_na.

    Parameters:
    -----------
    series : pl.Series
        The series to calculate mean for

    Returns:
    --------
    float
        Mean value with nulls removed
    """
    return series.drop_nulls().mean()


def unique_s(series: pl.Series) -> List:
    """
    Get unique values sorted, similar to R's unique_s.

    Parameters:
    -----------
    series : pl.Series
        The series to get unique values from

    Returns:
    --------
    List
        Sorted list of unique values
    """
    unique_vals = series.unique().drop_nulls().sort()
    return unique_vals.to_list()


def finalize_result(
    out,
    *,
    model,
    by,
    transform,
    equivalence,
    newdata,
    conf_level,
    J,
    equivalence_df: Optional[float] = None,
    postprocess: Optional[Callable] = None,
):
    """
    Shared helper to apply final transforms and wrap a MarginaleffectsResult.
    """
    from .test.equivalence import get_equivalence
    from .classes import MarginaleffectsResult
    from .transform import get_transform

    out = get_transform(out, transform=transform)
    if equivalence_df is None:
        out = get_equivalence(out, equivalence=equivalence)
    else:
        out = get_equivalence(out, equivalence=equivalence, df=equivalence_df)
    out = sort_columns(out, by=by, newdata=newdata)
    if postprocess is not None:
        out = postprocess(out)
    return MarginaleffectsResult(
        out, by=by, conf_level=conf_level, jacobian=J, newdata=newdata
    )


def call_avg(func, *, model, newdata=None, **kwargs):
    """
    Shared wrapper for avg_* helpers resolving callable newdata hooks.
    """
    if callable(newdata):
        newdata = newdata(model)
    return func(model=model, newdata=newdata, **kwargs)


def prepare_base_inputs(
    model,
    vcov,
    by,
    newdata,
    wts,
    hypothesis,
    *,
    enforce_pyfixest_warning: bool = True,
):
    """
    Shared helper to sanitize model, newdata, by, and hypothesis inputs.
    """
    from warnings import warn
    from .sanitize import sanitize_model
    from .sanitize import (
        sanitize_by,
        sanitize_hypothesis_null,
        sanitize_newdata,
        sanitize_vcov,
    )
    from .pyfixest import ModelPyfixest

    if callable(newdata):
        newdata = newdata(model)

    if not hasattr(model, "get_modeldata"):
        model = sanitize_model(model)

    if (
        enforce_pyfixest_warning
        and isinstance(model, ModelPyfixest)
        and vcov is not False
    ):
        has_fixef = getattr(model.model, "_has_fixef", False)
        if has_fixef:
            warn(
                "For this pyfixest model, marginaleffects cannot take into account the "
                "uncertainty in fixed-effects parameters. Standard errors are disabled "
                "and vcov=False is enforced.",
                UserWarning,
                stacklevel=2,
            )
        else:
            warn(
                "Standard errors are not available for predictions in pyfixest models. "
                "Setting vcov=False automatically.",
                UserWarning,
                stacklevel=2,
            )
        vcov = False

    by = sanitize_by(by)
    V = sanitize_vcov(vcov, model)
    newdata = sanitize_newdata(model, newdata, wts=wts, by=by)
    hypothesis_null = sanitize_hypothesis_null(hypothesis)

    modeldata = model.get_modeldata()
    validate_string_columns(by, modeldata, context="the 'by' parameter")

    return model, by, V, newdata, hypothesis_null, modeldata
