import narwhals as nw
import numpy as np
import polars as pl
from typing import Callable, Optional, Protocol, runtime_checkable, Union, List
from pydantic import ConfigDict, validate_call
from functools import wraps
from .equivalence import get_equivalence
from .result import MarginaleffectsResult
from .transform import get_transform
# from narwhals.typing import IntoFrame


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


def validate_string_columns(columns, modeldata, context=""):
    """
    Validate that specified columns are not String type.

    Parameters
    ----------
    columns : list[str] or str or dict or bool
        Column name(s) to validate
    modeldata : pl.DataFrame
        The model data containing the columns
    context : str
        Description of where validation is happening (for error messages)
    """
    # Handle different input types
    if columns is None or columns is False:
        return

    if isinstance(columns, str):
        columns = [columns]
    elif isinstance(columns, dict):
        columns = list(columns.keys())
    elif isinstance(columns, bool):
        return  # by=False or by=True shouldn't validate

    # Validate each column
    for col in columns:
        if col in ["index", "rownames"]:
            continue

        if col not in modeldata.columns:
            continue

        if modeldata[col].dtype in [pl.Utf8, pl.String]:
            msg = (
                f"Column '{col}' has String type and is used in {context}. "
                f"String columns are not allowed. "
                f"Please convert to Categorical or Enum.\n\n"
                f"For Polars DataFrames:\n"
                f"  df = df.with_columns(pl.col('{col}').cast(pl.Categorical))\n\n"
                f"For pandas DataFrames:\n"
                f"  df['{col}'] = df['{col}'].astype('category')"
            )
            raise TypeError(msg)


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


def get_type_dictionary(formula=None, modeldata=None):
    out = dict()
    if formula is None or callable(formula):
        variables = modeldata.columns
    else:
        from . import formulaic_utils as fml

        variables = fml.parse_variables(formula)
    for v in variables:
        t_i = [
            pl.Int8,
            pl.Int16,
            pl.Int32,
            pl.Int64,
            pl.UInt8,
            pl.UInt16,
            pl.UInt32,
            pl.UInt64,
        ]
        t_c = [pl.Utf8, pl.Categorical, pl.Enum]
        t_n = [pl.Float32, pl.Float64]
        t_b = [pl.Boolean]
        if modeldata[v].dtype in t_i:
            if modeldata[v].is_in([0, 1]).all():
                out[v] = "binary"
            else:
                out[v] = "integer"
        elif modeldata[v].dtype in t_n:
            if modeldata[v].is_in([0, 1]).all():
                out[v] = "binary"
            else:
                out[v] = "numeric"
        elif modeldata[v].dtype in t_c:
            out[v] = "character"
        elif modeldata[v].dtype in t_b:
            out[v] = "boolean"
        else:
            out[v] = "unknown"
    return out


def validate_types(func):
    """Decorator that validates types with arbitrary types allowed"""
    validator = validate_call(config=ConfigDict(arbitrary_types_allowed=True))(func)

    @wraps(func)
    def wrapper(*args, **kwargs):
        return validator(*args, **kwargs)

    return wrapper


def get_dataset_search(search: str):
    """Internal function to search available datasets"""
    try:
        index = pl.read_csv(
            "https://vincentarelbundock.github.io/Rdatasets/datasets.csv"
        )
        index = index.filter(
            index["Package"].str.contains(search)
            | index["Item"].str.contains(search)
            | index["Title"].str.contains(search)
        )
        return index.select(["Package", "Item", "Title", "Rows", "Cols", "CSV"])
    except BaseException as e:
        raise ValueError(f"Error searching dataset: {e}")


def get_dataset(
    dataset: str = "thornton",
    package: str = None,
    docs: bool = False,
    search: str = None,
):
    """
    Download and read a dataset as a Polars DataFrame from the `marginaleffects` or from the list at https://vincentarelbundock.github.io/Rdatasets/.

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(get_dataset)`
    """
    if search:
        return get_dataset_search(search)

    datasets = {
        "affairs": "https://marginaleffects.com/data/affairs",
        "airbnb": "https://marginaleffects.com/data/airbnb",
        "ces_demographics": "https://marginaleffects.com/data/ces_demographics",
        "ces_survey": "https://marginaleffects.com/data/ces_survey",
        "immigration": "https://marginaleffects.com/data/immigration",
        "lottery": "https://marginaleffects.com/data/lottery",
        "military": "https://marginaleffects.com/data/military",
        "thornton": "https://marginaleffects.com/data/thornton",
        "factorial_01": "https://marginaleffects.com/data/factorial_01",
        "interaction_01": "https://marginaleffects.com/data/interaction_01",
        "interaction_02": "https://marginaleffects.com/data/interaction_02",
        "interaction_03": "https://marginaleffects.com/data/interaction_03",
        "interaction_04": "https://marginaleffects.com/data/interaction_04",
        "polynomial_01": "https://marginaleffects.com/data/polynomial_01",
        "polynomial_02": "https://marginaleffects.com/data/polynomial_02",
    }

    # If package is None, try to guess the correct source
    if package is None:
        # First check if it's a marginaleffects dataset
        if dataset in datasets:
            package = "marginaleffects"
        else:
            # Try to find exact match in Rdatasets
            matches = get_dataset_search(f"^{dataset}$")
            if len(matches) == 1:
                package = matches["Package"][0]
                dataset = matches["Item"][0]
            elif len(matches) > 1:
                options = "\n".join(
                    [
                        f"  - {p}::{i}"
                        for p, i in zip(matches["Package"], matches["Item"])
                    ]
                )
                msg = f"Multiple matches found for dataset '{dataset}'. Please specify the package name.\nAvailable options:\n{options}"
                raise ValueError(msg)
            else:
                msg = f"Dataset '{dataset}' not found. Please:\n1. Specify the package name, or\n2. Use get_dataset(search='...') to search available datasets"
                raise ValueError(msg)

    try:
        if package == "marginaleffects":
            if dataset not in datasets:
                raise ValueError(
                    f"Dataset '{dataset}' is not available in the 'marginaleffects' package."
                )

            base_url = datasets[dataset]
            df = pl.read_parquet(f"{base_url}.parquet")
            if (
                "factorial" in dataset
                or "interaction" in dataset
                or "polynomial" in dataset
            ):
                doc_url = "https://marginaleffects.com/data/model_to_meaning_simulated_data.html"
            elif dataset.startswith("ces"):
                doc_url = "https://marginaleffects.com/data/ces.html"
            else:
                doc_url = f"{base_url}.html"
        else:
            parquet_url = f"https://vincentarelbundock.github.io/Rdatasets/parquet/{package}/{dataset}.parquet"
            doc_url = f"https://vincentarelbundock.github.io/Rdatasets/doc/{package}/{dataset}.html"
            df = pl.read_parquet(parquet_url)

        if docs:
            return doc_url

        return df

    except BaseException as e:
        raise ValueError(f"Error reading dataset: {e}")


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


def sanitize_datagrid_factor(
    values: List, newdata_col: pl.Series, variable_type: dict, var_name: str
):
    """
    Sanitize factor values for datagrid, similar to R's sanitize_datagrid_factor.

    Parameters:
    -----------
    values : List
        Values to validate
    newdata_col : pl.Series
        Original column from data
    variable_type : dict
        Dictionary mapping variable names to types
    var_name : str
        Name of the variable

    Returns:
    --------
    List
        Validated and converted values
    """
    # Check if this should be treated as a categorical/factor
    if newdata_col.dtype == pl.Categorical or (
        var_name in variable_type and variable_type[var_name] == "categorical"
    ):
        # Get the categories/levels
        if newdata_col.dtype == pl.Categorical:
            levels = newdata_col.cat.get_categories().to_list()
        else:
            # For character data treated as categorical, get sorted unique values
            levels = sorted(newdata_col.unique().drop_nulls().to_list())

        # Convert values to string for comparison
        str_values = [str(v) for v in values if v is not None]

        # Check if all values are valid levels
        invalid_values = [v for v in str_values if v not in levels]
        if invalid_values:
            msg = f'The "{var_name}" element corresponds to a factor variable. The values entered must be one of the factor levels: {levels}.'
            raise ValueError(msg)

        # Return as categorical if original was categorical
        if newdata_col.dtype == pl.Categorical:
            return pl.Series(values).cast(pl.Categorical).to_list()
        else:
            return values

    return values


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


get_dataset.__doc__ = """
    # `get_dataset()`


    Download and read a dataset as a Polars DataFrame from the `marginaleffects` or from the list at https://vincentarelbundock.github.io/Rdatasets/.
    Returns documentation link if `docs` is True.

    ## Parameters

    `dataset`: (str) String. Name of the dataset to download.

     - marginaleffects archive: affairs, airbnb, ces_demographics, ces_survey, immigration, lottery, military, thornton, factorial_01, interaction_01, interaction_02, interaction_03, interaction_04, polynomial_01, polynomial_02
     - Rdatasets archive: The name of a dataset listed on the Rdatasets index. See the website or the search argument.

    `package`: (str, optional) The package to download the dataset from.

    `docs`: (bool, optional) If True, return the documentation URL instead of the dataset. Default is False.

    `search`: (str, optional) The string is a regular expression. Download the dataset index from Rdatasets; search the "Package", "Item", and "Title" columns; and return the matching rows.

    ## Returns
    (Union[str, pl.DataFrame])
    * A string representing the documentation URL if `docs` is True, or
        a Polars DataFrame containing the dataset if `docs` is False.

    ## Raises
    ValueError
    * If the dataset is not among the specified choices.

    ## Examples
    ```py
    get_dataset()
    get_dataset("Titanic", package="Stat2Data")
    get_dataset(search = "(?i)titanic)
    ```
    """
