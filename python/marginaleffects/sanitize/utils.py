import polars as pl
from typing import List
from pydantic import ConfigDict, validate_call
from functools import wraps


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


def get_type_dictionary(formula=None, modeldata=None):
    out = dict()
    if formula is None or callable(formula):
        variables = modeldata.columns
    else:
        from .. import formula as fml

        variables = fml.parse_variables(formula)
    variables = [v for v in variables if v in modeldata.columns]
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


def validate_types(func):
    """Decorator that validates types with arbitrary types allowed"""
    validator = validate_call(config=ConfigDict(arbitrary_types_allowed=True))(func)

    @wraps(func)
    def wrapper(*args, **kwargs):
        return validator(*args, **kwargs)

    return wrapper
