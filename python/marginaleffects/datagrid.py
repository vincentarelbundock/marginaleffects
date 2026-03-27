from functools import reduce, partial
from typing import Any, Dict, Union, List, Callable, Optional

import polars as pl

import marginaleffects.utils as ut


# Map variable types to function-defaults keys
_TYPE_TO_FUN_KEY = {
    "binary": "binary",
    "character": "character",
    "logical": "logical",
    "categorical": "categorical",
    "factor": "categorical",
    "numeric": "numeric",
    "integer": "numeric",
    "other": "other",
}


def _detect_variable_type(
    data: pl.DataFrame, model: Optional[Any] = None
) -> Dict[str, str]:
    variable_type: Dict[str, str] = {}

    for col in data.columns:
        dtype = data[col].dtype

        try:
            unique_vals = data[col].unique()
            n_unique = len(unique_vals.drop_nulls())
        except pl.exceptions.InvalidOperationError:
            variable_type[col] = "other"
            continue

        if n_unique == 2:
            variable_type[col] = "binary"
        elif dtype in (pl.Int64, pl.Int32, pl.Int16, pl.Int8):
            variable_type[col] = "integer"
        elif dtype in (pl.Float64, pl.Float32):
            variable_type[col] = "numeric"
        elif dtype == pl.Boolean:
            variable_type[col] = "logical"
        elif dtype == pl.Categorical:
            variable_type[col] = "categorical"
        elif dtype in (pl.Utf8, pl.String):
            variable_type[col] = "character"
        else:
            variable_type[col] = "other"

    return variable_type


def datagrid(
    model=None,
    newdata=None,
    by: Optional[Union[str, List[str]]] = None,
    grid_type="mean_or_mode",
    response: bool = False,
    FUN: Optional[Callable] = None,
    FUN_binary=None,
    FUN_character=None,
    FUN_factor=None,
    FUN_logical=None,
    FUN_numeric=None,
    FUN_integer=None,
    FUN_other=None,
    **kwargs,
):
    """
    Generate a data grid of user-specified values for use in ``predictions()``, ``comparisons()``, and ``slopes()``.

    This is useful to define where in the predictor space we want to evaluate the quantities of interest. Ex: the predicted outcome or slope for a 37 year old college graduate.

    Parameters
    ----------
    model : object, optional
        Model object. One and only one of the ``model`` and ``newdata`` arguments can be used.
    newdata : DataFrame, optional
        Data frame used to define the predictor space. One and only one of the ``model`` and ``newdata`` arguments can be used.
    grid_type : str, optional
        Determines the functions to apply to each variable. The defaults can be overridden by defining individual variables explicitly in the ``**kwargs``, or by supplying a function to one of the ``FUN_*`` arguments.

        - "mean_or_mode": Character, factor, logical, and binary variables are set to their modes. Numeric, integer, and other variables are set to their means.
        - "balanced": Each unique level of character, factor, logical, and binary variables are preserved. Numeric, integer, and other variables are set to their means. Warning: When there are many variables and many levels per variable, a balanced grid can be very large. In those cases, it is better to use ``grid_type="mean_or_mode"`` and to specify the unique levels of a subset of named variables explicitly.
        - "counterfactual": the entire dataset is duplicated for each combination of the variable values specified in ``**kwargs``. Variables not explicitly supplied to ``datagrid()`` are set to their observed values in the original dataset.
    FUN_numeric : callable, optional
        The function to be applied to numeric variables.
    FUN_other : callable, optional
        The function to be applied to other variable types.
    **kwargs
        Named arguments where the name is the variable name and the value is a list of values to use in the grid. If a variable is not specified, it is set to its mean or mode depending on the ``grid_type`` argument.

    Returns
    -------
    polars.DataFrame
        DataFrame where each row corresponds to one combination of the named predictors supplied by the user. Variables which are not explicitly defined are held at their mean or mode.

    Examples
    --------
    >>> import polars as pl
    >>> import statsmodels.formula.api as smf
    >>> from marginaleffects import *
    >>> data = get_dataset("thornton")
    >>> datagrid(newdata = data, village = [43, 11])
    >>> mod = smf.ols("outcome ~ incentive + distance", data).fit()
    >>> datagrid(model = mod, village = [43, 11])
    >>> nd = datagrid(mod, village = [43, 11])
    >>> slopes(mod, newdata = nd)
    >>> dg = datagrid(newdata = data, village = [43, 11], grid_type = "counterfactual")
    >>> dg.shape
    """

    # allow predictions() to pass `model` argument automatically
    if model is None and newdata is None:
        out = partial(
            datagrid,
            by=by,
            grid_type=grid_type,
            response=response,
            FUN=FUN,
            FUN_binary=FUN_binary,
            FUN_character=FUN_character,
            FUN_factor=FUN_factor,
            FUN_logical=FUN_logical,
            FUN_numeric=FUN_numeric,
            FUN_integer=FUN_integer,
            FUN_other=FUN_other,
            **kwargs,
        )
        return out

    # Validation
    valid_grid_types = ["mean_or_mode", "balanced", "counterfactual", "dataframe"]
    if not isinstance(grid_type, str) or grid_type not in valid_grid_types:
        raise ValueError(f"grid_type must be one of {valid_grid_types}")

    if by is not None:
        if isinstance(by, str):
            by = [by]
        elif not isinstance(by, list):
            raise ValueError("by must be a string or list of strings")

    if model is not None:
        from .sanitize.sanitize_model import sanitize_model

        model = sanitize_model(model)

    if newdata is None:
        if model is None:
            raise ValueError("One of model or newdata must be specified")
        newdata = model.get_modeldata()

    # Validate 'by' columns exist
    if by is not None:
        missing_cols = [col for col in by if col not in newdata.columns]
        if missing_cols:
            raise ValueError(
                f"The following 'by' columns are not in newdata: {missing_cols}"
            )

    if grid_type == "counterfactual":
        return _datagridcf(model=model, newdata=newdata, by=by, **kwargs)

    # Get variable types
    if model is not None:
        model_variable_type = model.get_variable_type()
        full_variable_type = _detect_variable_type(newdata)
        variable_type = {**full_variable_type, **model_variable_type}
    else:
        variable_type = _detect_variable_type(newdata)

    # Build function defaults, applying user overrides
    func_defaults = _get_function_defaults(grid_type, FUN)
    fun_overrides = {
        "binary": FUN_binary,
        "character": FUN_character,
        "categorical": FUN_factor,
        "logical": FUN_logical,
        "numeric": FUN_numeric,
        "other": FUN_other,
    }
    for vtype, fn in fun_overrides.items():
        if fn is not None:
            func_defaults[vtype] = fn

    # Handle grouping - split data by 'by' variables
    if by is None:
        newdata_split = [newdata]
        by_groups = [{}]
    else:
        by_df = newdata.select(by).unique().sort(by)
        newdata_split = []
        by_groups = []
        for i in range(by_df.height):
            group_filter = pl.lit(True)
            group_dict = {}
            for col in by:
                val = by_df[col][i]
                group_filter = group_filter & (pl.col(col) == val)
                group_dict[col] = val
            filtered_data = newdata.filter(group_filter)
            newdata_split.append(filtered_data)
            by_groups.append(group_dict)

    # Process each group
    all_results = []
    for group_data, group_dict in zip(newdata_split, by_groups):
        result = _process_datagrid_group(
            group_data,
            model,
            variable_type,
            func_defaults,
            grid_type,
            **kwargs,
        )

        for col, val in group_dict.items():
            if col not in result.columns:
                result = result.with_columns(pl.lit(val).alias(col))

        all_results.append(result)

    if len(all_results) == 1:
        out = all_results[0]
    else:
        out = pl.concat(all_results, how="vertical")

    out.datagrid_explicit = list(kwargs.keys())
    return out


def _normalize_grid_value(key, value, newdata, variable_type):
    """Normalize a single kwarg value to a list, with factor validation."""
    if value is None:
        return [None]

    if callable(value):
        if key not in newdata.columns:
            print(f"Warning: The variable '{key}' is not in the newdata.")
            return None
        result = value(newdata[key])
        if hasattr(result, "__iter__") and not isinstance(result, str):
            values = list(result)
        else:
            values = [result]
    elif isinstance(value, pl.Series):
        values = value.to_list()
    elif isinstance(value, (str, bytes)):
        values = [value]
    elif hasattr(value, "__iter__"):
        values = list(value)
    else:
        values = [value]

    # Factor validation
    if key in newdata.columns:
        original_variable_type = (
            {key: variable_type[key]} if key in variable_type else {}
        )
        values = ut.sanitize_datagrid_factor(
            values, newdata[key], original_variable_type, key
        )

    return values


def _process_datagrid_group(
    newdata,
    model,
    variable_type,
    func_defaults,
    grid_type,
    **kwargs,
):
    """Process datagrid for a single group (used when 'by' is specified)."""

    # Process explicit variables (from kwargs)
    explicit_values = {}
    for key, value in kwargs.items():
        result = _normalize_grid_value(key, value, newdata, variable_type)
        if result is not None:
            explicit_values[key] = result

    # Process implicit variables (not specified in kwargs)
    implicit_values = {}
    implicit_cols = [
        col
        for col in newdata.columns
        if col not in explicit_values and newdata[col].null_count() < newdata.height
    ]

    # For balanced grids, exclude response variable
    if grid_type == "balanced" and model is not None:
        response_cols = []
        if hasattr(model, "response_name") and isinstance(model.response_name, str):
            response_cols = [model.response_name]
        elif hasattr(model, "response_name") and isinstance(model.response_name, list):
            response_cols = model.response_name
        implicit_cols = [col for col in implicit_cols if col not in response_cols]

    for col in implicit_cols:
        vtype = variable_type.get(col, "other")
        fun_key = _TYPE_TO_FUN_KEY.get(vtype, "other")
        try:
            implicit_values[col] = func_defaults[fun_key](newdata[col])
        except Exception as e:
            print(
                f"Warning: Error applying function to column '{col}': {e}. Using fallback."
            )
            if fun_key == "numeric":
                implicit_values[col] = ut.mean_na(newdata[col])
            else:
                implicit_values[col] = ut.get_mode(newdata[col])

    # Convert to DataFrames, preserving Enum/Categorical dtypes
    all_values = {}
    for key, value in {**implicit_values, **explicit_values}.items():
        if not isinstance(value, list):
            value = [value]
        if key in newdata.columns:
            original_dtype = newdata[key].dtype
            if isinstance(original_dtype, pl.Enum):
                all_values[key] = pl.DataFrame(
                    {key: pl.Series(value, dtype=original_dtype)}
                )
            elif original_dtype == pl.Categorical:
                categories = newdata[key].unique().drop_nulls().to_list()
                enum_dtype = pl.Enum(categories)
                all_values[key] = pl.DataFrame(
                    {key: pl.Series(value, dtype=enum_dtype)}
                )
            else:
                all_values[key] = pl.DataFrame({key: value})
        else:
            all_values[key] = pl.DataFrame({key: value})

    # Assemble the grid
    if not all_values:
        return pl.DataFrame()

    if grid_type == "dataframe":
        # Column-wise binding — all vectors must have same length (or length 1)
        lengths = [len(df[df.columns[0]]) for df in all_values.values()]
        unique_lengths = set(length for length in lengths if length > 1)

        if len(unique_lengths) > 1:
            raise ValueError(
                'With grid_type="dataframe", the length of each vector must be 1 or be the same for every variable.'
            )

        max_length = max(lengths) if lengths else 0
        expanded_dfs = []
        for df in all_values.values():
            if len(df) == 1 and max_length > 1:
                expanded_dfs.append(pl.concat([df] * max_length))
            else:
                expanded_dfs.append(df)

        return pl.concat(expanded_dfs, how="horizontal")
    else:
        # Cross-product for balanced and mean_or_mode
        return reduce(lambda x, y: x.join(y, how="cross"), all_values.values())


def _get_function_defaults(grid_type, FUN=None):
    """Get default functions for each variable type based on grid_type."""

    if grid_type == "balanced":
        defaults = {
            "binary": ut.unique_s,
            "character": ut.unique_s,
            "categorical": ut.unique_s,
            "logical": ut.unique_s,
            "numeric": ut.mean_na,
            "other": ut.mean_na,
        }
    else:
        defaults = {
            "binary": ut.get_mode,
            "character": ut.get_mode,
            "categorical": ut.get_mode,
            "logical": ut.get_mode,
            "numeric": ut.mean_na,
            "other": ut.get_mode,
        }

    if FUN is not None:
        for key in defaults:
            defaults[key] = FUN

    return defaults


def _datagridcf(model, newdata, by=None, **kwargs):
    if "rowid" not in newdata.columns:
        newdata = newdata.with_columns(
            pl.Series(range(newdata.shape[0])).alias("rowid")
        )
    newdata = newdata.rename({"rowid": "rowidcf"})

    if model is not None:
        modeldata = model.get_modeldata()
    else:
        modeldata = newdata

    # Create dataframe from kwargs
    dfs = []
    for key, value in kwargs.items():
        if value is None:
            dfs.append(pl.DataFrame({key: [None]}))
        elif callable(value):
            dfs.append(pl.DataFrame({key: value(modeldata[key])}))
        else:
            dfs.append(pl.DataFrame({key: value}))

    df_cross = reduce(lambda df1, df2: df1.join(df2, how="cross"), dfs)

    columns_to_drop = [col for col in df_cross.columns if col in newdata.columns]
    if columns_to_drop:
        newdata = newdata.drop(columns_to_drop)

    result = newdata.join(df_cross, how="cross")
    result.datagrid_explicit = list(kwargs.keys())

    return result
