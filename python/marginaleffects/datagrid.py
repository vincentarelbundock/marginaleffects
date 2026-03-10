from functools import reduce, partial
from typing import Union, List, Callable, Optional

import polars as pl

from .sanitize_model import sanitize_model
import marginaleffects.utils as ut
from .classes import _detect_variable_type, _check_variable_type


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
    Generate a data grid of user-specified values for use in the 'newdata' argument of the 'predictions()', 'comparisons()', and 'slopes()' functions.

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(datagrid)`
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

    if model is None and newdata is None:
        raise ValueError("One of model or newdata must be specified")

    if model is not None:
        # Validate and sanitize model - this will raise ValueError for unsupported types
        model = sanitize_model(model)

    if newdata is None:
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
        # Also detect types for all columns in newdata to handle columns not in model
        full_variable_type = _detect_variable_type(newdata)
        # Merge the two, preferring model types for variables in the model
        variable_type = {**full_variable_type, **model_variable_type}
    else:
        variable_type = _detect_variable_type(newdata)

    # Pre-compute variable type mappings to avoid repetitive checks
    type_mapping = _compute_variable_type_mapping(variable_type, newdata.columns)

    # Handle grouping - split data by 'by' variables
    if by is None:
        newdata_split = [newdata]
        by_groups = [{}]
    else:
        # Get unique combinations of 'by' variables
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
            type_mapping,
            grid_type,
            response,
            FUN,
            FUN_binary,
            FUN_character,
            FUN_factor,
            FUN_logical,
            FUN_numeric,
            FUN_integer,
            FUN_other,
            **kwargs,
        )

        # Add grouping columns back to result
        for col, val in group_dict.items():
            if col not in result.columns:
                result = result.with_columns(pl.lit(val).alias(col))

        all_results.append(result)

    # Combine all results
    if len(all_results) == 1:
        out = all_results[0]
    else:
        out = pl.concat(all_results, how="vertical")

    # Add rowid if not present (only for internal use, not for public API)
    # Commenting out to maintain compatibility with existing tests
    # if "rowid" not in out.columns and out.height > 0:
    #     out = out.with_columns(pl.Series(range(out.height)).alias("rowid"))

    out.datagrid_explicit = list(kwargs.keys())
    return out


def _process_datagrid_group(
    newdata,
    model,
    type_mapping,
    grid_type,
    response,
    FUN,
    FUN_binary,
    FUN_character,
    FUN_factor,
    FUN_logical,
    FUN_numeric,
    FUN_integer,
    FUN_other,
    **kwargs,
):
    """Process datagrid for a single group (used when 'by' is specified)."""

    # Set up function defaults based on grid_type and FUN parameter
    func_defaults = _get_function_defaults(grid_type, FUN)

    # Override with specific function parameters if provided
    if FUN_binary is not None:
        func_defaults["FUN_binary"] = FUN_binary
    if FUN_character is not None:
        func_defaults["FUN_character"] = FUN_character
    if FUN_factor is not None:
        func_defaults["FUN_factor"] = FUN_factor
    if FUN_logical is not None:
        func_defaults["FUN_logical"] = FUN_logical
    if FUN_numeric is not None:
        func_defaults["FUN_numeric"] = FUN_numeric
    if FUN_integer is not None:
        func_defaults["FUN_integer"] = FUN_integer
    if FUN_other is not None:
        func_defaults["FUN_other"] = FUN_other

    # Process explicit variables (from kwargs)
    explicit_values = {}
    for key, value in kwargs.items():
        # Handle None specially to create null columns (similar to R's NA)
        if value is None:
            explicit_values[key] = [None]
        elif callable(value):
            if key not in newdata.columns:
                print(f"Warning: The variable '{key}' is not in the newdata.")
                continue
            result = value(newdata[key])
            # Handle numpy arrays and similar
            if hasattr(result, "__iter__") and not isinstance(result, str):
                explicit_values[key] = list(result)
            else:
                explicit_values[key] = [result]
        else:
            # Handle Polars Series (e.g., from modeldata[col].unique())
            if isinstance(value, pl.Series):
                explicit_values[key] = value.to_list()
            # Handle iterables like range, but not strings
            elif (
                hasattr(value, "__iter__")
                and not isinstance(value, (str, bytes))
                and not isinstance(value, list)
            ):
                explicit_values[key] = list(value)
            elif not isinstance(value, list):
                value = [value]
                explicit_values[key] = value
            else:
                explicit_values[key] = value

        # Validate factors - need to reconstruct variable_type for this column
        # Skip validation for None values as they don't need factor validation
        if value is not None and key in newdata.columns:
            # For factor validation, we need the original variable_type dict
            original_variable_type = (
                {key: type_mapping[key]["original_type"]} if key in type_mapping else {}
            )
            explicit_values[key] = ut.sanitize_datagrid_factor(
                explicit_values[key], newdata[key], original_variable_type, key
            )

    # Process implicit variables (not specified in kwargs)
    implicit_values = {}
    implicit_cols = [
        col for col in newdata.columns if col not in explicit_values.keys()
    ]

    # Remove columns that are entirely null to avoid downstream issues
    implicit_cols = [
        col for col in implicit_cols if newdata[col].null_count() < newdata.height
    ]

    # For balanced grids, exclude response variable to avoid duplication
    if grid_type == "balanced" and model is not None:
        response_cols = []
        if hasattr(model, "response_name") and isinstance(model.response_name, str):
            response_cols = [model.response_name]
        elif hasattr(model, "response_name") and isinstance(model.response_name, list):
            response_cols = model.response_name
        implicit_cols = [col for col in implicit_cols if col not in response_cols]

    # Apply functions to implicit variables
    for col in implicit_cols:
        try:
            col_mapping = type_mapping.get(col, {})

            if col_mapping.get("is_binary", False):
                result = func_defaults["FUN_binary"](newdata[col])
            elif col_mapping.get("is_character", False):
                result = func_defaults["FUN_character"](newdata[col])
            elif col_mapping.get("is_logical", False):
                result = func_defaults["FUN_logical"](newdata[col])
            elif col_mapping.get("is_categorical", False) or col_mapping.get(
                "is_factor", False
            ):
                result = func_defaults["FUN_factor"](newdata[col])
            elif col_mapping.get("is_numeric", False) or col_mapping.get(
                "is_integer", False
            ):
                # Both numeric and integer types use FUN_numeric for compatibility
                result = func_defaults["FUN_numeric"](newdata[col])
            else:
                result = func_defaults["FUN_other"](newdata[col])

            implicit_values[col] = result
        except Exception as e:
            # Fallback: use appropriate default function for the column type
            print(
                f"Warning: Error applying function to column '{col}': {e}. Using fallback."
            )
            col_mapping = type_mapping.get(col, {})
            if col_mapping.get("is_numeric", False) or col_mapping.get(
                "is_integer", False
            ):
                implicit_values[col] = ut.mean_na(newdata[col])
            else:
                implicit_values[col] = ut.get_mode(newdata[col])

    # Convert to DataFrames for joining
    all_values = {}
    for key, value in {**implicit_values, **explicit_values}.items():
        if not isinstance(value, list):
            value = [value]
        # Create DataFrame - preserve Enum/Categorical dtypes from original data
        # This is crucial for patsy to create correct design matrices
        if key in newdata.columns:
            original_dtype = newdata[key].dtype
            # Preserve Enum dtype with its categories
            if isinstance(original_dtype, pl.Enum):
                all_values[key] = pl.DataFrame(
                    {key: pl.Series(value, dtype=original_dtype)}
                )
            # For Categorical, convert to Enum to preserve all categories
            elif original_dtype == pl.Categorical:
                # Get all unique categories from the original data
                # Don't sort - preserve the order as it appears in the data
                categories = newdata[key].unique().drop_nulls().to_list()
                enum_dtype = pl.Enum(categories)
                all_values[key] = pl.DataFrame(
                    {key: pl.Series(value, dtype=enum_dtype)}
                )
            else:
                all_values[key] = pl.DataFrame({key: value})
        else:
            all_values[key] = pl.DataFrame({key: value})

    # Create the grid based on grid_type
    if grid_type == "dataframe":
        # Column-wise binding - all vectors must have same length (or length 1)
        lengths = [len(df[df.columns[0]]) for df in all_values.values()]
        unique_lengths = set(length for length in lengths if length > 1)

        if len(unique_lengths) > 1:
            raise ValueError(
                'With grid_type="dataframe", the length of each vector must be 1 or be the same for every variable.'
            )

        # Combine by rows (column-wise binding)
        max_length = max(lengths) if lengths else 0
        expanded_dfs = []
        for key, df in all_values.items():
            if len(df) == 1 and max_length > 1:
                # Repeat single values to match max length
                expanded = pl.concat([df] * max_length)
                expanded_dfs.append(expanded)
            else:
                expanded_dfs.append(df)

        if expanded_dfs:
            out = pl.concat(expanded_dfs, how="horizontal")
        else:
            out = pl.DataFrame()
    else:
        # Cross-product for balanced and mean_or_mode
        if all_values:
            out = reduce(lambda x, y: x.join(y, how="cross"), all_values.values())
        else:
            out = pl.DataFrame()

    return out


def _get_function_defaults(grid_type, FUN=None):
    """Get default functions for each variable type based on grid_type."""

    if grid_type == "balanced":
        defaults = {
            "FUN_binary": ut.unique_s,
            "FUN_character": ut.unique_s,
            "FUN_factor": ut.unique_s,
            "FUN_logical": ut.unique_s,
            "FUN_numeric": ut.mean_na,
            "FUN_integer": ut.mean_i,
            "FUN_other": ut.mean_na,
        }
    elif grid_type in ["mean_or_mode", "dataframe"]:
        defaults = {
            "FUN_binary": ut.get_mode,
            "FUN_character": ut.get_mode,
            "FUN_factor": ut.get_mode,
            "FUN_logical": ut.get_mode,
            "FUN_numeric": ut.mean_na,
            "FUN_integer": ut.mean_i,
            "FUN_other": ut.get_mode,  # Use mode for other types, not mean
        }
    else:
        # Fallback defaults
        defaults = {
            "FUN_binary": ut.get_mode,
            "FUN_character": ut.get_mode,
            "FUN_factor": ut.get_mode,
            "FUN_logical": ut.get_mode,
            "FUN_numeric": ut.mean_na,
            "FUN_integer": ut.mean_i,
            "FUN_other": ut.get_mode,  # Use mode for other types, not mean
        }

    # Override all with FUN if provided
    if FUN is not None:
        for key in defaults:
            defaults[key] = FUN

    return defaults


def _compute_variable_type_mapping(variable_type: dict, columns: list) -> dict:
    """
    Pre-compute variable type mappings to avoid repetitive _check_variable_type calls.

    Parameters:
    -----------
    variable_type : dict
        Dictionary mapping column names to variable types
    columns : list
        List of column names to process

    Returns:
    --------
    dict
        Dictionary mapping column names to boolean flags for each type
    """
    type_mapping = {}

    for col in columns:
        if col in variable_type:
            type_mapping[col] = {
                "original_type": variable_type[col],
                "is_binary": _check_variable_type(variable_type, col, "binary"),
                "is_character": _check_variable_type(variable_type, col, "character"),
                "is_logical": _check_variable_type(variable_type, col, "logical"),
                "is_categorical": _check_variable_type(
                    variable_type, col, "categorical"
                ),
                "is_factor": _check_variable_type(variable_type, col, "factor"),
                "is_numeric": _check_variable_type(variable_type, col, "numeric"),
                "is_integer": _check_variable_type(variable_type, col, "integer"),
            }
        else:
            # Default mapping for unknown columns
            type_mapping[col] = {
                "original_type": "other",
                "is_binary": False,
                "is_character": False,
                "is_logical": False,
                "is_categorical": False,
                "is_factor": False,
                "is_numeric": False,
                "is_integer": False,
            }

    return type_mapping


def _datagridcf(model=None, newdata=None, by=None, **kwargs):
    if model is None and newdata is None:
        raise ValueError("One of model or newdata must be specified")

    model = sanitize_model(model)

    if newdata is None:
        newdata = model.get_modeldata()

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
        # Handle None specially to create null columns (similar to R's NA)
        if value is None:
            dfs.append(pl.DataFrame({key: [None]}))
        elif callable(value):
            dfs.append(pl.DataFrame({key: value(modeldata[key])}))
        else:
            dfs.append(pl.DataFrame({key: value}))

    # Perform cross join
    df_cross = reduce(lambda df1, df2: df1.join(df2, how="cross"), dfs)

    # Drop columns that exist in both newdata and df_cross to avoid duplicates
    columns_to_drop = [col for col in df_cross.columns if col in newdata.columns]
    if columns_to_drop:
        newdata = newdata.drop(columns_to_drop)

    result = newdata.join(df_cross, how="cross")

    result.datagrid_explicit = list(kwargs.keys())

    return result


datagrid.__doc__ = """
    # `datagrid()`

    Generate a data grid of user-specified values for use in the 'newdata' argument of the 'predictions()', 'comparisons()', and 'slopes()' functions.

    This is useful to define where in the predictor space we want to evaluate the quantities of interest. Ex: the predicted outcome or slope for a 37 year old college graduate.

    ## Parameters
    * model: (object, optional)
        Model object.
        * (one and only one of the `model` and `newdata` arguments can be used.)
    * newdata: (DataFrame, optional)
        Data frame used to define the predictor space.
        * (one and only one of the `model` and `newdata` arguments can be used.)
    * grid_type: (str, optional)
        Determines the functions to apply to each variable. The defaults can be overridden by defining individual variables explicitly in the `**kwargs`, or by supplying a function to one of the `FUN_*` arguments.
        * "mean_or_mode": Character, factor, logical, and binary variables are set to their modes. Numeric, integer, and other variables are set to their means.
        * "balanced": Each unique level of character, factor, logical, and binary variables are preserved. Numeric, integer, and other variables are set to their means. Warning: When there are many variables and many levels per variable, a balanced grid can be very large. In those cases, it is better to use `grid_type="mean_or_mode"` and to specify the unique levels of a subset of named variables explicitly.
        * "counterfactual": the entire dataset is duplicated for each combination of the variable values specified in `**kwargs`. Variables not explicitly supplied to `datagrid()` are set to their observed values in the original dataset.
    * FUN_numeric: (Callable, optional)
        The function to be applied to numeric variables.
    * FUN_other: (Callable, optional)
        The function to be applied to other variable types.
    * **kwargs
        * Named arguments where the name is the variable name and the value is a list of values to use in the grid. If a variable is not specified, it is set to its mean or mode depending on the `grid_type` argument.

    ## Returns
    (polars.DataFrame)
    * DataFrame where each row corresponds to one combination of the named predictors supplied by the user. Variables which are not explicitly defined are held at their mean or mode.

    ## Examples
    ```py
    import polars as pl
    import statsmodels.formula.api as smf
    from marginaleffects import *
    data = get_dataset("thornton")

    # The output only has 2 rows, and all the variables except `hp` are at their mean or mode.
    datagrid(newdata = data, village = [43, 11])

    # We get the same result by feeding a model instead of a DataFrame
    mod = smf.ols("outcome ~ incentive + distance", data).fit()
    datagrid(model = mod, village = [43, 11])

    # Use in `marginaleffects` to compute "Typical Marginal Effects". When used in `slopes()` or `predictions()` we do not need to specify the `model` or `newdata` arguments.
    nd = datagrid(mod, village = [43, 11])
    slopes(mod, newdata = nd)

    # The full dataset is duplicated with each observation given counterfactual values of 43 and 11 for the `village` variable. 
    # The original `thornton` includes 2884 rows, so the resulting dataset includes 5768 rows.
    dg = datagrid(newdata = data, village = [43, 11], grid_type = "counterfactual")
    dg.shape
    ```
    """
