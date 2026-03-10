import warnings
from typing import Any, Dict, Optional

import polars as pl

from .result import MarginaleffectsResult


# Backwards compatibility alias
class MarginaleffectsDataFrame(MarginaleffectsResult):
    def __init__(self, *args, **kwargs):
        warnings.warn(
            "MarginaleffectsDataFrame is deprecated; use MarginaleffectsResult instead.",
            DeprecationWarning,
            stacklevel=2,
        )
        super().__init__(*args, **kwargs)


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


def _check_variable_type(
    variable_type: Dict[str, str], variable_name: str, expected_type: str
) -> bool:
    if variable_name not in variable_type:
        return False

    actual_type = variable_type[variable_name]
    if expected_type == "factor" and actual_type == "categorical":
        return True
    if expected_type == "categorical" and actual_type == "factor":
        return True
    return actual_type == expected_type
