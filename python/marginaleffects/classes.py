import warnings

from .result import MarginaleffectsResult

# Re-export from datagrid for backward compatibility
from .datagrid import _detect_variable_type, _check_variable_type  # noqa: F401


# Backwards compatibility alias
class MarginaleffectsDataFrame(MarginaleffectsResult):
    def __init__(self, *args, **kwargs):
        warnings.warn(
            "MarginaleffectsDataFrame is deprecated; use MarginaleffectsResult instead.",
            DeprecationWarning,
            stacklevel=2,
        )
        super().__init__(*args, **kwargs)
