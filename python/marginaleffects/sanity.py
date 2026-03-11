# Backward-compatibility shim — all logic now lives in sanitize/
from .sanitize import (  # noqa: F401
    handle_deprecated_hypotheses_argument,
    handle_pyfixest_vcov_limitation,
    HiLo,
    sanitize_by,
    sanitize_comparison,
    sanitize_hypothesis_null,
    sanitize_newdata,
    sanitize_variables,
    sanitize_vcov,
)
