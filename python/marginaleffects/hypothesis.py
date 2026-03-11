# Backward-compatibility shim — all logic now lives in hypotheses/
from .hypotheses.core import (  # noqa: F401
    eval_string_hypothesis,
    get_hypothesis,
    get_hypothesis_row_labels,
    lincom_multiply,
)
