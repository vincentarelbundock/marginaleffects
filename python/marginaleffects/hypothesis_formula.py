# Backward-compatibility shim — all logic now lives in hypotheses/
from .hypotheses.formula import (  # noqa: F401
    eval_hypothesis_formula,
    parse_hypothesis_formula,
)
