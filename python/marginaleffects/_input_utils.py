from warnings import warn
from typing import Tuple

from .sanitize_model import sanitize_model
from .sanity import (
    sanitize_by,
    sanitize_hypothesis_null,
    sanitize_newdata,
    sanitize_vcov,
)
from .utils import validate_string_columns
from .pyfixest import ModelPyfixest


def prepare_base_inputs(
    model,
    vcov,
    by,
    newdata,
    wts,
    hypothesis,
    *,
    enforce_pyfixest_warning: bool = True,
) -> Tuple:
    """
    Shared helper to sanitize model, newdata, by, and hypothesis inputs.
    """
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
