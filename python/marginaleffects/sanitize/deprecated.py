from warnings import warn


def handle_deprecated_hypotheses_argument(hypothesis, kwargs, stacklevel=2):
    """Handle migration from 'hypotheses' to 'hypothesis' parameter."""
    if "hypotheses" in kwargs:
        if hypothesis is not None:
            raise ValueError("Specify at most one of `hypothesis` or `hypotheses`.")
        hypotheses = kwargs.pop("hypotheses")
        warn(
            "`hypotheses` is deprecated; use `hypothesis` instead.",
            DeprecationWarning,
            stacklevel=stacklevel,
        )
        return hypotheses
    return hypothesis


def handle_pyfixest_vcov_limitation(model, vcov, stacklevel=2):
    """Handle pyfixest vcov constraints with appropriate warnings."""
    # Import here to avoid circular dependency
    from ..pyfixest import ModelPyfixest

    if isinstance(model, ModelPyfixest) and vcov is not False:
        has_fixef = getattr(model.model, "_has_fixef", False)
        if has_fixef and not model.is_linear_model():
            warn(
                "For this pyfixest model, marginaleffects cannot take into account the "
                "uncertainty in fixed-effects parameters. Standard errors are disabled "
                "and vcov=False is enforced.",
                UserWarning,
                stacklevel=stacklevel,
            )
            return False

    return vcov
