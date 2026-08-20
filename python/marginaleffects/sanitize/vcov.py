import numpy as np


def sanitize_vcov(vcov, model):
    from ..inference.unconditional import as_unconditional, is_unconditional

    if is_unconditional(vcov):
        # The request is resolved once the estimand plan exists, so it travels
        # through the pipeline intact instead of collapsing to a matrix here.
        return as_unconditional(vcov)

    if isinstance(vcov, np.ndarray):
        V = vcov
        n = len(model.get_coef().ravel())
        if V.shape != (n, n):
            raise ValueError(
                f"`vcov` must be a square numpy array with {n} rows and columns "
                f"(matching the number of model coefficients). Got shape {V.shape}."
            )
        return V

    V = model.get_vcov(vcov)
    if V is not None and not isinstance(V, np.ndarray):
        raise TypeError("vcov must be True or a square NumPy array")
    return V
