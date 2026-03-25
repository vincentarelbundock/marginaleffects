import numpy as np


def sanitize_vcov(vcov, model):
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
    if V is not None:
        if not isinstance(V, np.ndarray):
            raise TypeError("vcov must be True or a square NumPy array")
    return V
