"""Delta-method covariance propagation and numerical differentiation."""

import numpy as np
import polars as pl


def _estimate_vector(x):
    if isinstance(x, pl.DataFrame):
        return x["estimate"].to_numpy()
    return np.asarray(x, dtype=np.float64).reshape(-1)


def get_jacobian(func, coefs, eps_vcov=None):
    """Compute a forward finite-difference Jacobian."""
    original_shape = None
    if coefs.ndim == 2:
        original_shape = coefs.shape
        if isinstance(coefs, np.ndarray):
            coefs_flat = coefs.flatten(order="F")
        else:
            coefs_flat = coefs.to_numpy().flatten(order="F")
    else:
        coefs_flat = np.asarray(coefs)

    baseline = _estimate_vector(func(coefs))
    jac = np.empty((baseline.shape[0], len(coefs_flat)), dtype=np.float64)
    for i, xi in enumerate(coefs_flat):
        h = eps_vcov
        if h is None:
            h = max(abs(xi) * np.sqrt(np.finfo(float).eps), 1e-10)
        dx = np.copy(coefs_flat)
        dx[i] += h
        if original_shape is not None:
            dx = dx.reshape(original_shape, order="F")
        jac[:, i] = (_estimate_vector(func(dx)) - baseline) / h
    return jac


def get_se(J, V):
    return np.sqrt(np.sum((J @ V) * J, axis=1))


def add_standard_errors(out, func, model, V, eps_vcov):
    """Compute a Jacobian and append its delta-method standard errors."""
    if V is None:
        return out, None
    J = get_jacobian(func=func, coefs=model.get_coef(), eps_vcov=eps_vcov)
    se = get_se(J, V)
    return out.with_columns(pl.Series(se).alias("std_error")), J
