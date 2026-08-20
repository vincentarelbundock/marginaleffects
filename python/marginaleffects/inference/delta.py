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
    variances = np.sum((J @ V) * J, axis=1)
    # Tiny negative variances are floating-point noise from the quadratic
    # form; clamp them to zero instead of letting sqrt() return NaN. An exact
    # zero is a statement, not a failure: a constant estimand has variance
    # exactly zero, and its undefined test statistic surfaces downstream.
    # This mirrors std_error_from_jacobian() in the R package.
    with np.errstate(invalid="ignore"):
        scale = np.nanmax(np.abs(variances), initial=0.0)
    if not np.isfinite(scale):
        scale = 0.0
    tol = np.sqrt(np.finfo(float).eps) * max(1.0, scale)
    variances = np.where((variances < 0) & (variances > -tol), 0.0, variances)
    with np.errstate(invalid="ignore"):
        return np.sqrt(variances)


def add_standard_errors(out, func, model, V, eps_vcov):
    """Compute a Jacobian and append its delta-method standard errors."""
    if V is None:
        return out, None
    J = get_jacobian(func=func, coefs=model.get_coef(), eps_vcov=eps_vcov)
    se = get_se(J, V)
    return out.with_columns(pl.Series(se).alias("std_error")), J
