import warnings

import numpy as np
import polars as pl
import scipy.stats as stats


def get_jacobian(func, coefs, eps_vcov=None):
    # forward finite difference (faster)
    if coefs.ndim == 2:
        if isinstance(coefs, np.ndarray):
            coefs_flat = coefs.flatten(order="F")
        else:
            coefs_flat = coefs.to_numpy().flatten(order="F")
        baseline = func(coefs)["estimate"].to_numpy()
        jac = np.empty((baseline.shape[0], len(coefs_flat)), dtype=np.float64)
        for i, xi in enumerate(coefs_flat):
            if eps_vcov is not None:
                h = eps_vcov
            else:
                h = max(abs(xi) * np.sqrt(np.finfo(float).eps), 1e-10)
            dx = np.copy(coefs_flat)
            dx[i] = dx[i] + h
            tmp = dx.reshape(coefs.shape, order="F")
            jac[:, i] = (func(tmp)["estimate"].to_numpy() - baseline) / h
        return jac
    else:
        baseline = func(coefs)["estimate"].to_numpy()
        jac = np.empty((baseline.shape[0], len(coefs)), dtype=np.float64)
        for i, xi in enumerate(coefs):
            if eps_vcov is not None:
                h = eps_vcov
            else:
                h = max(abs(xi) * np.sqrt(np.finfo(float).eps), 1e-10)
            dx = np.copy(coefs)
            dx[i] = dx[i] + h
            jac[:, i] = (func(dx)["estimate"].to_numpy() - baseline) / h
        return jac


def get_se(J, V):
    se = np.sqrt(np.sum((J @ V) * J, axis=1))
    return se


def get_z_p_ci(df, model, conf_level, hypothesis_null=0):
    if "std_error" not in df.columns:
        return df
    df = df.with_columns(
        ((pl.col("estimate") - float(hypothesis_null)) / pl.col("std_error")).alias(
            "statistic"
        )
    )
    if hasattr(model, "df_resid") and isinstance(model.df_resid, float):
        dof = model.df_resid
    else:
        dof = np.inf
    critical_value = stats.t.ppf((1 + conf_level) / 2, dof)

    df = df.with_columns(
        (pl.col("estimate") - critical_value * pl.col("std_error")).alias("conf_low")
    )
    df = df.with_columns(
        (pl.col("estimate") + critical_value * pl.col("std_error")).alias("conf_high")
    )

    df = df.with_columns(
        pl.col("statistic")
        .map_batches(
            lambda x: (2 * (1 - stats.t.cdf(np.abs(x), dof))), return_dtype=pl.Float64
        )
        .alias("p_value")
    )
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            df = df.with_columns(
                pl.col("p_value")
                .map_batches(lambda x: -np.log2(x), return_dtype=pl.Float64)
                .alias("s_value")
            )
        except Exception as e:
            print(f"An exception occurred: {e}")
    return df
