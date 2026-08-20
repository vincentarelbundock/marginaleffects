"""Test statistics, confidence intervals, and p-values."""

import warnings

import numpy as np
import polars as pl
from scipy import stats


def get_z_p_ci(df, model, conf_level, hypothesis_null=0):
    if "std_error" not in df.columns:
        return df
    df = df.with_columns(
        ((pl.col("estimate") - float(hypothesis_null)) / pl.col("std_error")).alias(
            "statistic"
        )
    )
    dof = model.get_df()
    critical_value = stats.t.ppf((1 + conf_level) / 2, dof)
    df = df.with_columns(
        (pl.col("estimate") - critical_value * pl.col("std_error")).alias("conf_low"),
        (pl.col("estimate") + critical_value * pl.col("std_error")).alias("conf_high"),
        pl.col("statistic")
        .map_batches(
            lambda x: 2 * (1 - stats.t.cdf(np.abs(x), dof)),
            return_dtype=pl.Float64,
        )
        .alias("p_value"),
    )
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        df = df.with_columns(
            pl.col("p_value")
            .map_batches(lambda x: -np.log2(x), return_dtype=pl.Float64)
            .alias("s_value")
        )
    return df
