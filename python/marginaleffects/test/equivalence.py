from typing import Union

import numpy as np
import polars as pl


def get_equivalence(
    x: pl.DataFrame, equivalence: Union[list, None], df: float = np.inf
) -> pl.DataFrame:
    if equivalence is None:
        return x

    assert (
        len(equivalence) == 2
        and isinstance(equivalence[0], float)
        and isinstance(equivalence[1], float)
    ), "The `equivalence` argument must be None or a list of two 'float' values."

    if not all(col in x for col in ["estimate", "std_error"]):
        msg = "The `equivalence` argument is not supported for `marginaleffects` commands which do not produce standard errors."
        raise ValueError(msg)

    x = x.with_columns(
        ((x["estimate"] - equivalence[0]) / x["std_error"]).alias("statistic_noninf"),
        ((x["estimate"] - equivalence[1]) / x["std_error"]).alias("statistic_nonsup"),
    )

    if np.isinf(df):
        from scipy.stats import norm

        x = x.with_columns(
            pl.col("statistic_noninf")
            .map_batches(lambda x: 1 - norm.cdf(x), return_dtype=pl.Float64)
            .alias("p_value_noninf"),
            pl.col("statistic_nonsup")
            .map_batches(lambda x: norm.cdf(x), return_dtype=pl.Float64)
            .alias("p_value_nonsup"),
        )
    else:
        from scipy.stats import t

        x = x.with_columns(
            pl.col("statistic_noninf")
            .map_batches(lambda x: 1 - t.cdf(x), return_dtype=pl.Float64)
            .alias("p_value_noninf"),
            pl.col("statistic_nonsup")
            .map_batches(lambda x: t.cdf(x), return_dtype=pl.Float64)
            .alias("p_value_nonsup"),
        )

    x = x.with_columns(
        pl.max_horizontal("p_value_nonsup", "p_value_noninf").alias("p_value_equiv")
    )

    return x
