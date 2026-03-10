import numpy as np
import polars as pl


def get_transform(x, transform=None):
    if transform is not None:

        def transform(x):
            return np.exp(x)

        for col in ["estimate", "conf_low", "conf_high"]:
            if col in x.columns:
                x = x.with_columns(transform(pl.col(col)))
        return x.drop(["std_error", "statistic"])
    else:
        return x
