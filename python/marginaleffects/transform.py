import numpy as np
import polars as pl


_TRANSFORMS = {
    "exp": lambda col: np.exp(col),
}


def get_transform(x, transform=None):
    if transform is None:
        return x

    if isinstance(transform, str):
        if transform not in _TRANSFORMS:
            raise ValueError(
                f"Unknown transform '{transform}'. Available: {', '.join(_TRANSFORMS.keys())}."
            )
        fn = _TRANSFORMS[transform]
    elif callable(transform):
        fn = transform
    else:
        raise ValueError("`transform` must be a string or callable.")

    for col in ["estimate", "conf_low", "conf_high"]:
        if col in x.columns:
            x = x.with_columns(fn(pl.col(col)))

    drop_cols = [c for c in ["std_error", "statistic"] if c in x.columns]
    if drop_cols:
        x = x.drop(drop_cols)

    return x
