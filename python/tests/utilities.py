import os
import re
from matplotlib.testing.compare import compare_images
from plotnine import ggsave
import warnings
import polars as pl
import pandas as pd


def sort_categories_pandas(df_pd):
    """Sort pandas categorical categories alphabetically for consistent model fitting."""
    for col in df_pd.columns:
        if df_pd[col].dtype.name == "category":
            df_pd[col] = df_pd[col].cat.reorder_categories(
                sorted(df_pd[col].cat.categories)
            )
    return df_pd


def convert_to_categorical_pandas(df_pd, columns, numeric_order=True):
    """Convert columns to categorical in pandas with proper ordering.

    Parameters
    ----------
    df_pd : pandas.DataFrame
        Input dataframe
    columns : list
        Column names to convert to categorical
    numeric_order : bool
        If True, use numeric ordering for the categories.
        If False, use lexical (alphabetical) ordering.

    Returns
    -------
    pandas.DataFrame
        DataFrame with columns converted to categorical

    Notes
    -----
    Converts values to strings before creating categorical to ensure they
    remain categorical after pandas→Polars conversion via Arrow.
    """
    for col in columns:
        if col in df_pd.columns:
            # Convert to string first to preserve categorical type through Arrow
            df_pd[col] = df_pd[col].astype(str)

            # Get unique string values
            unique_vals = df_pd[col].dropna().unique()

            # Sort based on ordering preference
            if numeric_order:
                # Numeric ordering: sort as numbers then convert to strings
                categories = sorted(
                    unique_vals,
                    key=lambda x: float(x)
                    if x.replace(".", "").replace("-", "").isdigit()
                    else x,
                )
            else:
                # Lexical ordering: sort as strings
                categories = sorted(unique_vals)

            # Convert to categorical with specified order
            df_pd[col] = pd.Categorical(
                df_pd[col], categories=categories, ordered=False
            )

    return df_pd


def compare_r_to_py(r_obj, py_obj, tolr=1e-3, tola=1e-3, msg=""):
    import polars as pl

    # Cast Enum columns to String for consistent sorting with R
    for col in py_obj.columns:
        if isinstance(py_obj[col].dtype, pl.Enum):
            py_obj = py_obj.with_columns(pl.col(col).cast(pl.String))

    cols = ["term", "contrast", "rowid"]
    cols = [x for x in cols if x in r_obj.columns and x in py_obj.columns]

    # If no standard sorting columns exist, sort by all common columns except numeric ones
    if not cols:
        cols = [
            c
            for c in r_obj.columns
            if c in py_obj.columns
            and r_obj[c].dtype not in [pl.Float64, pl.Float32, pl.Int64, pl.Int32]
        ]

    r_obj = r_obj.sort(cols)
    py_obj = py_obj.sort(cols)
    # dont' compare other statistics because degrees of freedom don't match
    # for col_py in ["estimate", "std_error"]:
    for col_py in ["estimate"]:
        col_r = re.sub("_", ".", col_py)
        if col_py in py_obj.columns and col_r in r_obj.columns:
            a = r_obj[col_r]
            b = py_obj[col_py]
            gap_rel = ((a - b) / a).abs().max()
            gap_abs = (a - b).abs().max()
            flag = gap_rel <= tolr or gap_abs <= tola
            assert flag, f"{msg} trel: {gap_rel}. tabs: {gap_abs}"


def assert_image(fig, label, folder, tolerance=5):
    known_path = f"./tests/images/{folder}/"
    unknown_path = f"./tests/images/.tmp_{folder}/"
    if os.path.isdir(unknown_path):
        for root, dirs, files in os.walk(unknown_path):
            for fname in files:
                os.remove(os.path.join(root, fname))
        os.rmdir(unknown_path)
    os.mkdir(unknown_path)
    unknown = f"{unknown_path}{label}.png"
    known = f"{known_path}{label}.png"
    if not os.path.exists(known):
        ggsave(fig, filename=known, verbose=False, height=5, width=10, dpi=100)
        warnings.warn(f"File {known} does not exist. Creating it now.")
        return None
    ggsave(fig, filename=unknown, verbose=False, height=5, width=10, dpi=100)
    out = compare_images(known, unknown, tol=tolerance)
    # os.remove(unknown)
    return out


# for polars
def cast_to_categorical(df, col):
    df = df.with_columns(pl.col(col).cast(pl.Categorical))
    return df
