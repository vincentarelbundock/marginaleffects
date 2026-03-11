import numpy as np
import polars as pl
import statsmodels.formula.api as smf
from marginaleffects import *
from polars.testing import assert_series_equal

dat = (
    get_dataset("penguins", "palmerpenguins")
    .drop_nulls(["species", "island", "bill_length_mm", "flipper_length_mm"])
    .with_columns(
        pl.col("island").replace_strict({"Biscoe": 1, "Dream": 2, "Torgersen": 3})
    )
)
mod = smf.mnlogit("island ~ bill_length_mm + flipper_length_mm", dat.to_pandas()).fit()

group_map = {"0": "Biscoe", "1": "Dream", "2": "Torgersen"}


def test_avg_predictions_01():
    known = pl.read_csv("tests/r/test_statsmodels_mnlogit_avg_predictions_01.csv")
    unknown = avg_predictions(mod).with_columns(
        pl.col("group").replace_strict(group_map)
    )
    assert_series_equal(
        known["estimate"], unknown["estimate"], rel_tol=1e-2, check_names=False
    )
    assert_series_equal(
        known["std.error"], unknown["std_error"], rel_tol=1e-2, check_names=False
    )


def test_avg_comparisons_01():
    known = pl.read_csv("tests/r/test_statsmodels_mnlogit_avg_comparisons_01.csv")
    unknown = avg_comparisons(mod).with_columns(
        pl.col("group").replace_strict(group_map)
    )
    # Sort both by term + group for consistent comparison
    known = known.sort(["term", "group"])
    unknown = unknown.sort(["term", "group"])
    # Point estimates: R uses nnet::multinom, Python uses statsmodels MNLogit.
    # Models differ slightly, so we use atol for small values near zero.
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        atol=1e-3,
        rtol=0.1,
    )
    # Standard errors
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        atol=1e-3,
        rtol=0.1,
    )


def test_comparisons_terms_differ():
    """Verify that comparisons for different terms produce different estimates."""
    cmp = comparisons(mod, vcov=False)
    bill = cmp.filter(pl.col("term") == "bill_length_mm")["estimate"]
    flip = cmp.filter(pl.col("term") == "flipper_length_mm")["estimate"]
    assert not np.allclose(bill.to_numpy(), flip.to_numpy())


# def test_predictions_01():
#     unknown = (
#         predictions(mod)
#         .with_columns(
#             pl.col("group").replace_strict(r), (pl.col("rowid") + 1).alias("rowid")
#         )
#         .rename({"estimate": "estimate_py"})
#     )

#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_predictions_01.csv")
#     compare = known.join(unknown, on=["rowid", "group"], how="inner")
#     compare.select("std.error", "std_error").head()
#     compare.select("estimate", "estimate_py").tail()
#     assert_series_equal(
#         compare["estimate"], compare["estimate_py"], rel_tol=1e-2, check_names=False
#     )
#     assert_series_equal(
#         compare["std.error"], compare["std_error"], rel_tol=1e-1, check_names=False
#     )


# def test_predictions_02():
#     unknown = predictions(mod, by="species")
#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_predictions_02.csv")
#     assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)


# def test_comparisons_01():
#     unknown = (
#         comparisons(mod)
#         .with_columns(pl.col("group").replace_strict(r))
#         .sort(["term", "group"])
#     )
#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_comparisons_01.csv").sort(
#         ["term", "group"]
#     )
#     assert_series_equal(known["estimate"].head(), unknown["estimate"].head(), rel_tol=1e-1)

#     unknown = comparisons(mod)
#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_comparisons_01.csv")
#     assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)


# def test_comparisons_02():
#     unknown = (
#         comparisons(mod, by=["group", "species"])
#         .with_columns(pl.col("group").replace_strict(r))
#         .sort(["term", "group", "species"])
#     )
#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_comparisons_02.csv").sort(
#         ["term", "group", "species"]
#     )
#     assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)
