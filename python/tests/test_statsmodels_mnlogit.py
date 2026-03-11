import numpy as np
import polars as pl
import statsmodels.formula.api as smf
from marginaleffects import *
from polars.testing import assert_series_equal

dat = (
    get_dataset("penguins", "palmerpenguins")
    .drop_nulls(["species", "island", "bill_length_mm", "flipper_length_mm"])
    .with_columns(
        pl.col("island").replace_strict({"Biscoe": 1, "Dream": 2, "Torgersen": 3}),
        pl.col("species").cast(pl.Categorical),
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
    known = known.sort(["term", "group"])
    unknown = unknown.sort(["term", "group"])
    # R uses nnet::multinom, Python uses statsmodels MNLogit.
    # Models differ slightly, so we use atol for small values near zero.
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        atol=1e-3,
        rtol=0.1,
    )
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        atol=1e-3,
        rtol=0.1,
    )


def test_predictions_01():
    known = pl.read_csv("tests/r/test_statsmodels_mnlogit_predictions_01.csv")
    unknown = pl.DataFrame(predictions(mod)).with_columns(
        pl.col("group").replace_strict(group_map),
        (pl.col("rowid") + 1).alias("rowid"),
    )
    compare = known.join(
        unknown.select("rowid", "group", pl.col("estimate").alias("estimate_py")),
        on=["rowid", "group"],
        how="inner",
    )
    assert_series_equal(
        compare["estimate"], compare["estimate_py"], rel_tol=1e-2, check_names=False
    )


def test_predictions_02():
    known = pl.read_csv("tests/r/test_statsmodels_mnlogit_predictions_02.csv")
    unknown = pl.DataFrame(predictions(mod, by="species")).with_columns(
        pl.col("group").replace_strict(group_map),
        pl.col("species").cast(pl.String),
    )
    known = known.sort(["group", "species"])
    unknown = unknown.sort(["group", "species"])
    assert_series_equal(
        known["estimate"], unknown["estimate"], rel_tol=1e-2, check_names=False
    )


def test_comparisons_01():
    known = pl.read_csv("tests/r/test_statsmodels_mnlogit_comparisons_01.csv")
    unknown = pl.DataFrame(comparisons(mod)).with_columns(
        pl.col("group").replace_strict(group_map),
        (pl.col("rowid") + 1).alias("rowid"),
    )
    known = known.sort(["rowid", "term", "group"])
    unknown = unknown.sort(["rowid", "term", "group"])
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        atol=1e-4,
    )


def test_comparisons_02():
    known = pl.read_csv("tests/r/test_statsmodels_mnlogit_comparisons_02.csv")
    unknown = pl.DataFrame(comparisons(mod, by=["group", "species"])).with_columns(
        pl.col("group").replace_strict(group_map),
        pl.col("species").cast(pl.String),
    )
    known = known.sort(["term", "group", "species"])
    unknown = unknown.sort(["term", "group", "species"])
    assert_series_equal(
        known["estimate"], unknown["estimate"], rel_tol=1e-2, check_names=False
    )


def test_comparisons_terms_differ():
    """Verify that comparisons for different terms produce different estimates."""
    cmp = comparisons(mod, vcov=False)
    bill = cmp.filter(pl.col("term") == "bill_length_mm")["estimate"]
    flip = cmp.filter(pl.col("term") == "flipper_length_mm")["estimate"]
    assert not np.allclose(bill.to_numpy(), flip.to_numpy())
