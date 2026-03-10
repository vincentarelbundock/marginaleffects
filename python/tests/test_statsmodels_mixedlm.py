import polars as pl
import statsmodels.formula.api as smf
from polars.testing import assert_series_equal
from marginaleffects import *
from tests.utilities import sort_categories_pandas

dat = (
    get_dataset("dietox", "geepack")
    .select("Weight", "Time", "Litter", "Pig", "Cu")
    .drop_nulls()
    .with_columns(
        pl.col("Cu").cast(pl.String).cast(pl.Categorical)
        # Litter stays as Int32 (numeric) to match R's numeric treatment
    )
)
dat_pd = sort_categories_pandas(dat.to_pandas())
mod = smf.mixedlm(
    formula="Weight ~ Time * Litter", data=dat_pd, groups=dat["Pig"]
).fit()


def test_predictions_01():
    unknown = predictions(mod)
    known = pl.read_csv("tests/r/test_statsmodels_mixedlm_predictions_01.csv")
    assert_series_equal(known["estimate"], unknown["estimate"], check_names=False)
    assert_series_equal(known["std.error"], unknown["std_error"], check_names=False)


def test_predictions_02():
    unknown = predictions(mod, by="Cu")
    known = pl.read_csv("tests/r/test_statsmodels_mixedlm_predictions_02.csv")
    assert_series_equal(known["estimate"], unknown["estimate"], check_names=False)
    assert_series_equal(known["std.error"], unknown["std_error"], check_names=False)


def test_comparisons_01():
    unknown = comparisons(mod).sort(["term", "contrast", "rowid"])
    known = pl.read_csv(
        "tests/r/test_statsmodels_mixedlm_comparisons_01.csv", ignore_errors=True
    ).sort(["term", "contrast", "rowid"])
    assert_series_equal(
        known["estimate"], unknown["estimate"], rel_tol=1e-4, check_names=False
    )
    assert_series_equal(
        known["std.error"], unknown["std_error"], check_names=False, rel_tol=1e-3
    )


def test_comparisons_02():
    unknown = comparisons(mod, by="Cu").sort(["term", "Cu"])
    known = pl.read_csv("tests/r/test_statsmodels_mixedlm_comparisons_02.csv").sort(
        ["term", "Cu"]
    )
    assert_series_equal(known["estimate"], unknown["estimate"], check_names=False)
    assert_series_equal(
        known["std.error"], unknown["std_error"], check_names=False, rel_tol=1e-3
    )
