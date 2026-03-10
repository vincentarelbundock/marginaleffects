import polars as pl
import statsmodels.formula.api as smf
from polars.testing import assert_series_equal

from marginaleffects import *
from tests.helpers import mtcars
from tests.utilities import sort_categories_pandas


dat = mtcars.with_columns(pl.col("cyl").cast(pl.String).cast(pl.Categorical))
mod = smf.ols(
    "mpg ~ qsec * wt + cyl", data=sort_categories_pandas(dat.to_pandas())
).fit()


def test_predictions_01():
    unknown = predictions(mod)
    known = pl.read_csv("tests/r/test_statsmodels_ols_predictions_01.csv")
    assert_series_equal(unknown["estimate"], known["estimate"])
    assert_series_equal(unknown["std_error"], known["std.error"], check_names=False)


def test_predictions_02():
    unknown = predictions(mod, by="carb")
    known = pl.read_csv("tests/r/test_statsmodels_ols_predictions_02.csv")
    assert_series_equal(unknown["estimate"], known["estimate"])
    assert_series_equal(unknown["std_error"], known["std.error"], check_names=False)


def test_comparisons_01():
    unknown = comparisons(mod).sort(["term", "contrast", "rowid"])
    known = pl.read_csv("tests/r/test_statsmodels_ols_comparisons_01.csv").sort(
        ["term", "contrast", "rowid"]
    )
    assert_series_equal(unknown["estimate"], known["estimate"])
    assert_series_equal(unknown["std_error"], known["std.error"], check_names=False)


def test_comparisons_02():
    unknown = comparisons(mod, by="carb").sort(["term", "carb"])
    known = pl.read_csv("tests/r/test_statsmodels_ols_comparisons_02.csv").sort(
        ["term", "carb"]
    )
    assert_series_equal(unknown["estimate"], known["estimate"])
    assert_series_equal(unknown["std_error"], known["std.error"], check_names=False)


def test_slopes_01():
    unknown = slopes(mod)
    known = pl.read_csv("tests/r/test_statsmodels_ols_slopes_01.csv")
    unknown = unknown.sort(["term", "contrast", "rowid"])
    known = known.sort(["term", "contrast", "rowid"])
    assert_series_equal(unknown["estimate"], known["estimate"], rel_tol=1e-4)
    # TODO: bad tolerance
    assert_series_equal(
        unknown["std_error"], known["std.error"], check_names=False, rel_tol=1e-1
    )


def test_slopes_02():
    unknown = slopes(mod, by="carb").sort(["term", "carb"])
    known = pl.read_csv("tests/r/test_statsmodels_ols_slopes_02.csv").sort(
        ["term", "carb"]
    )
    assert_series_equal(unknown["estimate"], known["estimate"])
    assert_series_equal(
        unknown["std_error"], known["std.error"], check_names=False, rel_tol=1e-2
    )
