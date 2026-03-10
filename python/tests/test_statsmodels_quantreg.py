import polars as pl
import pytest
import statsmodels.formula.api as smf
from polars.testing import assert_series_equal
from marginaleffects import comparisons, predictions
from tests.helpers import iris

dat = iris
dat = dat.rename(
    {
        "Sepal.Length": "Sepal_Length",
        "Sepal.Width": "Sepal_Width",
        "Petal.Length": "Petal_Length",
        "Petal.Width": "Petal_Width",
    }
)
dat = dat.drop_nulls("Species")
mod = smf.quantreg(
    "Sepal_Length ~ Sepal_Width * Petal_Length + Species", data=dat.to_pandas()
).fit(0.25, max_iter=1000)


def test_predictions_01():
    unknown = predictions(mod)
    known = pl.read_csv("tests/r/test_statsmodels_quantreg_predictions_01.csv")
    assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)


def test_predictions_02():
    unknown = predictions(mod, by="Species")
    known = pl.read_csv("tests/r/test_statsmodels_quantreg_predictions_02.csv")
    assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)


@pytest.mark.skip(reason="TODO: investigate")
def test_comparisons_01():
    unknown = (
        comparisons(mod)
        .rename(
            {
                "estimate": "estimate_unknown",
                "std_error": "std_error_unknown",
                "predicted": "predicted_unknown",
            }
        )
        .select(
            [
                "rowid",
                "term",
                "contrast",
                "Species",
                "estimate_unknown",
                "std_error_unknown",
                "predicted_unknown",
            ]
        )
        .with_columns(
            pl.col("term").str.replace("_", "."), pl.col("rowid").cast(pl.Int64) + 1
        )
    )
    known = pl.read_csv("tests/r/test_statsmodels_quantreg_comparisons_01.csv").select(
        ["rowid", "term", "contrast", "Species", "estimate", "std.error", "predicted"]
    )
    tmp = known.join(unknown, on=["rowid", "term", "contrast", "Species"], how="left")
    assert_series_equal(
        tmp["estimate"], tmp["estimate_unknown"], rel_tol=1e-2, check_names=False
    )


@pytest.mark.skip(reason="TODO: investigate")
def test_comparisons_02():
    unknown = comparisons(mod, by="Species").sort(["term", "Species"])
    known = pl.read_csv("tests/r/test_statsmodels_quantreg_comparisons_02.csv").sort(
        ["term", "Species"]
    )
    assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)
