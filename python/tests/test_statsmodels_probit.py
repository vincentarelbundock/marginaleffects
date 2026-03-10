import polars as pl
import statsmodels.formula.api as smf
from polars.testing import assert_series_equal
from tests.helpers import iris
from marginaleffects import *

dat = iris
dat = dat.rename(
    {
        "Sepal.Length": "Sepal_Length",
        "Sepal.Width": "Sepal_Width",
        "Petal.Length": "Petal_Length",
        "Petal.Width": "Petal_Width",
    }
)
dat = dat.with_columns(
    (pl.col("Sepal_Width") < pl.col("Sepal_Width").median()).cast(pl.Int16).alias("bin")
)
mod = smf.probit("bin ~ Petal_Length * Petal_Width", data=dat.to_pandas()).fit()


def test_predictions_01():
    unknown = predictions(mod)
    known = pl.read_csv("tests/r/test_statsmodels_probit_predictions_01.csv")
    assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-4)


def test_predictions_02():
    unknown = predictions(mod, by="Species")
    known = pl.read_csv("tests/r/test_statsmodels_probit_predictions_02.csv")
    assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-4)


def test_comparisons_01():
    unknown = comparisons(mod).sort(["term"])
    known = pl.read_csv("tests/r/test_statsmodels_probit_comparisons_01.csv").sort(
        ["term"]
    )
    assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-3)


def test_comparisons_02():
    unknown = comparisons(mod, by="Species").sort(["term", "Species"])
    known = pl.read_csv("tests/r/test_statsmodels_probit_comparisons_02.csv").sort(
        ["term", "Species"]
    )
    assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-4)
