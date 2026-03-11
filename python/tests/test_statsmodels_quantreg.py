# R's quantreg::rq uses the Barrodale-Roberts simplex algorithm (method="br") by
# default, while Python's statsmodels.QuantReg uses iteratively reweighted least
# squares (IRLS). The quantile regression objective is piecewise linear, so the
# solution is not unique — there is typically a flat region (polytope) of equally
# optimal solutions. The simplex algorithm lands on a vertex of this polytope
# (producing exact rational coefficients), while IRLS converges to an interior
# point. All R methods (br, fn, fnb, pfn) agree with each other, but differ from
# the Python IRLS solution by 1-8% on coefficients:
#
#   Parameter                  R (br)     Python IRLS    Rel diff
#   Intercept                  1.0557     1.1369          7.7%
#   Sepal.Width                0.7629     0.7435          2.5%
#   Petal.Length               1.2214     1.2065          1.2%
#   Species[versicolor]       -1.2344    -1.2159          1.5%
#   Species[virginica]        -1.8157    -1.7848          1.7%
#   Sepal.Width:Petal.Length  -0.1214    -0.1196          1.5%
#
# These coefficient differences propagate into comparisons, so we use abs_tol=0.035
# (rather than the usual rel_tol=1e-2) when comparing against R reference data.

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


def test_comparisons_01():
    unknown = (
        comparisons(mod)
        .data.rename(
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
            pl.col("term").str.replace("_", "."),
            pl.col("rowid").cast(pl.Int64) + 1,
            pl.col("Species").cast(pl.String),
        )
    )
    known = pl.read_csv("tests/r/test_statsmodels_quantreg_comparisons_01.csv").select(
        ["rowid", "term", "contrast", "Species", "estimate", "std.error", "predicted"]
    )
    tmp = known.join(unknown, on=["rowid", "term", "contrast", "Species"], how="left")
    assert_series_equal(
        tmp["estimate"], tmp["estimate_unknown"], abs_tol=0.035, check_names=False
    )


def test_comparisons_02():
    unknown = (
        comparisons(mod, by="Species")
        .data.sort(["term", "Species"])
        .with_columns(
            pl.col("term").str.replace("_", "."),
            pl.col("Species").cast(pl.String),
        )
    )
    known = pl.read_csv("tests/r/test_statsmodels_quantreg_comparisons_02.csv").sort(
        ["term", "Species"]
    )
    assert_series_equal(
        known["estimate"], unknown["estimate"], abs_tol=0.035, check_names=False
    )
