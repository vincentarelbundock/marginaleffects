import polars as pl
import statsmodels.formula.api as smf
from pytest import approx
from tests.helpers import quine
from marginaleffects import *
from tests.utilities import sort_categories_pandas

dat = quine
mod = smf.negativebinomial(
    "Days ~ Sex/(Age + Eth*Lrn)", data=sort_categories_pandas(dat.to_pandas())
).fit()


def test_predictions_01():
    unknown = predictions(mod)
    known = pl.read_csv("tests/r/test_statsmodels_negativebinomial_predictions_01.csv")
    assert known["estimate"].to_numpy() == approx(
        unknown["estimate"].to_numpy(), rel=1e-3
    )


def test_comparisons_01():
    unknown = comparisons(mod, variables="Sex")
    known = pl.read_csv(
        "tests/r/test_statsmodels_negativebinomial_comparisons_01.csv"
    ).filter(pl.col("term") == "Sex")
    assert known["estimate"].to_numpy() == approx(
        unknown["estimate"].to_numpy(), rel=1e-2
    )
