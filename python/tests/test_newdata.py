import polars as pl
import statsmodels.formula.api as smf
from tests.helpers import mtcars
from marginaleffects import *

mod = smf.probit("am ~ hp + wt", data=mtcars).fit()


def test_mean_median():
    a = slopes(mod, newdata="mean")
    b = slopes(mod, newdata="median")
    assert a.shape[0] == 2
    assert b.shape[0] == 2
    assert all(b["estimate"].to_numpy() != a["estimate"].to_numpy())


def test_predictions_mean():
    p = predictions(mod, newdata="median")
    assert p.shape[0] == 1


def test_predictions_padding():
    dat = pl.read_csv("tests/data/impartiality.csv").with_columns(
        pl.col("impartial").cast(pl.Int32),
        pl.col("democracy").cast(pl.Categorical),
        pl.col("continent").cast(pl.Categorical),
    )
    m = smf.logit(
        "impartial ~ equal * democracy + continent", data=dat.to_pandas()
    ).fit()
    p = predictions(m, newdata=dat.head())
    assert p.shape[0] == 5
    p = predictions(m, newdata="mean")
    assert p.shape[0] == 1


def test_issue202_upcast_bug():
    dat = get_dataset("interaction_02")
    mod = smf.logit("Y ~ X * M", data=dat.to_pandas()).fit()
    cmp = comparisons(
        mod, variables="X", newdata=datagrid(M=[dat["M"].min(), dat["M"].max()])
    )
    assert cmp.height == 2
