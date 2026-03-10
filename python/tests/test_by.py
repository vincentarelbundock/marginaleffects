import polars as pl
import statsmodels.formula.api as smf
from marginaleffects import *
from tests.helpers import guerry, guerry_mod  # noqa

from tests.utilities import *


def test_predictions_by_string(guerry_mod):
    cmp_py = predictions(guerry_mod, by="Region")
    cmp_r = pl.read_csv("tests/r/test_by_01.csv").sort("Region")
    compare_r_to_py(cmp_r, cmp_py)


def test_comparisons_by_true(guerry_mod):
    cmp_py = comparisons(guerry_mod, by=True)
    cmp_r = pl.read_csv("tests/r/test_by_02.csv")
    compare_r_to_py(cmp_r, cmp_py)


def test_comparisons_by_false(guerry_mod):
    cmp_py = comparisons(guerry_mod, by=False)
    cmp_r = pl.read_csv("tests/r/test_by_03.csv")
    compare_r_to_py(cmp_r, cmp_py)


def test_predictions_by_wts(guerry_mod):
    pre_py = predictions(guerry_mod, by="Region", wts="Donations")
    pre_r = pl.read_csv("tests/r/test_by_04.csv").sort("Region")
    compare_r_to_py(pre_r, pre_py)


def test_median_by(guerry_mod):
    mod = smf.ols(
        formula="Lottery ~ Literacy + Wealth + C(Region)", data=guerry.to_pandas()
    ).fit()
    s = avg_slopes(mod, variables="Literacy", by="Region", newdata="median")
    assert s.shape[0] == 5, "by variable not treated as unique in datagrid"


########### snapshot tests don't work

# @pytest.fixture
# def predictions_fixture():
#     df = pl.read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv") \
#         .with_columns(pl.col("cyl").cast(pl.Utf8))
#     mod = smf.ols("mpg ~ hp * qsec + cyl", df).fit()
#     p = predictions(mod, by = "cyl")
#     return p

# def test_predictions_snapshot_order(predictions_fixture, snapshot):
#     snapshot.assert_match(predictions_fixture.to_csv(index=False))
