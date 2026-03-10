import polars as pl
import statsmodels.formula.api as smf

from marginaleffects import *
from tests.helpers import mtcars
from tests.utilities import *


mod_py = smf.ols("mpg ~ wt * hp", mtcars).fit()


def test_comparison_derivatives():
    est = ["dydx", "dydxavg", "dyex", "dyexavg", "eydx", "eydxavg", "eyex", "eyexavg"]
    for e in est:
        cmp_py = comparisons(mod_py, comparison=e)
        cmp_r = pl.read_csv(f"tests/r/test_slopes_01_{e}.csv")
        compare_r_to_py(cmp_r, cmp_py, msg=e, tolr=1e-4, tola=1e-4)


def test_slopes():
    est = ["dydx", "dydx", "dyex", "eydx", "eyex"]
    for e in est:
        cmp_py = slopes(mod_py, slope=e)
        cmp_r = pl.read_csv(f"tests/r/test_slopes_01_{e}.csv")
        compare_r_to_py(cmp_r, cmp_py, msg=e, tolr=1e-5, tola=1e-5)


def test_slopes_padding():
    dat = mtcars.with_columns(pl.col("cyl").cast(pl.String).cast(pl.Categorical))
    mod = smf.ols("mpg ~ cyl + hp", dat.to_pandas()).fit()
    s = slopes(mod, newdata="mean")
    assert s.shape[0] == 3


def test_bug_newdata_variables():
    dat = mtcars.with_columns(pl.col("cyl").cast(pl.String).cast(pl.Categorical))
    mod = smf.ols("mpg ~ cyl + hp", dat.to_pandas()).fit()
    s = slopes(mod, newdata="mean", variables="hp")
    assert s.shape[0] == 1
