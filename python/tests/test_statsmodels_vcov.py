import numpy as np
import polars as pl
import statsmodels.formula.api as smf
from marginaleffects import *


dat = get_dataset("thornton").drop_nulls(["outcome", "distance", "age"])
mod = smf.ols("outcome ~ distance + age", data=dat.to_pandas()).fit()


def test_avg_slopes_hc0():
    known = pl.read_csv("tests/r/test_statsmodels_vcov_avg_slopes_hc0.csv")
    unknown = avg_slopes(mod, vcov="HC0")
    known = known.sort("term")
    unknown = unknown.sort("term")
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-4,
    )
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        rtol=1e-3,
    )


def test_avg_slopes_hc1():
    known = pl.read_csv("tests/r/test_statsmodels_vcov_avg_slopes_hc1.csv")
    unknown = avg_slopes(mod, vcov="HC1")
    known = known.sort("term")
    unknown = unknown.sort("term")
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-4,
    )
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        rtol=1e-3,
    )


def test_avg_slopes_hc2():
    known = pl.read_csv("tests/r/test_statsmodels_vcov_avg_slopes_hc2.csv")
    unknown = avg_slopes(mod, vcov="HC2")
    known = known.sort("term")
    unknown = unknown.sort("term")
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-4,
    )
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        rtol=1e-3,
    )


def test_avg_slopes_hc3():
    known = pl.read_csv("tests/r/test_statsmodels_vcov_avg_slopes_hc3.csv")
    unknown = avg_slopes(mod, vcov="HC3")
    known = known.sort("term")
    unknown = unknown.sort("term")
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-4,
    )
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        rtol=1e-3,
    )


def test_avg_predictions_hc3():
    known = pl.read_csv("tests/r/test_statsmodels_vcov_avg_predictions_hc3.csv")
    unknown = avg_predictions(mod, vcov="HC3")
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-4,
    )
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        rtol=1e-3,
    )


def test_vcov_numpy_matrix():
    """Passing a numpy vcov matrix directly should give same results as string."""
    V = np.array(mod.cov_HC3)
    s_str = avg_slopes(mod, vcov="HC3")
    s_mat = avg_slopes(mod, vcov=V)
    np.testing.assert_allclose(
        s_str["std_error"].to_numpy(),
        s_mat["std_error"].to_numpy(),
        rtol=1e-10,
    )
