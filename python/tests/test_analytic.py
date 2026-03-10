import numpy as np
import polars as pl
import statsmodels.formula.api as smf
import statsmodels.api as sm
from scipy.stats import logistic
from polars.testing import assert_series_equal

from marginaleffects import *


# Translated from https://github.com/vincentarelbundock/marginaleffects/blob/main/inst/tinytest/test-analytic.R


def test_linear_quadratic():
    np.random.seed(1)
    f = "y ~ x + I(x**2)"

    def truth(x):
        return 1 + 2 * x

    N = 100000
    dat = pl.DataFrame({"x": np.random.normal(size=N)})
    dat = dat.with_columns(
        y=(1 + pl.col("x") + pl.col("x") ** 2 + np.random.normal(size=N))
    )
    mod = smf.ols(f, dat.to_pandas()).fit()
    nd = datagrid(newdata=dat, x=range(-2, 3))
    res = slopes(mod, newdata=nd)
    res = res.with_columns(truth=truth(pl.col("x")).cast(pl.Float64))
    assert_series_equal(res["estimate"], res["truth"], check_names=False, abs_tol=0.01)


def test_linear_log():
    np.random.seed(1)
    f = "y ~ np.log(x)"

    def truth(x):
        return 1 / x

    N = 10000
    dat = pl.DataFrame({"x": np.random.uniform(size=N)})
    dat = dat.with_columns(y=np.log(pl.col("x")) + np.random.normal(size=N))
    mod = smf.ols(f, dat.to_pandas()).fit()
    nd = datagrid(newdata=dat, x=range(1, 5))
    res = slopes(mod, newdata=nd)
    res = res.with_columns(truth=truth(pl.col("x")).cast(pl.Float64))
    assert_series_equal(res["estimate"], res["truth"], check_names=False, abs_tol=0.02)


def test_logit():
    np.random.seed(1)
    f = "y ~ x"
    beta0 = 1
    beta1 = 0.2

    def truth(x):
        return beta1 * logistic.pdf(beta0 + beta1 * x)

    N = int(1e5)
    dat = pl.DataFrame({"x": np.random.normal(size=N)})
    pr = logistic.cdf(beta0 + beta1 * dat["x"])
    dat = dat.with_columns(y=np.random.binomial(1, pr, size=N))
    mod = smf.glm(f, data=dat, family=sm.families.Binomial()).fit()
    nd = datagrid(newdata=dat, x=range(-10, 11))
    res = slopes(mod, newdata=nd)
    tru = truth(res["x"])
    res = res.with_columns(truth=tru)
    assert_series_equal(res["estimate"], res["truth"], check_names=False, abs_tol=0.01)


############################################################################
#  golder tests copied to the R version of `marginaleffects`               #
#  from the `margins` github repository on 2021-09-18                      #
#  and translated to Python on 2024-02-15                                  #
#  LICENSE: MIT Thomas J. Leeper                                           #
############################################################################

# tests based on formulae from Matt Golder's OLS examples, for numerical accuracy and precision
# http://mattgolder.com/wp-content/uploads/2015/05/standarderrors1.png


def test_golder():
    # example data for tests
    np.random.seed(1)
    N = 25
    dat = pl.DataFrame(
        {
            "w": np.random.normal(size=N),
            "x": np.random.normal(size=N),
            "z": np.random.normal(size=N),
        }
    )
    dat = dat.with_columns(
        y=pl.col("w")
        + pl.col("x")
        + pl.col("z")
        + pl.col("w") * pl.col("x")
        + pl.col("w") * pl.col("z")
        + pl.col("x") * pl.col("z")
        + pl.col("w") * pl.col("x") * pl.col("z")
        + np.random.normal(size=N)
    )

    # set comparison tolerance
    tol = 0.001
    tol_se = 0.01

    # Golder Interaction Case 1a/1b correct
    f1_1 = "y ~ x + z + x:z"
    mod = smf.ols(f1_1, data=dat).fit()
    res = slopes(mod)
    dydx = mod.params["x"] + (dat["z"] * mod.params["x:z"])
    vcov = mod.cov_params()
    sedydx = np.sqrt(
        vcov.at["x", "x"]
        + dat["z"] ** 2 * vcov.at["x:z", "x:z"]
        + 2 * dat["z"] * vcov.at["x", "x:z"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["estimate"],
        dydx,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["std_error"],
        sedydx,
        check_names=False,
        abs_tol=tol_se,
    )

    # Golder Interaction Case 2 correct
    f1_2 = "y ~ x + z + w + x:z + z:w"
    mod = smf.ols(f1_2, data=dat).fit()
    res = slopes(mod)
    dydx = mod.params["x"] + (dat["z"] * mod.params["x:z"])
    vcov = mod.cov_params()
    sedydx = np.sqrt(
        vcov.at["x", "x"]
        + dat["z"] ** 2 * vcov.at["x:z", "x:z"]
        + 2 * dat["z"] * vcov.at["x", "x:z"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["estimate"],
        dydx,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["std_error"],
        sedydx,
        check_names=False,
        abs_tol=tol_se,
    )

    # Golder Interaction Case 3 correct
    f1_3 = "y ~ x + z + w + x:z + x:w + z:w + x:z:w"
    mod = smf.ols(f1_3, data=dat).fit()
    res = slopes(mod)
    dydx = (
        mod.params["x"]
        + dat["z"] * mod.params["x:z"]
        + dat["w"] * mod.params["x:w"]
        + dat["z"] * dat["w"] * mod.params["x:z:w"]
    )
    vcov = mod.cov_params()
    sedydx = np.sqrt(
        vcov.at["x", "x"]
        + dat["z"] ** 2 * vcov.at["x:z", "x:z"]
        + dat["w"] ** 2 * vcov.at["x:w", "x:w"]
        + dat["z"] ** 2 * dat["w"] ** 2 * vcov.at["x:z:w", "x:z:w"]
        + 2 * dat["z"] * vcov.at["x", "x:z"]
        + 2 * dat["w"] * vcov.at["x", "x:w"]
        + 2 * dat["z"] * dat["w"] * vcov.at["x", "x:z:w"]
        + 2 * dat["z"] * dat["w"] * vcov.at["x:z", "x:w"]
        + 2 * dat["z"] ** 2 * dat["w"] * vcov.at["x:z", "x:z:w"]
        + 2 * dat["z"] * dat["w"] ** 2 * vcov.at["x:w", "x:z:w"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["estimate"],
        dydx,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["std_error"],
        sedydx,
        check_names=False,
        abs_tol=tol_se,
    )

    # Golder Quadratic Case 1 correct
    f2_1 = "y ~ x + I(x**2)"
    mod = smf.ols(f2_1, data=dat.to_pandas()).fit()
    res = slopes(mod)
    dydx = mod.params["x"] + (2 * mod.params["I(x ** 2)"] * dat["x"])
    vcov = mod.cov_params()
    sedydx = np.sqrt(
        vcov.at["x", "x"]
        + 4 * dat["x"] ** 2 * vcov.at["I(x ** 2)", "I(x ** 2)"]
        + 4 * dat["x"] * vcov.at["x", "I(x ** 2)"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["estimate"],
        dydx,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["std_error"],
        sedydx,
        check_names=False,
        abs_tol=tol_se,
    )

    # Golder Quadratic Case 2 correct
    f2_2 = "y ~ x + I(x**2) + z"
    mod = smf.ols(f2_2, data=dat.to_pandas()).fit()
    res = slopes(mod)
    dydx = mod.params["x"] + (2 * mod.params["I(x ** 2)"] * dat["x"])
    vcov = mod.cov_params()
    sedydx = np.sqrt(
        vcov.at["x", "x"]
        + 4 * dat["x"] ** 2 * vcov.at["I(x ** 2)", "I(x ** 2)"]
        + 4 * dat["x"] * vcov.at["x", "I(x ** 2)"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["estimate"],
        dydx,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["std_error"],
        sedydx,
        check_names=False,
        abs_tol=tol_se,
    )

    # Golder Quadratic Case 3a/3b correct
    f2_3 = "y ~ x + I(x**2) + z + x:z"
    mod = smf.ols(f2_3, data=dat.to_pandas()).fit()
    res = slopes(mod)
    vcov = mod.cov_params()

    # ME with respect to x
    dydx = (
        mod.params["x"]
        + 2 * mod.params["I(x ** 2)"] * dat["x"]
        + dat["z"] * mod.params["x:z"]
    )
    sedydx = np.sqrt(
        vcov.at["x", "x"]
        + 4 * dat["x"] ** 2 * vcov.at["I(x ** 2)", "I(x ** 2)"]
        + dat["z"] ** 2 * vcov.at["x:z", "x:z"]
        + 4 * dat["x"] * vcov.at["x", "I(x ** 2)"]
        + 2 * dat["z"] * vcov.at["x", "x:z"]
        + 4 * dat["x"] * dat["z"] * vcov.at["I(x ** 2)", "x:z"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["estimate"],
        dydx,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["std_error"],
        sedydx,
        check_names=False,
        abs_tol=tol_se,
    )

    # ME with respect to z
    dydz = mod.params["z"] + (dat["x"] * mod.params["x:z"])
    sedydz = np.sqrt(
        vcov.at["z", "z"]
        + dat["x"] ** 2 * vcov.at["x:z", "x:z"]
        + 2 * dat["x"] * vcov.at["z", "x:z"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "z")["estimate"],
        dydz,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "z")["std_error"],
        sedydz,
        check_names=False,
        abs_tol=tol_se,
    )

    # Golder Quadratic Case 4a/4b correct
    f2_4 = "y ~ x + I(x**2) + z + x:z + I(x**2):z"
    mod = smf.ols(f2_4, data=dat.to_pandas()).fit()
    res = slopes(mod)
    vcov = mod.cov_params()

    # ME with respect to x
    dydx = (
        mod.params["x"]
        + (2 * mod.params["I(x ** 2)"] * dat["x"])
        + (dat["z"] * mod.params["x:z"])
        + (2 * dat["x"] * dat["z"] * mod.params["I(x ** 2):z"])
    )
    sedydx = np.sqrt(
        vcov.at["x", "x"]
        + 4 * dat["x"] ** 2 * vcov.at["I(x ** 2)", "I(x ** 2)"]
        + dat["z"] ** 2 * vcov.at["x:z", "x:z"]
        + 4 * dat["x"] ** 2 * dat["z"] ** 2 * vcov.at["I(x ** 2):z", "I(x ** 2):z"]
        + 4 * dat["x"] * vcov.at["x", "I(x ** 2)"]
        + 2 * dat["z"] * vcov.at["x", "x:z"]
        + 4 * dat["x"] * dat["z"] * vcov.at["I(x ** 2)", "x:z"]
        + 4
        * dat["x"]
        * dat["z"]
        * vcov.at[
            "x",
            "I(x ** 2):z",
        ]
        + 8 * dat["x"] ** 2 * dat["z"] * vcov.at["I(x ** 2)", "I(x ** 2):z"]
        + 4 * dat["x"] * dat["z"] ** 2 * vcov.at["x:z", "I(x ** 2):z"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["estimate"],
        dydx,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "x")["std_error"],
        sedydx,
        check_names=False,
        abs_tol=tol_se,
    )

    # ME with respect to z
    dydz = (
        mod.params["z"]
        + dat["x"] * mod.params["x:z"]
        + dat["x"] ** 2 * mod.params["I(x ** 2):z"]
    )
    sedydz = np.sqrt(
        vcov.at["z", "z"]
        + dat["x"] ** 2 * vcov.at["x:z", "x:z"]
        + dat["x"] ** 4 * vcov.at["I(x ** 2):z", "I(x ** 2):z"]
        + 2 * dat["x"] * vcov.at["z", "x:z"]
        + 2 * dat["x"] ** 2 * vcov.at["z", "I(x ** 2):z"]
        + 2 * dat["x"] ** 3 * vcov.at["x:z", "I(x ** 2):z"]
    )
    assert_series_equal(
        res.filter(pl.col("term") == "z")["estimate"],
        dydz,
        check_names=False,
        abs_tol=tol,
    )
    assert_series_equal(
        res.filter(pl.col("term") == "z")["std_error"],
        sedydz,
        check_names=False,
        abs_tol=tol_se,
    )
