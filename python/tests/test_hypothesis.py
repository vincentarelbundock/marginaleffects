import numpy as np
import polars as pl
import statsmodels.formula.api as smf
from polars.testing import assert_frame_equal, assert_series_equal

from marginaleffects import *
from marginaleffects.hypothesis import get_hypothesis
from tests.utilities import sort_categories_pandas

mtcars = get_dataset("mtcars", "datasets")
mod = smf.ols("mpg ~ hp + cyl", data=sort_categories_pandas(mtcars.to_pandas())).fit()
p = predictions(mod, by="cyl")["estimate"]


def test_predictions_reference():
    q = predictions(mod, by="cyl", hypothesis="difference~reference")["estimate"]
    assert np.isclose(p[1] - p[0], q[0])
    assert np.isclose(p[2] - p[0], q[1])
    q = predictions(mod, by="cyl", hypothesis="ratio~reference")["estimate"]
    assert np.isclose(p[1] / p[0], q[0])
    assert np.isclose(p[2] / p[0], q[1])
    q = predictions(mod, by="cyl", hypothesis="difference~revreference")["estimate"]
    assert np.isclose(p[0] - p[1], q[0])
    assert np.isclose(p[0] - p[2], q[1])
    q = predictions(mod, by="cyl", hypothesis="ratio~revreference")["estimate"]
    assert np.isclose(p[0] / p[1], q[0])
    assert np.isclose(p[0] / p[2], q[1])


def test_predictions_sequential():
    q = predictions(mod, by="cyl", hypothesis="difference~sequential")["estimate"]
    assert np.isclose(p[1] - p[0], q[0])
    assert np.isclose(p[2] - p[1], q[1])
    q = predictions(mod, by="cyl", hypothesis="ratio~sequential")["estimate"]
    assert np.isclose(p[1] / p[0], q[0])
    assert np.isclose(p[2] / p[1], q[1])
    q = predictions(mod, by="cyl", hypothesis="difference~sequential")["estimate"]
    assert np.isclose(p[1] - p[0], q[0])
    assert np.isclose(p[2] - p[1], q[1])
    q = predictions(mod, by="cyl", hypothesis="ratio~sequential")["estimate"]
    assert np.isclose(p[1] / p[0], q[0])
    assert np.isclose(p[2] / p[1], q[1])


def test_predictions_pairwise():
    p = predictions(mod, by="cyl")["estimate"]
    q = predictions(mod, by="cyl", hypothesis="ratio~pairwise")["estimate"]
    assert np.isclose(p[1] / p[0], q[0])
    assert np.isclose(p[2] / p[0], q[1])
    assert np.isclose(p[2] / p[1], q[2])
    p = predictions(mod, by="cyl")["estimate"]
    q = predictions(mod, by="cyl", hypothesis="difference~pairwise")["estimate"]
    assert np.isclose(p[1] - p[0], q[0])
    assert np.isclose(p[2] - p[0], q[1])
    assert np.isclose(p[2] - p[1], q[2])
    p = predictions(mod, by="cyl")["estimate"]
    q = predictions(mod, by="cyl", hypothesis="ratio~revpairwise")["estimate"]
    assert np.isclose(p[0] / p[1], q[0])
    assert np.isclose(p[0] / p[2], q[1])
    assert np.isclose(p[1] / p[2], q[2])
    p = predictions(mod, by="cyl")["estimate"]
    q = predictions(mod, by="cyl", hypothesis="difference~revpairwise")["estimate"]
    assert np.isclose(p[0] - p[1], q[0])
    assert np.isclose(p[0] - p[2], q[1])
    assert np.isclose(p[1] - p[2], q[2])


def test_ratio_hypothesis_uses_null_one():
    out = predictions(mod, by="cyl", hypothesis="ratio~sequential")
    expected = (out["estimate"] - 1) / out["std_error"]
    assert_series_equal(
        out["statistic"],
        expected,
        check_names=False,
        abs_tol=1e-9,
        rel_tol=1e-9,
    )


def test_ratio_hypothesis_sequence_uses_null_one():
    out = predictions(mod, by="cyl", hypothesis=["ratio~sequential"])
    expected = (out["estimate"] - 1) / out["std_error"]
    assert_series_equal(
        out["statistic"],
        expected,
        check_names=False,
        abs_tol=1e-9,
        rel_tol=1e-9,
    )


def test_comparisons_by():
    mtcars = (
        get_dataset("mtcars", "datasets")
        .sort("cyl")
        .with_columns(pl.col("cyl").cast(pl.String).cast(pl.Categorical))
        .to_pandas()
    )
    mod = smf.ols("mpg ~ hp * C(cyl)", data=mtcars).fit()
    q = avg_comparisons(mod, hypothesis="ratio~sequential")
    assert q.shape[0] == 2

    q = avg_comparisons(
        mod, variables="hp", by="cyl", hypothesis="difference~revpairwise"
    )
    assert q["estimate"][0] < 0
    assert q["estimate"][1] < 0
    assert q["estimate"][2] > 0
    assert q.shape[0] == 3


def test_hypothesis_by_01():
    dat = mtcars.with_columns(pl.col("cyl").cast(pl.String).cast(pl.Categorical))
    mod = smf.ols(
        "mpg ~ hp * C(cyl) * am", data=sort_categories_pandas(dat.to_pandas())
    ).fit()
    p = avg_predictions(mod, by=["am", "cyl"], hypothesis="~ reference | am")
    r_b = pl.Series([-3.775, -7.85, -7.50833333333336, -12.675])
    r_se = pl.Series(
        [2.19340004101813, 1.85376132615083, 1.94424126381778, 2.27038464937531]
    )
    assert_series_equal(p["estimate"], r_b, check_names=False)
    assert_series_equal(p["std_error"], r_se, check_names=False)


def numpy_formula():
    dat = get_dataset("thornton")
    dat.head(6)
    mod = ols("outcome ~ agecat - 1", data=dat.to_pandas()).fit()
    h = hypotheses(mod, hypothesis="b1**2 * np.exp(b0) = 0")
    assert h.height == 1


def test_get_hypothesis_sequence_of_strings():
    base = pl.DataFrame(
        {"term": ["a", "b", "c"], "estimate": [1.0, 2.0, -0.5]},
    )

    single_1 = get_hypothesis(base, hypothesis="b - a = 0")
    single_2 = get_hypothesis(base, hypothesis="c = 0")
    combined = get_hypothesis(base, hypothesis=["b - a = 0", "c = 0"])

    expected = pl.concat([single_1, single_2], how="vertical")
    assert_frame_equal(combined, expected)


def test_trash_example_matches_r_output():
    mtcars = pl.read_csv("tests/data/mtcars.csv").with_columns(
        pl.col("cyl").cast(pl.String).cast(pl.Categorical)
    )
    mod = smf.ols("mpg ~ C(cyl)", data=sort_categories_pandas(mtcars.to_pandas())).fit()

    hypotheses = ["b1 - b0 = 0", "b2 - b0 = 0"]
    out = avg_predictions(mod, by="cyl", hypothesis=hypotheses)

    r_estimate = pl.Series([-6.92077922077923, -11.5636363636364])
    r_std_error = pl.Series([1.55834813148785, 1.29862349320347])

    assert_series_equal(
        out["estimate"],
        r_estimate,
        check_names=False,
        abs_tol=1e-6,
        rel_tol=1e-6,
    )
    assert_series_equal(
        out["std_error"],
        r_std_error,
        check_names=False,
        abs_tol=1e-6,
        rel_tol=1e-6,
    )
