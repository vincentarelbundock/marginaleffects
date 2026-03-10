# FET re

import polars as pl
import statsmodels.formula.api as smf
import statsmodels.api as sm
from polars.testing import assert_series_equal
import pytest

import marginaleffects
from marginaleffects import *
from tests.helpers import mtcars
from tests.utilities import convert_to_categorical_pandas

dat = mtcars
# Convert to pandas and then convert cyl to categorical with numeric ordering
# gear stays numeric (not converted, used as continuous variable)
dat_pd = dat.to_pandas()
dat_pd = convert_to_categorical_pandas(dat_pd, ["cyl"], numeric_order=True)

# Fixed formulas: removed C(gear) since gear should be numeric
# mod1: additive model, mod2: interaction between gear (numeric) and cyl (categorical)
mod1 = smf.ols("mpg ~ C(cyl) + wt + gear", data=dat_pd).fit()
mod2 = smf.ols("mpg ~ gear * C(cyl) + wt", data=dat_pd).fit()

cmp1 = comparisons(mod1, newdata=datagrid())
cmp2 = comparisons(mod2, newdata=datagrid(), cross=False)
cmp3 = comparisons(mod2, variables=["cyl", "gear"], newdata=datagrid(), cross=True)


def test_def_contrast():
    # comment in r version:
    # interaction automatic flip from NULL to useful
    assert "contrast" in cmp2.columns
    assert all([s in cmp3.columns for s in ["contrast_cyl", "contrast_gear"]])


def test_interaction_01_data():
    df = get_dataset("interaction_01")
    df = df.with_columns(pl.col("M").cast(pl.Categorical))
    mod = smf.glm("Y ~ X * M", data=df.to_pandas(), family=sm.families.Binomial()).fit()
    cmp_py = avg_comparisons(mod, by=["X", "M"]).sort(["term", "contrast"])
    cmp_r = pl.read_csv("tests/r/test_comparisons_interaction_01.csv").sort(
        ["term", "contrast"]
    )
    assert_series_equal(cmp_py["estimate"], cmp_r["estimate"], rel_tol=1e-2)
    assert_series_equal(
        cmp_py["std_error"], cmp_r["std.error"], check_names=False, rel_tol=3e-2
    )


# Issue #230 (closed)
# Issue ?
def test_interaction_emmeans():
    dat = mtcars
    # Convert to pandas and then convert am/cyl to categorical with numeric ordering
    # gear stays numeric (used as continuous variable)
    dat_pd = dat.to_pandas()
    dat_pd = convert_to_categorical_pandas(dat_pd, ["am", "cyl"], numeric_order=True)
    mod_em = smf.ols("mpg ~ C(am) + C(cyl) + wt + gear", data=dat_pd).fit()
    cmp = comparisons(
        mod_em, variables={"cyl": "all", "am": "all"}, newdata=datagrid(), cross=True
    )
    cmp_r_emmeans = pl.read_csv("tests/r/test_comparisons_interaction_emmeans.csv")
    # Sort both by absolute estimate to match ordering
    cmp_sorted = cmp.sort(cmp["estimate"].abs())
    cmp_r_sorted = cmp_r_emmeans.sort(cmp_r_emmeans["estimate"].abs())
    assert_series_equal(
        cmp_sorted["estimate"].abs(),
        cmp_r_sorted["estimate"].abs(),
        check_names=False,
        rel_tol=1e-4,
    )
    assert_series_equal(
        cmp_sorted["std_error"].abs(),
        cmp_r_sorted["SE"].abs(),
        check_names=False,
        rel_tol=1e-3,
    )


# Unclear translation: tidy analogue? Polars vs pandas?
"""
# tidy does not error (no validity)
mod = lm(mpg ~ factor(am) + factor(cyl) + wt + gear, data = mtcars)
cmp = comparisons(mod, variables = c("am", "cyl"), cross = True)
tid = tidy(cmp)
expect_true(all(tid$term == "cross"))
"""


# `variables` must be specified
def test_variables_specified():
    # All variables used as numeric (no C() in formula)
    mod = smf.ols("mpg ~ am + cyl + wt + gear", data=mtcars.to_pandas()).fit()
    cmp = comparisons(mod, variables=["am", "cyl"], cross=True)
    isinstance(cmp, marginaleffects.result.MarginaleffectsResult)
    with pytest.raises(Exception):
        comparisons(mod, cross=True)


""" 
# interaction (no validity)
mod = lm(mpg ~ factor(am) * factor(cyl) + wt + gear, data = mtcars)

# one row only means tidy is same nrows
# on some machines I get 21 rows instead of 18, but can't replicate. maybe look into this if I have the energy. Seems minor.
cmp = comparisons(
    mod,
    variables = list("cyl" = "all", "am" = "all"),
    newdata = datagrid(),
    cross = True
)
expect_true(nrow(cmp) > 17)
expect_true(nrow(tidy(cmp)) > 17)
"""


# deprecated argument
def test_depr_argument_comparisons():
    with pytest.raises(TypeError):
        comparisons(mod1, interaction=True)
