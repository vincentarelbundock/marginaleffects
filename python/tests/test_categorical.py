import marginaleffects
from marginaleffects import *
import polars as pl
import statsmodels.formula.api as smf
from polars.testing import assert_series_equal

# mtcars is redefined below, so no import needed
import pytest


hiv = get_dataset("thornton").to_pandas()
mtcars = (
    get_dataset("mtcars", "datasets")
    .sort("gear")
    .with_columns(pl.col("gear").cast(pl.String).cast(pl.Categorical))
    .to_pandas()
)


def test_avg_predictions_by_cat():
    # R results
    e = pl.Series([0.669811320754608, 0.672553348049938, 0.720383275261309])
    s = pl.Series([0.0241453345825489, 0.011617570113057, 0.0121533676309027])

    mod = smf.logit("outcome ~ agecat + incentive", hiv).fit()
    p = predictions(mod, by="agecat")
    assert_series_equal(p["estimate"], e, check_names=False)
    assert_series_equal(p["std_error"], s, check_names=False)
    k = (
        pl.DataFrame(hiv)
        .drop_nulls(subset=["outcome", "agecat", "incentive"])
        .to_pandas()
    )
    p = predictions(mod, by="agecat", newdata=k)
    assert_series_equal(p["estimate"], e, check_names=False)
    assert_series_equal(p["std_error"], s, check_names=False, rel_tol=1e-4)


def test_mtcars_avg_slopes():
    mod = smf.ols("mpg ~ wt + C(gear)", data=mtcars).fit()
    s = avg_slopes(mod)
    assert s.shape[0] == 3
    assert all(s["contrast"] == ["4 - 3", "5 - 3", "dY/dX"])


def test_hiv_avg_slopes():
    mod = smf.ols("outcome ~ incentive + agecat", data=hiv).fit()
    s = avg_slopes(mod)
    assert s.shape[0] == 3
    assert all(s["contrast"] == ["18 to 35 - <18", ">35 - <18", "1 - 0"])


def test_avg_predictions_raises_categorical_error():
    mtcars = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + C(gear)", data=mtcars.to_pandas()).fit()
    with pytest.raises(Exception, match=".*categorical.*"):
        avg_predictions(mod, by="gear")


def test_issue185():
    dat = marginaleffects.utils.ingest(mtcars)
    dat = dat.with_columns(pl.col("gear").cast(pl.String).cast(pl.Categorical))
    mod = smf.ols("mpg ~ C(gear)", data=dat.to_pandas()).fit()
    p = avg_comparisons(mod, by="gear")
    assert p.shape[0] == 6
