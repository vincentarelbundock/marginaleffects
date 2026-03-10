import polars as pl
import marginaleffects as me
from polars.testing import assert_series_equal
from tests.helpers import wage_panel_pd
from linearmodels.panel import PanelOLS

formula = "lwage ~ exper * hours * educ * married - 1"
data = wage_panel_pd
mod = me.fit_linearmodels(formula, data, engine=PanelOLS)


def test_predictions_01():
    unknown = me.predictions(mod)
    known = pl.read_csv("tests/r/test_feols_linearmodels_panelols_predictions_01.csv")
    assert_series_equal(unknown["estimate"], known["estimate"], abs_tol=1e-7)
    assert_series_equal(
        unknown["std_error"], known["std.error"], check_names=False, abs_tol=1e-7
    )


def test_predictions_02():
    unknown = me.predictions(mod, by="married")
    known = pl.read_csv("tests/r/test_feols_linearmodels_panelols_predictions_02.csv")
    assert_series_equal(unknown["estimate"], known["estimate"], abs_tol=1e-7)
    assert_series_equal(
        unknown["std_error"], known["std.error"], check_names=False, abs_tol=1e-7
    )


def test_comparisons_01():
    unknown = me.comparisons(mod).sort(["term", "contrast", "rowid"])
    known = pl.read_csv(
        "tests/r/test_feols_linearmodels_panelols_comparisons_01.csv"
    ).sort(["term", "contrast", "rowid"])
    assert_series_equal(unknown["estimate"], known["estimate"], abs_tol=1e-7)
    assert_series_equal(
        unknown["std_error"], known["std.error"], check_names=False, abs_tol=1e-7
    )


def test_comparisons_02():
    unknown = me.comparisons(mod, by="married").sort(["term", "married"])
    known = pl.read_csv(
        "tests/r/test_feols_linearmodels_panelols_comparisons_02.csv"
    ).sort(["term", "married"])
    assert_series_equal(unknown["estimate"], known["estimate"])
    assert_series_equal(unknown["std_error"], known["std.error"], check_names=False)


def test_slopes_01():
    unknown = me.slopes(mod)
    known = pl.read_csv("tests/r/test_feols_linearmodels_panelols_slopes_01.csv")
    unknown = unknown.sort(["term", "contrast", "rowid"])
    known = known.sort(["term", "contrast", "rowid"])
    assert_series_equal(unknown["estimate"], known["estimate"], abs_tol=1e-7)
    # TODO: bad tolerance
    assert_series_equal(
        unknown["std_error"], known["std.error"], check_names=False, abs_tol=1e-2
    )


def test_slopes_02():
    unknown = me.slopes(mod, by="married").sort(["term", "married"])
    known = pl.read_csv("tests/r/test_feols_linearmodels_panelols_slopes_02.csv").sort(
        ["term", "married"]
    )
    assert_series_equal(unknown["estimate"], known["estimate"], abs_tol=1e-7)
    # TODO: bad tolerance
    assert_series_equal(
        unknown["std_error"], known["std.error"], check_names=False, abs_tol=1e-4
    )
