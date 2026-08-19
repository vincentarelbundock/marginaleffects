import numpy as np
import polars as pl
import pytest
import statsmodels.api as sm
import statsmodels.formula.api as smf

from marginaleffects import (
    avg_comparisons,
    comparisons,
    get_dataset,
    predictions,
)


def _model():
    return smf.ols("mpg ~ hp + wt", get_dataset("mtcars").to_pandas()).fit()


def test_public_results_report_jacobian_method():
    model = _model()
    assert predictions(model).jacobian_method == "analytic"
    assert comparisons(model, variables="hp").jacobian_method == "analytic"
    assert (
        avg_comparisons(model, variables="hp", by="cyl").jacobian_method == "analytic"
    )


def test_explicit_step_reports_finite_difference():
    out = predictions(_model(), eps_vcov=1e-5)
    assert out.jacobian_method == "finite_difference"


def test_nonlinear_hypothesis_reports_composed_numeric_stage():
    out = predictions(_model(), by="cyl", hypothesis="b1**2 - b0 = 0")
    assert out.jacobian_method == "analytic+numeric_stage"


def test_custom_comparison_without_y_falls_back_without_trusted_derivative():
    model = _model()
    out = comparisons(model, variables="hp", comparison=lambda hi, lo: hi / lo)
    assert out.jacobian_method != "analytic"
    assert np.isfinite(out.jacobian).all()


def test_custom_comparison_using_y_falls_back():
    model = _model()
    out = comparisons(
        model,
        variables="hp",
        comparison=lambda hi, lo, y: (hi - lo) / y,
    )
    assert out.jacobian_method != "analytic"


@pytest.mark.parametrize(
    "comparison",
    ["difference", "ratio", "lnratio", "lift", "dydx", "dyex", "expdydx"],
)
def test_public_builtin_families_use_analytic_path(comparison):
    model = _model()
    out = comparisons(model, variables="hp", comparison=comparison)
    assert out.jacobian_method == "analytic"


@pytest.mark.parametrize(
    "comparison", ["difference", "ratio", "lnratio", "lnor", "lift"]
)
def test_public_weighted_averages_use_analytic_path(comparison):
    data = get_dataset("mtcars").with_columns(w=pl.col("wt") + 1)
    model = smf.glm(
        "am ~ hp + wt", data.to_pandas(), family=sm.families.Binomial()
    ).fit()
    out = avg_comparisons(
        model,
        variables="hp",
        comparison=comparison,
        wts="w",
    )
    assert out.jacobian_method == "analytic"


@pytest.mark.parametrize("comparison", ["eyex", "eydx"])
def test_y_dependent_elasticities_fall_back(comparison):
    out = comparisons(_model(), variables="hp", comparison=comparison)
    assert out.jacobian_method != "analytic"


def test_result_operations_preserve_jacobian_method():
    out = predictions(_model())
    assert out.filter(pl.col("rowid") < 2).jacobian_method == "analytic"


def test_glm_offset_falls_back():
    data = get_dataset("mtcars").to_pandas()
    model = smf.glm(
        "am ~ hp + wt",
        data,
        family=sm.families.Binomial(),
        offset=np.linspace(0.1, 0.2, len(data)),
    ).fit()
    assert predictions(model).jacobian_method != "analytic"
