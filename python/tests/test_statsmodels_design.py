"""Regression tests for training-design preservation (issue #1758)."""

import numpy as np
import patsy
import polars as pl
import pytest
import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy.stats import t

from marginaleffects import avg_comparisons, comparisons, fit_statsmodels, predictions

FORMULAS = [
    "y ~ cat1 * cat2",
    "y ~ cat1 * num",
    "y ~ cat1 + cat2 + num",
    "y ~ cat1 * cat2 + num",
    "y ~ cat1 * cat2 * num",
]


@pytest.fixture(params=["enum", "reversed_enum", "categorical"])
def interaction_data(request):
    rng = np.random.default_rng(1024)
    n = 500
    data = pl.DataFrame(
        {
            "y": rng.binomial(1, 0.5, n),
            "num": rng.normal(size=n),
            "cat1": pl.Series(rng.choice(["a", "b"], n)).cast(pl.Enum(["a", "b"])),
            "cat2": pl.Series(rng.choice(["x", "y", "z"], n)).cast(
                pl.Enum(["x", "y", "z"])
            ),
        }
    )
    if request.param == "reversed_enum":
        data = data.with_columns(
            pl.col("cat1").cast(pl.Enum(["b", "a"])),
            pl.col("cat2").cast(pl.Enum(["z", "y", "x"])),
        )
    elif request.param == "categorical":
        data = data.with_columns(pl.col("cat1", "cat2").cast(pl.Categorical))
    return data


@pytest.fixture(params=["formulaic", "patsy"])
def fit_model(request):
    def fit(formula, data, engine):
        if request.param == "formulaic":
            model = fit_statsmodels(
                formula, data=data, engine=engine, kwargs_fit={"disp": 0}
            )
            return model, model.get_fitted_model()
        factory = smf.logit if engine is sm.Logit else smf.ols
        fitted = factory(formula, data=data.to_pandas()).fit(disp=0)
        return fitted, fitted

    return fit


def native_design(fitted, data):
    """Use the native fit's saved encoding, independently of the adapter."""
    frame = data.to_pandas()
    training = fitted.model.data.frame
    if isinstance(training, pl.DataFrame):
        training = training.to_pandas()
    for column in ["cat1", "cat2"]:
        if column in frame:
            frame[column] = frame[column].cat.set_categories(
                training[column].cat.categories
            )
    spec = getattr(fitted.model.data, "model_spec", None)
    if spec is None:
        spec = getattr(fitted.model.data, "design_info", None)
    if isinstance(spec, patsy.DesignInfo):
        matrix = patsy.build_design_matrices([spec], frame)[0]
    else:
        matrix = fitted.model.data.orig_exog.model_spec.get_model_matrix(
            frame, context={"np": np}
        )
    return np.asarray(matrix)


def native_prediction_gradient(fitted, matrix):
    estimate = fitted.model.predict(fitted.params, matrix)
    if isinstance(fitted.model, sm.Logit):
        gradient = estimate[:, None] * (1 - estimate[:, None]) * matrix
    else:
        gradient = matrix
    return estimate, gradient


def assert_uncertainty(result, fitted, estimate, gradient):
    variance = np.einsum("ij,jk,ik->i", gradient, fitted.cov_params(), gradient)
    se = np.sqrt(variance)
    # Logit uses numerical coefficient derivatives; OLS also exercises the
    # analytic path. The bound allows finite-difference noise, not design drift.
    np.testing.assert_allclose(result["std_error"], se, rtol=1e-5, atol=1e-8)
    critical = t.ppf(0.975, fitted.df_resid)
    np.testing.assert_allclose(
        result["conf_low"], estimate - critical * se, atol=1e-6, rtol=0
    )
    np.testing.assert_allclose(
        result["conf_high"], estimate + critical * se, atol=1e-6, rtol=0
    )


@pytest.mark.parametrize("formula", FORMULAS)
@pytest.mark.parametrize("engine", [sm.Logit, sm.OLS])
def test_predictions_match_same_native_fit(
    interaction_data, fit_model, formula, engine
):
    model, fitted = fit_model(formula, interaction_data, engine)
    expected = np.asarray(fitted.predict())
    matrix = np.asarray(fitted.model.exog)
    _, gradient = native_prediction_gradient(fitted, matrix)
    for vcov in [True, False]:
        result = predictions(model, vcov=vcov).sort("rowid")
        np.testing.assert_array_equal(result["rowid"], np.arange(len(expected)))
        np.testing.assert_allclose(result["estimate"], expected, atol=1e-12, rtol=0)
        if vcov:
            assert_uncertainty(result, fitted, expected, gradient)


@pytest.mark.parametrize("formula", FORMULAS[-2:])
@pytest.mark.parametrize("engine", [sm.Logit, sm.OLS])
def test_counterfactual_contrasts_match_same_fit(
    interaction_data, fit_model, formula, engine
):
    model, fitted = fit_model(formula, interaction_data, engine)
    native = []
    for level in ["a", "b"]:
        data = interaction_data.with_columns(
            pl.lit(level).cast(interaction_data.schema["cat1"]).alias("cat1")
        )
        native.append(native_prediction_gradient(fitted, native_design(fitted, data)))
    expected = native[1][0] - native[0][0]
    gradient = native[1][1] - native[0][1]
    variables = {"cat1": ["a", "b"]}
    result = comparisons(model, variables=variables).sort("rowid")
    np.testing.assert_allclose(result["estimate"], expected, atol=1e-12, rtol=0)
    assert_uncertainty(result, fitted, expected, gradient)
    average = avg_comparisons(model, variables=variables)
    assert average["contrast"].to_list() == ["b - a"]
    np.testing.assert_allclose(
        average["estimate"], [expected.mean()], atol=1e-12, rtol=0
    )
    assert_uncertainty(
        average,
        fitted,
        np.array([expected.mean()]),
        gradient.mean(axis=0, keepdims=True),
    )


@pytest.mark.parametrize("formula", FORMULAS[-2:])
def test_prediction_grid_keeps_training_categories(
    interaction_data, fit_model, formula
):
    model, fitted = fit_model(formula, interaction_data, sm.Logit)
    # Omit response and fitted levels, reverse row order and incoming category
    # order: neither the training encoding nor the row alignment may change.
    grid = (
        interaction_data.filter((pl.col("cat1") == "b") & (pl.col("cat2") == "y"))
        .drop("y")
        .reverse()
        .with_columns(
            pl.col("cat1").cast(pl.Enum(["b", "a"])),
            pl.col("cat2").cast(pl.Enum(["z", "y", "x"])),
        )
    )
    expected, gradient = native_prediction_gradient(fitted, native_design(fitted, grid))
    result = predictions(model, newdata=grid).sort("rowid")
    np.testing.assert_array_equal(result["rowid"], np.arange(grid.height))
    np.testing.assert_allclose(result["estimate"], expected, atol=1e-12, rtol=0)
    assert_uncertainty(result, fitted, expected, gradient)


@pytest.mark.parametrize(
    "formula", ["y ~ cat1 + bs(num, df=4)", "y ~ cat1 + np.square(num)"]
)
def test_prediction_grid_keeps_fitted_transforms(interaction_data, fit_model, formula):
    model, fitted = fit_model(formula, interaction_data, sm.Logit)
    grid = interaction_data.filter(pl.col("num").is_between(-0.5, 0.5)).drop("y")
    expected, gradient = native_prediction_gradient(fitted, native_design(fitted, grid))
    # Use a controlled step for the nonlinear spline coefficients; the issue
    # formulas above separately verify the default inference settings.
    result = predictions(model, newdata=grid, eps_vcov=1e-6).sort("rowid")
    np.testing.assert_allclose(result["estimate"], expected, atol=1e-12, rtol=0)
    assert_uncertainty(result, fitted, expected, gradient)


def test_treatment_reference_keeps_fitted_category_order():
    # Polars categories enter formulaic in order of appearance; the adapter
    # must reuse that fitted order whether or not C() has extra arguments.
    rng = np.random.default_rng(4)
    data = pl.DataFrame(
        {"g": ["c", "a", "b"] * 50, "x": rng.normal(size=150)}
    ).with_columns(pl.col("g").cast(pl.Categorical))
    data = data.with_columns(y=pl.col("x") + rng.normal(size=150))
    contrasts = {}
    for formula in ["y ~ C(g) + x", "y ~ C(g, Treatment(reference='b')) + x"]:
        model = fit_statsmodels(formula, data, sm.OLS)
        contrasts[formula] = avg_comparisons(model, variables="g")["contrast"]
    assert contrasts["y ~ C(g) + x"].to_list() == ["a - c", "b - c"]
    assert contrasts["y ~ C(g, Treatment(reference='b')) + x"].to_list() == [
        "a - c",
        "b - c",
    ]


@pytest.mark.parametrize(
    ("key", "expected"),
    [
        ("g", "g"),
        ("C(g)", "g"),
        ("C( g )", "g"),
        ("C(g, Treatment(reference='b'))", "g"),
        ("C(g,Sum)", "g"),
    ],
)
def test_encoder_state_variable_names(key, expected):
    from marginaleffects.sanitize.validation import encoder_variable_name

    assert encoder_variable_name(key) == expected
