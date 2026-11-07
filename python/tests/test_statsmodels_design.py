"""Regression tests for training-design preservation (issue #1758)."""

import numpy as np
import pandas as pd
import patsy
import polars as pl
import pytest
import statsmodels.api as sm
import statsmodels.formula
import statsmodels.formula.api as smf
from scipy.stats import t
from statsmodels.miscmodels.ordinal_model import OrderedModel

from marginaleffects import (
    avg_comparisons,
    avg_slopes,
    comparisons,
    fit_statsmodels,
    predictions,
)
from marginaleffects.statsmodels.model import ModelStatsmodels

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


def use_formula_engine(engine):
    """Select statsmodels' formula engine, returning the previous setting."""
    options = getattr(statsmodels.formula, "options", None)
    if options is None:
        if engine == "formulaic":
            pytest.skip("statsmodels 0.14 has only the native Patsy engine")
        return None
    previous = options.formula_engine
    options.formula_engine = engine
    return previous


@pytest.fixture(params=["patsy", "formulaic"])
def formula_engine(request):
    """Run a test under each statsmodels formula engine, then restore it."""
    previous = use_formula_engine(request.param)
    try:
        yield request.param
    finally:
        use_formula_engine(previous)


@pytest.fixture(params=["fit_statsmodels", "patsy", "formulaic"])
def fit_model(request):
    if request.param == "fit_statsmodels":

        def fit(formula, data, engine):
            model = fit_statsmodels(
                formula, data=data, engine=engine, kwargs_fit={"disp": 0}
            )
            return model, model.get_fitted_model()

        yield fit
        return

    def fit(formula, data, engine):
        factory = smf.logit if engine is sm.Logit else smf.ols
        fitted = factory(formula, data=data.to_pandas()).fit(disp=0)
        return fitted, fitted

    # Native formula fits use statsmodels' global engine option.
    previous = use_formula_engine(request.param)
    try:
        yield fit
    finally:
        use_formula_engine(previous)


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
    if spec is None:
        spec = fitted.model.data.orig_exog.model_spec
    if isinstance(spec, patsy.DesignInfo):
        matrix = patsy.build_design_matrices([spec], frame)[0]
    else:
        matrix = spec.get_model_matrix(frame, context={"np": np})
    matrix = pd.DataFrame(np.asarray(matrix), columns=spec.column_names)
    return matrix[fitted.model.exog_names].to_numpy()


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


@pytest.fixture
def mixed_data():
    rng = np.random.default_rng(758)
    n = 400
    data = pd.DataFrame(
        {
            "x": rng.normal(size=n),
            "g": pd.Categorical(rng.choice(["a", "b", "c"], n)),
            "h": pd.Categorical(rng.choice(["u", "v"], n)),
        }
    )
    latent = data["x"] + (data["g"] == "b") - (data["h"] == "v") * 0.5
    latent = latent + rng.logistic(size=n)
    data["yo"] = pd.Categorical(
        np.select([latent < -0.5, latent < 1], ["lo", "mid"], "hi"),
        categories=["lo", "mid", "hi"],
        ordered=True,
    )
    data["ym"] = rng.choice([0, 1, 2], n)
    data["y"] = latent
    return data


def long_predictions(fitted):
    """Native multi-outcome predictions in marginaleffects' (group, rowid) order."""
    return np.asarray(fitted.predict()).T.ravel()


def assert_replays_fitted_spec(fitted):
    vault = ModelStatsmodels(fitted).vault
    assert vault.design_info_patsy is not None or vault.model_spec is not None


@pytest.mark.parametrize(
    "formula", ["yo ~ x + g*h", "yo ~ g*h + x", "yo ~ x + g", "yo ~ bs(x, df=3) + g*h"]
)
def test_ordered_model_matches_native_predict(formula_engine, mixed_data, formula):
    fitted = OrderedModel.from_formula(formula, mixed_data, distr="logit").fit(
        method="bfgs", disp=0, maxiter=500
    )
    assert_replays_fitted_spec(fitted)
    result = predictions(fitted).sort("group", "rowid")
    np.testing.assert_allclose(
        result["estimate"], long_predictions(fitted), atol=1e-10, rtol=0
    )
    # A subset must reuse fitted transform state (e.g. spline knots). Build
    # the expected design by name: under formulaic, statsmodels' own
    # `predict(newdata)` replays OrderedModel's spec in the wrong column order.
    grid = mixed_data.iloc[:25]
    spec = getattr(fitted.model.data, "model_spec", None)
    if spec is None:
        spec = fitted.model.data.design_info
    if isinstance(spec, patsy.DesignInfo):
        matrix = patsy.build_design_matrices([spec], grid)[0]
    else:
        matrix = spec.get_model_matrix(grid)
    matrix = pd.DataFrame(np.asarray(matrix), columns=spec.column_names)
    names = fitted.model.exog_names[: fitted.model.exog.shape[1]]
    probs = fitted.model.predict(fitted.params, matrix[names].to_numpy())
    expected = np.asarray(probs).T.ravel()
    result = predictions(fitted, newdata=grid).sort("group", "rowid")
    np.testing.assert_allclose(result["estimate"], expected, atol=1e-10, rtol=0)


@pytest.mark.parametrize("formula", ["ym ~ x + g*h", "ym ~ g*h + x"])
def test_mnlogit_matches_native_predict(formula_engine, mixed_data, formula):
    fitted = smf.mnlogit(formula, mixed_data).fit(disp=0)
    assert_replays_fitted_spec(fitted)
    result = predictions(fitted).sort("group", "rowid")
    np.testing.assert_allclose(
        result["estimate"], long_predictions(fitted), atol=1e-10, rtol=0
    )


def test_treatment_reference_matches_native_predict(formula_engine, mixed_data):
    fitted = smf.ols("y ~ C(g, Treatment(reference='b')) + x", mixed_data).fit()
    result = predictions(fitted).sort("rowid")
    np.testing.assert_allclose(
        result["estimate"], np.asarray(fitted.predict()), atol=1e-10, rtol=0
    )


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
        ("C(g)", "g"),
        ("C( g )", "g"),
        ("C(g, Treatment(reference='b'))", "g"),
        ("C(g,Sum)", "g"),
    ],
)
def test_encoder_state_variable_names(key, expected):
    from marginaleffects.formula import parse_variables_categorical

    assert parse_variables_categorical(key)[0] == expected


def double(values):
    return 2 * values


def test_user_function_in_formula(formula_engine, mixed_data):
    fitted = smf.ols("y ~ double(x) + g", mixed_data).fit()
    if formula_engine == "patsy":
        # Patsy stores the namespace used at fit time.
        result = avg_slopes(fitted, variables="x")
        np.testing.assert_allclose(
            result["estimate"], [2 * fitted.params["double(x)"]], rtol=1e-6
        )
        return
    # statsmodels' formulaic path does not keep the fitting namespace, so the
    # adapter cannot evaluate `double` again; it must say so clearly.
    with pytest.raises(ValueError, match=r"does not keep the namespace.*double\(x\)"):
        avg_slopes(fitted, variables="x")


def test_rebuilt_design_missing_fitted_column_raises(mixed_data):
    fitted = smf.ols("y ~ x + g", mixed_data).fit()
    model = ModelStatsmodels(fitted)
    with pytest.raises(ValueError, match=r"lacks these columns.*'x'"):
        model._align_to_fitted(np.zeros((2, 3)), ["Intercept", "g[T.b]", "g[T.c]"])


@pytest.mark.parametrize("formula", FORMULAS[-2:])
def test_formula_fallback_aligns_to_fitted_columns(interaction_data, formula):
    # Without a fitted spec, ``get_exog`` rebuilds the design with patsy, which
    # orders these interaction columns differently from formulaic (#1758).
    model = fit_statsmodels(formula, interaction_data, sm.OLS)
    expected = predictions(model).sort("rowid")["estimate"].to_numpy()
    model.vault.model_spec = None
    model.vault.formula_engine = "patsy"
    result = predictions(model).sort("rowid")["estimate"].to_numpy()
    np.testing.assert_allclose(result, expected, atol=1e-12, rtol=0)


def test_fit_statsmodels_stores_model_spec(interaction_data):
    model = fit_statsmodels("y ~ cat1 * cat2 + num", interaction_data, sm.OLS)
    assert model.vault.model_spec is not None
