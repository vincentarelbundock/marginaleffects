"""
Tests comparing autodiff (JAX) vs finite differences results.

These tests verify that JAX-computed standard errors match finite difference
standard errors within a reasonable tolerance for all supported autodiff features.
"""

import pytest
import numpy as np
import polars as pl
import statsmodels.api as sm
import statsmodels.formula.api as smf

from marginaleffects import (
    predictions,
    avg_predictions,
    comparisons,
    avg_comparisons,
    autodiff,
)


def _jax_available():
    try:
        from marginaleffects.autodiff import _JAX_AVAILABLE

        return _JAX_AVAILABLE
    except ImportError:
        return False


# Skip all tests if JAX is not available
pytestmark = pytest.mark.skipif(
    not _jax_available(),
    reason="JAX not available",
)


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def ols_model():
    """OLS model with multiple predictors."""
    np.random.seed(42)
    n = 500
    data = pl.DataFrame(
        {
            "y": np.random.randn(n),
            "x1": np.random.randn(n),
            "x2": np.random.randn(n),
            "x3": np.random.randn(n),
        }
    )
    return smf.ols("y ~ x1 + x2 + x3", data.to_pandas()).fit()


@pytest.fixture
def glm_gaussian_model():
    """GLM model with Gaussian family."""
    np.random.seed(42)
    n = 500
    data = pl.DataFrame(
        {
            "y": np.random.randn(n),
            "x1": np.random.randn(n),
            "x2": np.random.randn(n),
            "x3": np.random.randn(n),
        }
    )
    return sm.GLM.from_formula(
        "y ~ x1 + x2 + x3", data.to_pandas(), family=sm.families.Gaussian()
    ).fit()


@pytest.fixture
def glm_binomial_model():
    """Logistic regression model (GLM with Binomial family)."""
    np.random.seed(42)
    n = 500
    x1 = np.random.randn(n)
    x2 = np.random.randn(n)
    x3 = np.random.randn(n)
    prob = 1 / (1 + np.exp(-(0.5 + 0.3 * x1 - 0.2 * x2 + 0.1 * x3)))
    y = np.random.binomial(1, prob)
    data = pl.DataFrame({"y": y, "x1": x1, "x2": x2, "x3": x3})
    return sm.GLM.from_formula(
        "y ~ x1 + x2 + x3", data.to_pandas(), family=sm.families.Binomial()
    ).fit()


@pytest.fixture
def glm_poisson_model():
    """Poisson regression model."""
    np.random.seed(42)
    n = 500
    x1 = np.random.randn(n)
    x2 = np.random.randn(n)
    mu = np.exp(0.5 + 0.3 * x1 - 0.2 * x2)
    y = np.random.poisson(mu)
    data = pl.DataFrame({"y": y, "x1": x1, "x2": x2})
    return sm.GLM.from_formula(
        "y ~ x1 + x2", data.to_pandas(), family=sm.families.Poisson()
    ).fit()


@pytest.fixture
def large_ols_model():
    """OLS model with many predictors for scalability testing."""
    np.random.seed(42)
    n = 1000
    p = 20
    X = np.random.randn(n, p)
    beta = np.random.randn(p)
    y = X @ beta + np.random.randn(n) * 0.5

    data = {"y": y}
    for i in range(p):
        data[f"x{i}"] = X[:, i]
    data = pl.DataFrame(data)

    formula = "y ~ " + " + ".join([f"x{i}" for i in range(p)])
    return smf.ols(formula, data.to_pandas()).fit()


@pytest.fixture
def ols_model_with_group():
    """OLS model with an extra grouping column for `by` tests."""
    np.random.seed(42)
    n = 400
    x1 = np.random.randn(n)
    x2 = np.random.randn(n)
    x3 = np.random.randn(n)
    group = np.where(x1 > 0, "hi", "lo")
    y = 0.5 + 0.3 * x1 - 0.2 * x2 + 0.1 * x3 + np.random.randn(n) * 0.5
    data = pl.DataFrame({"y": y, "x1": x1, "x2": x2, "x3": x3, "grp": group})
    df = data.to_pandas()
    df["grp"] = df["grp"].astype("category")
    return smf.ols("y ~ x1 + x2 + x3 + C(grp)", df).fit()


# =============================================================================
# Helper function
# =============================================================================


def compare_autodiff_vs_finite_diff(func, model, rtol=1e-4, atol=1e-6, **kwargs):
    """
    Compare results with autodiff enabled vs disabled.

    Asserts that estimates and standard errors match within tolerance.
    """
    autodiff(True)
    result_jax = func(model, **kwargs)

    autodiff(False)
    result_fd = func(model, **kwargs)

    autodiff(None)  # Reset to auto-detect

    # Extract estimates and standard errors
    est_jax = result_jax["estimate"].to_numpy()
    est_fd = result_fd["estimate"].to_numpy()
    se_jax = result_jax["std_error"].to_numpy()
    se_fd = result_fd["std_error"].to_numpy()

    # For comparisons that can produce NaN (e.g., lnratio with negative predictions),
    # only compare non-NaN values where both methods produced valid results
    valid_est = ~(np.isnan(est_jax) | np.isnan(est_fd))
    valid_se = ~(np.isnan(se_jax) | np.isnan(se_fd))

    # Compare estimates (only where both are valid)
    if valid_est.any():
        np.testing.assert_allclose(
            est_jax[valid_est],
            est_fd[valid_est],
            rtol=rtol,
            atol=atol,
            err_msg=f"Estimates don't match for {func.__name__}",
        )

    # Compare standard errors (only where both are valid)
    if valid_se.any():
        np.testing.assert_allclose(
            se_jax[valid_se],
            se_fd[valid_se],
            rtol=rtol,
            atol=atol,
            err_msg=f"Standard errors don't match for {func.__name__}",
        )


# =============================================================================
# Test predictions() - by=False (row-level)
# =============================================================================


class TestPredictionsByFalse:
    """Test predictions(by=False) with autodiff vs finite differences."""

    def test_ols(self, ols_model):
        compare_autodiff_vs_finite_diff(predictions, ols_model, by=False)

    def test_glm_gaussian(self, glm_gaussian_model):
        compare_autodiff_vs_finite_diff(predictions, glm_gaussian_model, by=False)

    def test_glm_binomial(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(predictions, glm_binomial_model, by=False)

    def test_glm_poisson(self, glm_poisson_model):
        compare_autodiff_vs_finite_diff(predictions, glm_poisson_model, by=False)

    def test_large_model(self, large_ols_model):
        compare_autodiff_vs_finite_diff(predictions, large_ols_model, by=False)


# =============================================================================
# Test predictions() - by=True (averaged) / avg_predictions()
# =============================================================================


class TestPredictionsByTrue:
    """Test predictions(by=True) and avg_predictions() with autodiff vs finite differences."""

    def test_ols_predictions_by_true(self, ols_model):
        compare_autodiff_vs_finite_diff(predictions, ols_model, by=True)

    def test_ols_avg_predictions(self, ols_model):
        compare_autodiff_vs_finite_diff(avg_predictions, ols_model)

    def test_glm_gaussian_predictions_by_true(self, glm_gaussian_model):
        compare_autodiff_vs_finite_diff(predictions, glm_gaussian_model, by=True)

    def test_glm_gaussian_avg_predictions(self, glm_gaussian_model):
        compare_autodiff_vs_finite_diff(avg_predictions, glm_gaussian_model)

    def test_glm_binomial_predictions_by_true(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(predictions, glm_binomial_model, by=True)

    def test_glm_binomial_avg_predictions(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(avg_predictions, glm_binomial_model)

    def test_glm_poisson_predictions_by_true(self, glm_poisson_model):
        compare_autodiff_vs_finite_diff(predictions, glm_poisson_model, by=True)

    def test_glm_poisson_avg_predictions(self, glm_poisson_model):
        compare_autodiff_vs_finite_diff(avg_predictions, glm_poisson_model)

    def test_large_model_avg_predictions(self, large_ols_model):
        compare_autodiff_vs_finite_diff(avg_predictions, large_ols_model)


class TestPredictionsGrouped:
    """Test predictions with complex `by` groupings."""

    def test_grouped_predictions(self, ols_model_with_group):
        compare_autodiff_vs_finite_diff(predictions, ols_model_with_group, by="grp")


# =============================================================================
# Test comparisons() - by=False (row-level) with different comparison types
# =============================================================================


class TestComparisonsByFalse:
    """Test comparisons(by=False) with autodiff vs finite differences."""

    # --- OLS model ---
    def test_ols_difference(self, ols_model):
        compare_autodiff_vs_finite_diff(
            comparisons, ols_model, variables="x1", comparison="difference", by=False
        )

    def test_ols_ratio(self, ols_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            ols_model,
            variables="x1",
            comparison="ratio",
            by=False,
            rtol=1e-3,  # ratio can have slightly larger differences
        )

    def test_ols_lnratio(self, ols_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            ols_model,
            variables="x1",
            comparison="lnratio",
            by=False,
            rtol=1e-3,
        )

    def test_ols_lift(self, ols_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            ols_model,
            variables="x1",
            comparison="lift",
            by=False,
            rtol=1e-3,
        )

    # --- GLM Gaussian model ---
    def test_glm_gaussian_difference(self, glm_gaussian_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            glm_gaussian_model,
            variables="x1",
            comparison="difference",
            by=False,
        )

    def test_glm_gaussian_ratio(self, glm_gaussian_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            glm_gaussian_model,
            variables="x1",
            comparison="ratio",
            by=False,
            rtol=1e-3,
        )

    # --- GLM Binomial model ---
    def test_glm_binomial_difference(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            glm_binomial_model,
            variables="x1",
            comparison="difference",
            by=False,
        )

    def test_glm_binomial_lnor(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            glm_binomial_model,
            variables="x1",
            comparison="lnor",
            by=False,
            rtol=1e-3,
        )

    def test_glm_binomial_ratio(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            glm_binomial_model,
            variables="x1",
            comparison="ratio",
            by=False,
            rtol=1e-3,
        )

    # --- GLM Poisson model ---
    def test_glm_poisson_difference(self, glm_poisson_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            glm_poisson_model,
            variables="x1",
            comparison="difference",
            by=False,
        )

    def test_glm_poisson_ratio(self, glm_poisson_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            glm_poisson_model,
            variables="x1",
            comparison="ratio",
            by=False,
            rtol=1e-3,
        )

    # --- Large model ---
    def test_large_model_difference(self, large_ols_model):
        compare_autodiff_vs_finite_diff(
            comparisons,
            large_ols_model,
            variables="x0",
            comparison="difference",
            by=False,
        )


# =============================================================================
# Test comparisons() - by=True (averaged) / avg_comparisons()
# =============================================================================


class TestComparisonsByTrue:
    """Test comparisons(by=True) and avg_comparisons() with autodiff vs finite differences."""

    # --- OLS model ---
    def test_ols_comparisons_by_true(self, ols_model):
        compare_autodiff_vs_finite_diff(
            comparisons, ols_model, variables="x1", comparison="difference", by=True
        )

    def test_ols_avg_comparisons_difference(self, ols_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons, ols_model, variables="x1", comparison="difference"
        )

    def test_ols_avg_comparisons_ratio(self, ols_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons, ols_model, variables="x1", comparison="ratio", rtol=1e-3
        )

    def test_ols_avg_comparisons_lnratio(self, ols_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons, ols_model, variables="x1", comparison="lnratio", rtol=1e-3
        )

    def test_ols_avg_comparisons_lift(self, ols_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons, ols_model, variables="x1", comparison="lift", rtol=1e-3
        )

    # --- GLM Gaussian model ---
    def test_glm_gaussian_avg_comparisons(self, glm_gaussian_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons, glm_gaussian_model, variables="x1", comparison="difference"
        )

    # --- GLM Binomial model ---
    def test_glm_binomial_avg_comparisons_difference(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons, glm_binomial_model, variables="x1", comparison="difference"
        )

    def test_glm_binomial_avg_comparisons_lnor(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons,
            glm_binomial_model,
            variables="x1",
            comparison="lnor",
            rtol=1e-3,
        )

    def test_glm_binomial_avg_comparisons_ratio(self, glm_binomial_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons,
            glm_binomial_model,
            variables="x1",
            comparison="ratio",
            rtol=1e-3,
        )

    # --- GLM Poisson model ---
    def test_glm_poisson_avg_comparisons(self, glm_poisson_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons, glm_poisson_model, variables="x1", comparison="difference"
        )

    # --- Large model ---
    def test_large_model_avg_comparisons(self, large_ols_model):
        compare_autodiff_vs_finite_diff(
            avg_comparisons, large_ols_model, variables="x0", comparison="difference"
        )


class TestComparisonsGrouped:
    """Test comparisons with complex `by` groupings."""

    def test_grouped_comparisons(self, ols_model_with_group):
        compare_autodiff_vs_finite_diff(
            comparisons,
            ols_model_with_group,
            variables="x1",
            comparison="difference",
            by="grp",
        )


# =============================================================================
# Test multiple variables
# =============================================================================


class TestMultipleVariables:
    """Test autodiff with multiple variables in comparisons."""

    def test_ols_multiple_vars_comparisons(self, ols_model):
        """Test comparisons with multiple variables."""
        compare_autodiff_vs_finite_diff(
            comparisons,
            ols_model,
            variables=["x1", "x2"],
            comparison="difference",
            by=False,
        )

    def test_ols_multiple_vars_avg_comparisons(self, ols_model):
        """Test avg_comparisons with multiple variables."""
        compare_autodiff_vs_finite_diff(
            avg_comparisons, ols_model, variables=["x1", "x2"], comparison="difference"
        )

    def test_glm_binomial_multiple_vars(self, glm_binomial_model):
        """Test GLM with multiple variables."""
        compare_autodiff_vs_finite_diff(
            avg_comparisons,
            glm_binomial_model,
            variables=["x1", "x2"],
            comparison="difference",
        )
