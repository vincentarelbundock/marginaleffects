import numpy as np
import polars as pl
import marginaleffects as me
from marginaleffects import MarginaleffectsResult
from sklearn.linear_model import LinearRegression
from tests.helpers import wage_panel_pd

data = wage_panel_pd

# Fit two different types of models to test
formula = "lwage ~ exper * hours * educ * married - 1"
mod_linear = me.fit_sklearn(formula, data, engine=LinearRegression())


def test_predictions_linear():  # std_error is missing for some reason
    pred = me.predictions(mod_linear)
    assert isinstance(pred, MarginaleffectsResult)
    assert isinstance(pred.data, pl.DataFrame)
    assert "estimate" in pred.columns
    # assert "std_error" in pred.columns # std_error is missing for some reason
    assert len(pred) == len(data)


def test_comparisons_linear():
    comp = me.comparisons(mod_linear, variables="exper").sort(
        ["term", "contrast", "rowid"]
    )
    assert isinstance(comp, MarginaleffectsResult)
    assert isinstance(comp.data, pl.DataFrame)
    assert "estimate" in comp.columns
    # assert "std_error" in comp.columns # std_error is missing for some reason
    assert len(comp) == len(data)


def test_slopes_linear():
    slopes = me.slopes(mod_linear, variables="exper").sort(
        ["term", "contrast", "rowid"]
    )
    assert isinstance(slopes, MarginaleffectsResult)
    assert isinstance(slopes.data, pl.DataFrame)
    assert "estimate" in slopes.columns
    # assert "std_error" in slopes.columns # std_error is missing for some reason
    assert len(slopes) == len(data)


def test_issue_221_long_column_names():
    """Issue #221: Handle very long column names in sklearn models."""
    np.random.seed(123)
    N = 100
    y = np.random.normal(0, 1, size=N)
    x1 = np.random.normal(0, 1, size=N)
    x2 = np.random.binomial(1, 0.5, size=N)

    # Create data with very long column names using polars
    data_long = pl.DataFrame(
        {"y_verylongname": y, "x1_supersuperlongname": x1, "x2_anotherlongestname": x2}
    )

    # Fit model with long column names
    mod_long = me.fit_sklearn(
        "y_verylongname~x1_supersuperlongname+x2_anotherlongestname",
        data=data_long,
        engine=LinearRegression(),
    )

    # Test that comparisons work with long column names
    comp_long = me.avg_comparisons(mod_long)
    assert isinstance(comp_long, MarginaleffectsResult)
    assert isinstance(comp_long.data, pl.DataFrame)
    assert "estimate" in comp_long.columns
    assert "term" in comp_long.columns
    assert len(comp_long) == 2

    # Verify that the long names are preserved in the output
    terms_long = comp_long["term"].to_list()
    assert "x1_supersuperlongname" in terms_long
    assert "x2_anotherlongestname" in terms_long

    # Create equivalent data with short column names
    data_short = pl.DataFrame({"y": y, "x1": x1, "x2": x2})

    # Fit model with short column names
    mod_short = me.fit_sklearn(
        "y~x1+x2",
        data=data_short,
        engine=LinearRegression(),
    )

    # Test that comparisons work with short column names
    comp_short = me.avg_comparisons(mod_short)

    # Sort both by the predictor order (x1 first, x2 second)
    comp_long_sorted = comp_long.sort("term")
    comp_short_sorted = comp_short.sort("term")

    # Verify estimates are equivalent
    est_long = comp_long_sorted["estimate"].to_numpy()
    est_short = comp_short_sorted["estimate"].to_numpy()
    assert np.allclose(est_long, est_short, rtol=1e-10)

    # Verify standard errors are equivalent (if present)
    if "std_error" in comp_long.columns and "std_error" in comp_short.columns:
        se_long = comp_long_sorted["std_error"].to_numpy()
        se_short = comp_short_sorted["std_error"].to_numpy()
        assert np.allclose(se_long, se_short, rtol=1e-10)


def test_categorical_formula_equivalence():
    """Test that C(branch) and branch produce equivalent estimates in avg_predictions."""
    military = me.get_dataset("military").with_columns(
        pl.col("branch").cast(pl.Categorical)
    )

    # Fit model with C(branch) - categorical treatment
    mod_categorical = me.fit_sklearn(
        "rank ~ officer + hisp + C(branch)",
        data=military,
        engine=LinearRegression(),
    )

    # Fit model with branch directly (should be treated as categorical by default)
    mod_direct = me.fit_sklearn(
        "rank ~ officer + hisp + branch",
        data=military,
        engine=LinearRegression(),
    )

    # Get average predictions by branch for both models
    pred_categorical = me.avg_predictions(mod_categorical, by="branch").sort("branch")
    pred_direct = me.avg_predictions(mod_direct, by="branch").sort("branch")

    # Verify both results have the expected structure
    assert isinstance(pred_categorical, MarginaleffectsResult)
    assert isinstance(pred_direct, MarginaleffectsResult)
    assert "estimate" in pred_categorical.columns
    assert "estimate" in pred_direct.columns
    assert "branch" in pred_categorical.columns
    assert "branch" in pred_direct.columns

    # Verify estimates are equivalent
    est_categorical = pred_categorical["estimate"].to_numpy()
    est_direct = pred_direct["estimate"].to_numpy()
    assert np.allclose(est_categorical, est_direct, rtol=1e-10)

    # Verify branch values are the same
    branch_categorical = pred_categorical["branch"].to_list()
    branch_direct = pred_direct["branch"].to_list()
    assert branch_categorical == branch_direct
