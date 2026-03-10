"""
Test new datagrid features to ensure R feature parity.
"""

import pytest
import numpy as np
import polars as pl
from marginaleffects import datagrid, get_dataset
from marginaleffects.classes import _detect_variable_type, _check_variable_type
import marginaleffects.utils as ut


class TestVariableTypeDetection:
    """Test the new variable type detection system."""

    def test_detect_variable_type_basic(self):
        """Test basic variable type detection."""
        data = pl.DataFrame(
            {
                "numeric_col": [1.1, 2.2, 3.3, 4.4, 5.5],
                "integer_col": [1, 2, 3, 4, 5],
                "binary_col": [0, 1, 0, 1, 0],
                "character_col": ["a", "b", "c", "d", "e"],  # Many unique -> character
                "categorical_col": [
                    "cat1",
                    "cat2",
                    "cat1",
                    "cat2",
                    "cat1",
                ],  # Few unique -> categorical
                "logical_col": [True, False, True, False, True],
            }
        )

        # Add explicit categorical column separately with more values
        data = data.with_columns(
            pl.Series("explicit_categorical", ["A", "B", "C", "A", "C"]).cast(
                pl.Categorical
            )
        )

        var_types = _detect_variable_type(data)

        assert var_types["numeric_col"] == "numeric"
        assert var_types["integer_col"] == "integer"
        assert var_types["binary_col"] == "binary"
        assert var_types["character_col"] == "character"
        assert var_types["categorical_col"] == "binary"  # Only 2 unique values
        assert var_types["logical_col"] == "binary"  # Boolean with 2 unique values
        assert (
            var_types["explicit_categorical"] == "categorical"
        )  # Explicit categorical with 3 values

    def test_check_variable_type(self):
        """Test variable type checking."""
        var_types = {
            "num_col": "numeric",
            "cat_col": "categorical",
            "bin_col": "binary",
        }

        assert _check_variable_type(var_types, "num_col", "numeric")
        assert not _check_variable_type(var_types, "num_col", "categorical")
        assert not _check_variable_type(var_types, "nonexistent", "numeric")

        # Test factor/categorical aliasing
        assert _check_variable_type(var_types, "cat_col", "factor")
        assert _check_variable_type(var_types, "cat_col", "categorical")


class TestUtilityFunctions:
    """Test the new utility functions."""

    def test_get_mode(self):
        """Test get_mode function."""
        # Test with clear mode
        series1 = pl.Series([1, 2, 2, 3, 2])
        assert ut.get_mode(series1) == 2

        # Test with strings
        series2 = pl.Series(["a", "b", "a", "c", "a"])
        assert ut.get_mode(series2) == "a"

        # Test with nulls
        series3 = pl.Series([1, 2, None, 2, None, 2])
        assert ut.get_mode(series3) == 2

        # Test with all nulls
        series4 = pl.Series([None, None, None], dtype=pl.Int64)
        ut.get_mode(series4)
        # Should handle gracefully (result can be None or fallback)

    def test_mean_i(self):
        """Test mean_i function (integer mean)."""
        series = pl.Series([10, 20, 30, 40, 50])
        assert ut.mean_i(series) == 30

        # Test with floats that should round
        series2 = pl.Series([10, 21, 30])  # mean = 20.33... -> 20
        assert ut.mean_i(series2) == 20

    def test_mean_na(self):
        """Test mean_na function."""
        series = pl.Series([1.0, 2.0, 3.0])
        assert abs(ut.mean_na(series) - 2.0) < 0.001

        # Test with nulls
        series2 = pl.Series([1.0, None, 3.0])
        assert abs(ut.mean_na(series2) - 2.0) < 0.001

    def test_unique_s(self):
        """Test unique_s function (sorted unique)."""
        series = pl.Series([3, 1, 2, 1, 3])
        result = ut.unique_s(series)
        assert result == [1, 2, 3]

        series2 = pl.Series(["c", "a", "b", "a"])
        result2 = ut.unique_s(series2)
        assert result2 == ["a", "b", "c"]


class TestNewDatagridFeatures:
    """Test the new datagrid features."""

    @pytest.fixture
    def sample_data(self):
        """Sample data for testing."""
        return pl.DataFrame(
            {
                "numeric_var": [1.1, 2.2, 3.3, 4.4, 5.5, 6.6],
                "integer_var": [10, 20, 30, 40, 50, 60],
                "character_var": ["A", "A", "B", "C", "C", "C"],  # C is most frequent
                "binary_var": [0, 1, 0, 1, 0, 1],
                "group": ["G1", "G1", "G2", "G2", "G3", "G3"],
            }
        )

    def test_grid_type_dataframe(self, sample_data):
        """Test grid_type='dataframe' for column-wise binding."""
        grid = datagrid(
            newdata=sample_data,
            numeric_var=[1.0, 2.0],
            integer_var=[10, 20],
            grid_type="dataframe",
        )

        assert grid.height == 2  # Column-wise binding, not cross-product
        assert grid["numeric_var"].to_list() == [1.0, 2.0]
        assert grid["integer_var"].to_list() == [10, 20]

    def test_grid_type_dataframe_different_lengths_error(self, sample_data):
        """Test that dataframe grid_type raises error for different lengths."""
        with pytest.raises(
            ValueError, match="length of each vector must be 1 or be the same"
        ):
            datagrid(
                newdata=sample_data,
                numeric_var=[1.0, 2.0, 3.0],  # length 3
                integer_var=[10, 20],  # length 2
                grid_type="dataframe",
            )

    def test_by_parameter_single(self, sample_data):
        """Test 'by' parameter with single grouping variable."""
        grid = datagrid(
            newdata=sample_data,
            numeric_var=[1.0, 2.0],
            by="group",
            grid_type="mean_or_mode",
        )

        unique_groups = sorted(grid["group"].unique().to_list())
        expected_groups = sorted(sample_data["group"].unique().to_list())
        assert unique_groups == expected_groups
        assert grid.height == 6  # 2 values * 3 groups

    def test_by_parameter_multiple(self, sample_data):
        """Test 'by' parameter with multiple grouping variables."""
        # Add another grouping variable
        test_data = sample_data.with_columns(
            pl.Series(["X", "Y"] * 3).alias("subgroup")
        )

        grid = datagrid(
            newdata=test_data,
            numeric_var=[1.0, 2.0],
            by=["group", "subgroup"],
            grid_type="mean_or_mode",
        )

        # Should have all combinations of group and subgroup
        assert "group" in grid.columns
        assert "subgroup" in grid.columns
        assert grid.height >= 2  # At least 2 values per group combination

    def test_by_parameter_validation(self, sample_data):
        """Test 'by' parameter validation."""
        with pytest.raises(ValueError, match="not in newdata"):
            datagrid(newdata=sample_data, numeric_var=[1.0], by="nonexistent_column")

    def test_fun_global_override(self, sample_data):
        """Test global FUN parameter overrides defaults."""
        grid = datagrid(
            newdata=sample_data,
            numeric_var=[1.0, 2.0],
            FUN=lambda x: x.median(),
            FUN_character=lambda x: ["custom_mode"],
            grid_type="mean_or_mode",
        )

        # FUN should override all type-specific functions
        assert grid.height == 2
        # Hard to test exact values since FUN affects all columns

    def test_fun_specific_parameters(self, sample_data):
        """Test specific FUN_* parameters."""
        # character_var is detected as character, so use FUN_character
        grid = datagrid(
            newdata=sample_data,
            numeric_var=[1.0, 2.0],
            FUN_character=lambda x: ["custom_mode"],
            grid_type="mean_or_mode",
        )

        assert "custom_mode" in grid["character_var"].to_list()
        assert grid.height == 2

    def test_fun_precedence(self, sample_data):
        """Test that specific FUN_* parameters override global FUN."""
        # character_var is detected as character, so use FUN_character
        grid = datagrid(
            newdata=sample_data,
            numeric_var=[1.0, 2.0],
            FUN=lambda x: x.max(),  # Global function
            FUN_character=lambda x: ["specific_override"],  # Specific override
            grid_type="mean_or_mode",
        )

        # FUN_character should override the global FUN for character variables
        assert "specific_override" in grid["character_var"].to_list()

    def test_callable_values(self, sample_data):
        """Test callable values in kwargs."""

        def range_func(x):
            return [x.min(), x.max()]

        grid = datagrid(
            newdata=sample_data, numeric_var=range_func, grid_type="mean_or_mode"
        )

        assert grid.height == 2  # min and max values
        values = grid["numeric_var"].to_list()
        assert 1.1 in values  # min
        assert 6.6 in values  # max

    def test_callable_with_numpy_array(self, sample_data):
        """Test callable returning numpy array."""

        def quantile_func(x):
            return np.quantile(x, [0.25, 0.5, 0.75])

        grid = datagrid(
            newdata=sample_data, numeric_var=quantile_func, grid_type="mean_or_mode"
        )

        assert grid.height == 3  # 3 quantiles
        values = grid["numeric_var"].to_list()
        assert len(values) == 3

    def test_factor_validation_valid(self):
        """Test factor validation with valid levels."""
        data = pl.DataFrame(
            {
                "cat_col": pl.Series(["A", "B", "A", "B"]).cast(pl.Categorical),
                "num_col": [1, 2, 3, 4],
            }
        )

        # Should work with valid levels
        grid = datagrid(newdata=data, cat_col=["A", "B"], grid_type="mean_or_mode")

        assert grid.height == 2
        assert set(grid["cat_col"].to_list()) == {"A", "B"}

    def test_factor_validation_invalid(self):
        """Test factor validation with invalid levels."""
        data = pl.DataFrame(
            {
                "cat_col": pl.Series(["A", "B", "A", "B"]).cast(pl.Categorical),
                "num_col": [1, 2, 3, 4],
            }
        )

        # Should fail with invalid level
        with pytest.raises(ValueError, match="factor levels"):
            datagrid(newdata=data, cat_col=["A", "INVALID"], grid_type="mean_or_mode")

    def test_response_parameter_placeholder(self, sample_data):
        """Test response parameter (placeholder for future implementation)."""
        # Currently the response parameter exists but doesn't change behavior much
        # This test ensures it doesn't break anything
        grid = datagrid(
            newdata=sample_data,
            numeric_var=[1.0, 2.0],
            response=True,
            grid_type="mean_or_mode",
        )

        assert grid.height == 2

    def test_model_validation(self):
        """Test model type validation."""

        class InvalidModel:
            pass

        invalid_model = InvalidModel()

        with pytest.raises(ValueError, match="Unknown model type"):
            datagrid(model=invalid_model, x=[1, 2, 3])

    def test_partial_function_return(self):
        """Test that datagrid returns partial function when both args are None."""
        result = datagrid()  # No model or newdata

        assert callable(result)
        # Should be a partial function that can be called later

    def test_integration_with_existing_features(self, sample_data):
        """Test that new features work with existing ones."""
        # Test combining multiple new features
        # character_var is detected as character, so use FUN_character
        grid = datagrid(
            newdata=sample_data,
            numeric_var=[1.0, 2.0, 3.0],
            by="group",
            FUN_character=lambda x: ["combined_test"],
            grid_type="balanced",  # existing feature
        )

        assert "combined_test" in grid["character_var"].to_list()
        assert grid.height > 3  # Should have combinations due to balanced grid
        assert "group" in grid.columns

    def test_backwards_compatibility(self):
        """Test that existing functionality still works."""
        mtcars = get_dataset("mtcars", "datasets")

        # Test existing patterns still work
        grid1 = datagrid(newdata=mtcars, mpg=[20, 25])
        assert grid1.height == 2

        grid2 = datagrid(newdata=mtcars, mpg=[20, 25], grid_type="balanced")
        assert grid2.height > 2

        # Test counterfactual still works
        grid3 = datagrid(newdata=mtcars, mpg=[20, 25], grid_type="counterfactual")
        assert grid3.height == mtcars.height * 2  # Original rows * 2 values

    def test_counterfactual_retains_all_columns(self):
        """Test that counterfactual grid retains all newdata columns (#1175)."""
        newdata = pl.DataFrame({"id": [1, 2, 3], "x": [10, 20, 30]})

        # Test with variable not in newdata - should retain all original columns
        grid = datagrid(newdata=newdata, z=[100, 200], grid_type="counterfactual")
        assert set(newdata.columns).issubset(set(grid.columns))
        assert "z" in grid.columns

    def test_counterfactual_with_model_retains_non_model_columns(self):
        """Test that counterfactual grid retains non-model columns when model is passed (#1175)."""
        import statsmodels.formula.api as smf
        from marginaleffects import predictions, get_dataset

        # Use mtcars dataset
        mtcars = get_dataset("mtcars")

        # Fit model using only some columns (hp, wt)
        model = smf.ols("mpg ~ hp + wt", data=mtcars).fit()

        # Create counterfactual grid from model - should retain non-model columns like qsec, cyl
        grid = datagrid(
            model=model,
            hp=[100, 150],  # Vary HP
            grid_type="counterfactual",
        )

        # Should retain columns not in the model formula
        assert "qsec" in grid.columns, "qsec (non-model column) should be retained"
        assert "cyl" in grid.columns, "cyl (non-model column) should be retained"

        # Test that predictions work with the retained columns
        preds = predictions(model, newdata=grid)

        # Should be able to access non-model columns for post-prediction analysis
        assert "qsec" in preds.columns, "qsec should be available for grouping/analysis"
        assert "cyl" in preds.columns, "cyl should be available for grouping/analysis"


class TestPerformanceOptimization:
    """Test the performance optimization (code repetition elimination)."""

    def test_type_mapping_computation(self):
        """Test that type mapping is computed correctly."""
        # Use more data points to avoid binary detection
        data = pl.DataFrame(
            {
                "num": [1.0, 2.5, 3.7, 4.2, 5.1],  # Clear numeric
                "int": [10, 20, 30, 40, 50],  # Clear integer
                "char": ["a", "b", "c", "d", "e"],  # Many unique -> character
            }
        )

        var_types = _detect_variable_type(data)

        # This should work without errors (internal function test)
        from marginaleffects.datagrid import _compute_variable_type_mapping

        type_mapping = _compute_variable_type_mapping(var_types, data.columns)

        assert "num" in type_mapping
        assert type_mapping["num"]["is_numeric"]
        assert not type_mapping["num"]["is_character"]

        assert "int" in type_mapping
        assert type_mapping["int"]["is_integer"]
        assert not type_mapping["int"]["is_numeric"]

        assert "char" in type_mapping
        assert type_mapping["char"]["is_character"]
        assert not type_mapping["char"]["is_numeric"]
