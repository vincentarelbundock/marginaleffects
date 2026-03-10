"""
Tests for categorical-only enforcement.
Ensures String columns are rejected with clear error messages.
"""

import polars as pl
import pytest
import statsmodels.formula.api as smf
from marginaleffects import predictions


def test_polars_string_column_rejected():
    """Polars DataFrame with String column should raise TypeError"""
    df = pl.DataFrame(
        {
            "y": [1, 2, 3, 4, 5],
            "x": [1.0, 2.0, 3.0, 4.0, 5.0],
            "cat": ["a", "b", "a", "b", "a"],  # String type by default
        }
    )

    # Should raise TypeError with helpful message
    with pytest.raises(TypeError, match="Column 'cat' has String type"):
        mod = smf.ols("y ~ x + cat", data=df.to_pandas()).fit()
        predictions(mod)


def test_polars_categorical_accepted():
    """Polars DataFrame with Categorical should work"""
    df = pl.DataFrame(
        {
            "y": [1, 2, 3, 4, 5],
            "x": [1.0, 2.0, 3.0, 4.0, 5.0],
            "cat": ["a", "b", "a", "b", "a"],
        }
    ).with_columns(pl.col("cat").cast(pl.Categorical))

    mod = smf.ols("y ~ x + cat", data=df.to_pandas()).fit()
    pred = predictions(mod)
    assert pred.shape[0] > 0


def test_polars_enum_accepted():
    """Polars DataFrame with Enum should work"""
    df = pl.DataFrame(
        {
            "y": [1, 2, 3, 4, 5],
            "x": [1.0, 2.0, 3.0, 4.0, 5.0],
            "cat": ["a", "b", "a", "b", "a"],
        }
    )
    categories = df["cat"].unique().sort()
    df = df.with_columns(pl.col("cat").cast(pl.Enum(categories)))

    mod = smf.ols("y ~ x + cat", data=df.to_pandas()).fit()
    pred = predictions(mod)
    assert pred.shape[0] > 0


def test_formula_c_with_string_rejected():
    """C() in formula with String variable should raise TypeError"""
    df = pl.DataFrame(
        {
            "y": [1, 2, 3, 4, 5],
            "x": [1.0, 2.0, 3.0, 4.0, 5.0],
            "cat": ["a", "b", "a", "b", "a"],  # String type
        }
    )

    with pytest.raises(TypeError, match="Column 'cat' has String type"):
        mod = smf.ols("y ~ x + C(cat)", data=df.to_pandas()).fit()
        predictions(mod)


def test_formula_c_with_categorical_accepted():
    """C() in formula with Categorical variable should work"""
    df = pl.DataFrame(
        {
            "y": [1, 2, 3, 4, 5],
            "x": [1.0, 2.0, 3.0, 4.0, 5.0],
            "cat": ["a", "b", "a", "b", "a"],
        }
    ).with_columns(pl.col("cat").cast(pl.Categorical))

    mod = smf.ols("y ~ x + C(cat)", data=df.to_pandas()).fit()
    pred = predictions(mod)
    assert pred.shape[0] > 0


def test_pandas_object_dtype_rejected():
    """pandas DataFrame with object dtype string column should raise TypeError"""
    try:
        import pandas as pd
    except ImportError:
        pytest.skip("pandas not installed")

    df = pd.DataFrame(
        {
            "y": [1, 2, 3, 4, 5],
            "x": [1.0, 2.0, 3.0, 4.0, 5.0],
            "cat": ["a", "b", "a", "b", "a"],  # object dtype
        }
    )

    with pytest.raises(TypeError, match="Column 'cat' has String type"):
        mod = smf.ols("y ~ x + cat", data=df).fit()
        predictions(mod)


def test_pandas_categorical_accepted():
    """pandas DataFrame with Categorical dtype should work"""
    try:
        import pandas as pd
    except ImportError:
        pytest.skip("pandas not installed")

    df = pd.DataFrame(
        {
            "y": [1, 2, 3, 4, 5],
            "x": [1.0, 2.0, 3.0, 4.0, 5.0],
            "cat": pd.Categorical(["a", "b", "a", "b", "a"]),
        }
    )

    mod = smf.ols("y ~ x + cat", data=df).fit()
    pred = predictions(mod)
    assert pred.shape[0] > 0


def test_error_message_includes_examples():
    """Error message should include code examples"""
    df = pl.DataFrame(
        {
            "y": [1, 2, 3, 4, 5],
            "x": [1.0, 2.0, 3.0, 4.0, 5.0],
            "cat": ["a", "b", "a", "b", "a"],
        }
    )

    with pytest.raises(TypeError) as exc_info:
        mod = smf.ols("y ~ x + cat", data=df.to_pandas()).fit()
        predictions(mod)

    error_msg = str(exc_info.value)
    # Check that error includes helpful examples
    assert "cast(pl.Categorical)" in error_msg
    assert "For Polars DataFrames:" in error_msg
    assert "For pandas DataFrames:" in error_msg
