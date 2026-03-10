import pytest
import pandas as pd
import polars as pl
import duckdb
from marginaleffects.utils import ingest, get_dataset


def get_sample_data():
    return pd.DataFrame(
        {
            "id": [1, 2, 3],
            "name": ["Alice", "Bob", "Charlie"],
            "age": [25, 30, 35],
            "score": [85.5, 90.0, 95.5],
        }
    )


sample_data = get_sample_data()


@pytest.fixture
def sample_pandas_df():
    return get_sample_data()


@pytest.fixture
def sample_polars_df():
    pd_df = get_sample_data()
    return pl.from_pandas(pd_df)


@pytest.fixture
def sample_arrow_df():
    with duckdb.connect() as con:
        out = con.execute("SELECT * FROM sample_data").arrow()
    return out


def test_ingest_pandas(sample_pandas_df):
    result = ingest(sample_pandas_df)
    if "index" in result.columns:
        result = result.drop("index")
    assert isinstance(result, pl.DataFrame), "Result should be a Polars DataFrame"
    # Verify contents
    expected = pl.from_pandas(sample_pandas_df)
    assert result.equals(expected), (
        "Ingested DataFrame does not match expected Polars DataFrame"
    )


def test_ingest_polars(sample_polars_df):
    result = ingest(sample_polars_df)
    assert isinstance(result, pl.DataFrame), "Result should be a Polars DataFrame"
    # Verify contents
    expected = sample_polars_df
    assert result.equals(expected), (
        "Ingested DataFrame does not match expected Polars DataFrame"
    )


def test_ingest_arrow(sample_arrow_df):
    result = ingest(sample_arrow_df).to_pandas()
    expected = get_sample_data()
    assert result.equals(expected), (
        "Ingested DataFrame does not match expected Polars DataFrame"
    )


@pytest.mark.parametrize(
    "input_args, expected_output",
    [
        ({"search": "(?i)titanic"}, (8, 6)),
        ({}, (2884, 8)),
    ],
)
def test_get_dataset_polars(input_args, expected_output):
    output = get_dataset(**input_args)
    assert isinstance(output, pl.DataFrame), "Result should be a Polars DataFrame"
    assert output.shape == expected_output, "Expected shape (2884, 8)"


@pytest.mark.parametrize(
    "input_args, expected_output",
    [({"docs": "True", "dataset": "ArgentinaCPI", "package": "AER"}, None)],
)
def test_get_dataset_doc(input_args, expected_output):
    output = get_dataset(**input_args)
    assert isinstance(output, str), "Result should be a str"
    assert output.endswith("html"), "Expected output to be a URL"
