import numpy as np
import polars as pl

from marginaleffects import *
from statsmodels.formula.api import logit, ols

mtcars = get_dataset("mtcars", "datasets")


def test_FUN_numeric():
    d = datagrid(newdata=mtcars, FUN_numeric=lambda x: x.median())
    assert d["cyl"][0] == mtcars["cyl"].median()
    assert d["hp"][0] == mtcars["hp"].median()
    assert d["carb"][0] == mtcars["carb"].median()


def test_none_values():
    """Test that None creates null columns (similar to R's NA)"""
    # Test with None and other explicit values
    nd = datagrid(newdata=mtcars, mpg=None, hp=[1, 2, 3, 4])
    assert nd.shape[0] == 4
    assert nd["mpg"].is_null().all()
    assert nd["hp"].to_list() == [1, 2, 3, 4]

    # Test with None alone
    nd2 = datagrid(newdata=mtcars, mpg=None)
    assert nd2.shape[0] == 1
    assert nd2["mpg"].is_null().all()

    # Test with multiple None values
    nd3 = datagrid(newdata=mtcars, mpg=None, hp=None)
    assert nd3.shape[0] == 1
    assert nd3["mpg"].is_null().all()
    assert nd3["hp"].is_null().all()

    # Test with counterfactual grid
    nd4 = datagrid(newdata=mtcars, mpg=None, grid_type="counterfactual")
    assert nd4.shape[0] == 32  # Same as number of rows in mtcars
    assert nd4["mpg"].is_null().all()


def test_simple_grid():
    d = datagrid(mpg=24, newdata=mtcars)
    assert d.shape == (1, 12)
    d = datagrid(mpg=[23, 24], hp=[120, 130], newdata=mtcars)
    assert d.shape == (4, 12)


def test_cf():
    assert datagrid(newdata=mtcars, mpg=32).shape[0] == 1
    assert (
        datagrid(newdata=mtcars, mpg=[30, 32], grid_type="counterfactual").shape[0]
        == 64
    )
    assert (
        datagrid(
            newdata=mtcars, mpg=32, am=0, hp=100, grid_type="counterfactual"
        ).shape[0]
        == 32
    )
    assert (
        datagrid(
            newdata=mtcars, am=[0, 1], hp=[100, 110, 120], grid_type="counterfactual"
        ).shape[0]
        == 192
    )
    assert (
        datagrid(newdata=mtcars, mpg=[30, 32], grid_type="counterfactual")
        .unique("rowidcf")
        .shape[0]
        == 32
    )
    assert set(
        datagrid(newdata=mtcars, mpg=[30, 32], grid_type="counterfactual").columns
    ) == {
        "gear",
        "qsec",
        "mpg",
        "cyl",
        "am",
        "wt",
        "vs",
        "drat",
        "rowidcf",
        "disp",
        "rownames",
        "hp",
        "carb",
    }


def test_issue156():
    rng = np.random.default_rng(seed=48103)
    N = 10
    dat = pl.DataFrame(
        {
            "Num": rng.normal(loc=0, scale=1, size=N),
            "Bin": rng.binomial(n=1, p=0.5, size=N),
            "Cat": rng.choice(["A", "B", "C"], size=N),
        }
    )
    d = datagrid(grid_type="balanced", newdata=dat)
    assert d.shape[0] == 6
    assert set(d["Bin"].unique()) == {0, 1}
    assert set(d["Cat"].unique()) == {"A", "B", "C"}
    assert len(d["Num"].unique()) == 1


def test_mean_or_mode():
    d1 = datagrid(newdata=mtcars)
    d2 = datagrid(newdata=mtcars, grid_type="mean_or_mode")
    for col in d1.columns[1:]:
        assert (d1[col] == d2[col]).all()


def test_issue_169():
    dat = get_dataset("thornton")
    dat = dat.drop_nulls(subset=["incentive"])
    mod = logit("outcome ~ incentive + agecat + distance", data=dat.to_pandas()).fit()
    p = predictions(mod, newdata="balanced")
    assert p.shape[0] == 6


def test_callable():
    mod = ols("hp ~ mpg * cyl", data=mtcars.to_pandas()).fit()

    def fivenum(x):
        return np.quantile(x, [0, 0.25, 0.5, 0.75, 1])

    p = predictions(mod, newdata=datagrid(mpg=fivenum))
    assert p.height == 5


def test_datagrid_counterfactual_lambda():
    mtcars = get_dataset("mtcars", "datasets")
    grid = datagrid(
        am=lambda x: x.unique(),
        cyl=np.unique,
        newdata=mtcars,
        grid_type="counterfactual",
    )
    assert grid.height == 32 * 2 * 3


def test_datagrid_balanced_outputs_sorted_unique_values():
    df = pl.DataFrame(
        {
            "letter": ["c", "a", "b", "c"],
            "flag": [1, 0, 1, 0],
        }
    )
    grid = datagrid(newdata=df, grid_type="balanced")
    pairs = list(zip(grid["letter"], grid["flag"]))
    assert pairs == sorted(pairs)


def test_datagrid_balanced_multiple_columns_sorted_cartesian():
    df = pl.DataFrame(
        {
            "group": ["z", "y", "x"],
            "level": ["b", "a", "c"],
            "indicator": [1, 0, 1],
        }
    )
    grid = datagrid(newdata=df, grid_type="balanced")
    rows = list(zip(grid["group"], grid["level"], grid["indicator"]))
    assert rows == sorted(rows)
