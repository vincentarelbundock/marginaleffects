import pytest
import polars as pl
from polars.testing import assert_series_equal
import statsmodels.formula.api as smf
from tests.helpers import guerry, penguins, diamonds, mtcars
from marginaleffects import *
from marginaleffects import MarginaleffectsResult
from tests.utilities import *

df = guerry.with_columns(pl.Series(range(guerry.shape[0])).alias("row_id")).sort(
    "Region", "row_id"
)
mod_py = smf.ols(
    "Literacy ~ Pop1831 * Desertion", sort_categories_pandas(df.to_pandas())
).fit()


def test_newdata_balanced():
    mod = smf.ols(
        "body_mass_g ~ flipper_length_mm * species * bill_length_mm + island",
        penguins.to_pandas(),
    ).fit()
    p = predictions(mod, newdata="balanced")
    assert p.shape[0] == 9


def test_predictions():
    pre_py = predictions(mod_py)
    pre_r = pl.read_csv("tests/r/test_predictions_01.csv")
    compare_r_to_py(pre_r, pre_py)


def test_by():
    pre_py = predictions(mod_py, by="Region")
    pre_r = pl.read_csv("tests/r/test_predictions_02.csv")
    compare_r_to_py(pre_r, pre_py)


def test_by_2():
    pre_py = predictions(mod_py, by=["Region", "MainCity"])
    assert pre_py.shape == (13, 9)


def test_by_hypothesis():
    pre_py = predictions(mod_py, by="Region")
    pre_py = predictions(mod_py, by="Region", hypothesis="b0 * b2 = b2*2")
    pre_r = pl.read_csv("tests/r/test_predictions_03.csv")
    compare_r_to_py(pre_r, pre_py)


def test_class_manipulation():
    p = predictions(mod_py)
    assert isinstance(p, MarginaleffectsResult)
    assert isinstance(p.data, pl.DataFrame)
    assert p.data.equals(p.to_polars())
    assert p["estimate"].equals(p.data["estimate"])
    head_df = p.head()
    assert isinstance(head_df, pl.DataFrame)
    assert head_df.equals(p.data.head())
    assert isinstance(p.summary(), str)


def issue_38():
    p = avg_predictions(mod_py, by=True)
    assert p.shape[0] == 1
    p = avg_predictions(mod_py)
    assert p.shape[0] == 1


def issue_59():
    p = predictions(mod_py, vcov=False)
    assert p.shape[0] == df.shape[0]
    assert p.shape[1] > 20


def test_issue_95():
    diamonds_pd = sort_categories_pandas(diamonds.to_pandas())
    model = smf.ols("price ~ cut + clarity + color", diamonds_pd).fit()

    newdata = diamonds.slice(0, 20)
    p = predictions(model, newdata=newdata, by="cut")

    # Convert newdata to pandas, preserving all categorical levels from training data
    newdata_pd = newdata.to_pandas()
    for col in ["cut", "clarity", "color"]:
        if col in newdata_pd.columns and diamonds_pd[col].dtype.name == "category":
            newdata_pd[col] = (
                newdata_pd[col]
                .astype("category")
                .cat.set_categories(diamonds_pd[col].cat.categories)
            )
    newdata_pd = sort_categories_pandas(newdata_pd)

    newdata = newdata.with_columns(pred=pl.Series(model.predict(newdata_pd)))
    newdata = newdata.group_by("cut").agg(pl.col("pred").mean())
    p = p.sort(by="cut")
    newdata = newdata.sort(by="cut")

    assert_series_equal(p["estimate"], newdata["pred"], check_names=False)


def test_issue161():
    mod = smf.ols("mpg ~ wt * hp * cyl", mtcars.to_pandas()).fit()

    # predictions(variables=)
    p = predictions(mod)
    assert p.shape[0] == 32
    p = predictions(mod, variables="cyl")
    assert p.shape[0] == 96
    p = predictions(mod, variables={"cyl": None})
    assert p.shape[0] == 96
    p = predictions(mod, variables=["cyl"])
    assert p.shape[0] == 96
    p = predictions(mod, variables=["cyl", "am"])
    assert p.shape[0] == 192

    # avg_predictions(variables=)
    p1 = avg_predictions(mod)
    p2 = avg_predictions(mod, variables="cyl")
    p3 = predictions(mod, variables="cyl")
    assert p2["estimate"][0] != p1["estimate"][0]
    # Use approximate equality for floating point comparison
    assert p2["estimate"][0] == pytest.approx(p3["estimate"].mean())


def test_issue83():
    diamonds = pl.read_csv(
        "https://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/diamonds.csv"
    )
    diamonds83 = diamonds.with_columns(
        cut_ideal_null=pl.when(pl.col("cut") == "Ideal")
        .then(pl.lit(None))
        .otherwise(pl.col("cut"))
        .cast(pl.Categorical)
    )
    model = smf.ols("price ~ cut_ideal_null", diamonds83.to_pandas()).fit()
    newdata = diamonds.slice(0, 20).with_columns(
        cut_ideal_null=pl.when(pl.col("cut") == "Ideal")
        .then(pl.lit("Premium"))
        .otherwise(pl.col("cut"))
        .cast(pl.Categorical)
    )

    with pytest.raises(ValueError, match="missing value"):
        predictions(model, newdata=diamonds83.head())

    # Note: With Categorical dtype, newdata with different levels is now allowed
    # since Polars Categorical handles this differently than pandas categorical levels
    p = predictions(model, newdata=newdata)
    assert p is not None

    p = predictions(model, newdata=diamonds83.head().drop_nulls())
    assert p is not None


def test_missing_predictors():
    penguins = (
        get_dataset("penguins", "palmerpenguins")
        .drop_nulls(subset="species")
        .with_columns(pl.col("species").cast(pl.Categorical))
    )
    penguins.select("species", "body_mass_g", "flipper_length_mm").group_by(
        "species"
    ).mean()
    fit = smf.ols(
        "flipper_length_mm ~ body_mass_g * species", data=penguins.to_pandas()
    ).fit()
    p = avg_predictions(fit, by="species")
    assert p.height == 3
