import polars as pl
import pytest
import statsmodels.formula.api as smf
from linearmodels.datasets import wage_panel


def sort_categories_pandas(df_pd):
    """Sort pandas categorical categories alphabetically for consistent model fitting."""
    for col in df_pd.columns:
        if df_pd[col].dtype.name == "category":
            df_pd[col] = df_pd[col].cat.reorder_categories(
                sorted(df_pd[col].cat.categories)
            )
    return df_pd


diamonds = pl.read_csv("tests/data/diamonds.csv").with_columns(
    pl.col("cut").cast(pl.Categorical),
    pl.col("color").cast(pl.Categorical),
    pl.col("clarity").cast(pl.Categorical),
)

dietox = pl.read_csv("tests/data/dietox.csv").with_columns(
    pl.col("Cu").cast(pl.String).cast(pl.Categorical),
    pl.col("Evit").cast(pl.Categorical),
)

guerry = (
    pl.read_csv(
        "tests/data/Guerry.csv",
        null_values="NA",
    )
    .drop_nulls()
    .with_columns(
        pl.col("Region").cast(pl.Categorical),
        pl.col("dept").cast(pl.String).cast(pl.Categorical),
        pl.col("Department").cast(pl.Categorical),
        pl.col("MainCity").cast(pl.Categorical),
    )
)

guerry_with_nulls = pl.read_csv("tests/data/Guerry.csv")

impartiality_df = pl.read_csv("tests/data/impartiality.csv").with_columns(
    pl.col("impartial").cast(pl.Int8),
    pl.col("democracy").cast(pl.Categorical),
    pl.col("country").cast(pl.Categorical),
    pl.col("continent").cast(pl.Categorical),
)

iris = pl.read_csv("tests/data/iris.csv").with_columns(
    pl.col("Species").cast(pl.Categorical)
)

mtcars = pl.read_csv("tests/data/mtcars.csv")

penguins = (
    pl.read_csv(
        "tests/data/penguins.csv",
        null_values="NA",
    )
    .drop_nulls()
    .with_columns(
        pl.col("species").cast(pl.Categorical),
        pl.col("island").cast(pl.Categorical),
        pl.col("sex").cast(pl.Categorical),
    )
)

quine = pl.read_csv("tests/data/quine.csv").with_columns(
    pl.col("Eth").cast(pl.Categorical),
    pl.col("Sex").cast(pl.Categorical),
    pl.col("Age").cast(pl.Categorical),
    pl.col("Lrn").cast(pl.Categorical),
)

wage_panel_pd = wage_panel.load().set_index(["nr", "year"])


@pytest.fixture(scope="session")
def guerry_mod():
    return smf.ols(
        "Literacy ~ Pop1831 * Desertion", sort_categories_pandas(guerry.to_pandas())
    ).fit()


@pytest.fixture(scope="session")
def impartiality_model():
    return smf.logit(
        "impartial ~ equal * democracy + continent",
        data=sort_categories_pandas(impartiality_df.to_pandas()),
    ).fit()


@pytest.fixture(scope="session")
def penguins_model():
    mod = smf.ols(
        "body_mass_g ~ flipper_length_mm * species * bill_length_mm + island",
        data=sort_categories_pandas(penguins.to_pandas()),
    ).fit()
    return mod


@pytest.fixture(scope="session")
def penguins_mod_add():
    mod = smf.ols(
        "body_mass_g ~ flipper_length_mm * species * bill_length_mm * island",
        sort_categories_pandas(penguins.to_pandas()),
    ).fit()
    return mod


@pytest.fixture(scope="session")
def penguins_mod_5var():
    mod = smf.ols(
        "body_mass_g ~ flipper_length_mm * species * bill_length_mm * island * bill_depth_mm",
        sort_categories_pandas(penguins.to_pandas()),
    ).fit()
    return mod


@pytest.fixture(scope="session")
def mtcars_mod():
    mod = smf.ols("mpg ~ hp * wt * disp * cyl * qsec", data=mtcars).fit()
    return mod
