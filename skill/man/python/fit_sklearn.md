# `fit_sklearn()` {.unnumbered}

Fit a sklearn model with output that is compatible with pymarginaleffects.

This function streamlines the process of fitting sklearn models by:

1. Parsing the formula
2. Handling missing values
3. Creating model matrices
4. Fitting the model with specified options

## Parameters {.unnumbered}

`formula`: (str)
    Model formula

* Example: "outcome ~ distance + incentive"

`data`: (pandas.DataFrame or polars.DataFrame) Dataframe with the response variable and predictors.

**Important:** All categorical variables must be explicitly converted to `Categorical` or `Enum` dtype before fitting. String columns are not accepted in model formulas.

For Polars DataFrames:
```python
import polars as pl

# Option 1: Cast to Categorical (simplest) {.unnumbered}
df = df.with_columns(pl.col("region").cast(pl.Categorical))

# Option 2: Cast to Enum with explicit category order (recommended for control) {.unnumbered}
categories = ["<18", "18 to 35", ">35"]
df = df.with_columns(pl.col("age_group").cast(pl.Enum(categories)))
```

For pandas DataFrames:
```python
df["region"] = df["region"].astype("category")
```

`engine`: (callable) sklearn model class (e.g., LinearRegression, LogisticRegression)

`kwargs_engine`: (dict, default={}) Additional arguments passed to the model initialization.

* Example: `{'weights': weights_array}`


## Returns {.unnumbered}

(ModelSklearn)
    A fitted model wrapped in the ModelSklearn class for compatibility
    with marginaleffects.

## Examples {.unnumbered}

```{python}
from marginaleffects import *
from statsmodels.formula.api import ols
import polars as pl
import polars.selectors as cs
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import OneHotEncoder, FunctionTransformer
from sklearn.linear_model import LinearRegression
from sklearn.compose import make_column_transformer
from xgboost import XGBRegressor


# Linear regression: Scikit-learn {.unnumbered}
military = get_dataset("military")

# Convert categorical variables to proper dtypes {.unnumbered}
military = military.with_columns(
    pl.col("branch").cast(pl.Categorical)
)

mod_sk = fit_sklearn(
    "rank ~ officer + hisp + branch",
    data=military,
    engine=LinearRegression(),
)
avg_predictions(mod_sk, by="branch")

# Linear regression: Statsmodels {.unnumbered}
mod_sm = ols("rank ~ officer + hisp + branch", data=military.to_pandas()).fit()
avg_predictions(mod_sm, by="branch")

# XGBoost: Scikit-learn {.unnumbered}
airbnb = get_dataset("airbnb")

# Convert categorical variables to proper dtypes {.unnumbered}
catvar = airbnb.select(~cs.numeric()).columns
airbnb = airbnb.with_columns(
    [pl.col(c).cast(pl.Categorical) for c in catvar]
)

train, test = train_test_split(airbnb)

def selector(data):
    y = data.select(cs.by_name("price", require_all=False))
    X = data.select(~cs.by_name("price", require_all=False))
    return y, X


preprocessor = make_column_transformer(
    (OneHotEncoder(), catvar),
    remainder=FunctionTransformer(lambda x: x.to_numpy()),
)
pipeline = make_pipeline(preprocessor, XGBRegressor())

mod = fit_sklearn(selector, data=train, engine=pipeline)

avg_predictions(mod, newdata=test, by="unit_type")

avg_comparisons(mod, variables={"bedrooms": 2}, newdata=test)
```

## Notes {.unnumbered}

The fitted model includes additional attributes:

- `data`: The processed data after listwise deletion
- `formula`: The original formula string
- `formula_engine`: Set to "sklearn"
- `model`: The fitted sklearn model object
