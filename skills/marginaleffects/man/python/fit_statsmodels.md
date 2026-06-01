# `fit_statsmodels()` {.unnumbered}

Fit a statsmodels model with output that is compatible with pymarginaleffects.

This function streamlines the process of fitting statsmodels models by:
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

`engine`: (callable) statsmodels model class (e.g., OLS, Logit)

`kwargs_engine`: (dict, default={}) Additional arguments passed to the model initialization.

* Example: `{'weights': weights_array}`
  
`kwargs_fit`: (dict, default={}) Additional arguments passed to the model's fit method.

* Example: `{'cov_type': 'HC3'}`


## Returns {.unnumbered}

(ModelStatsmodels)
    A fitted model wrapped in the ModelStatsmodels class for compatibility
    with marginaleffects.

## Examples {.unnumbered}

```python
from marginaleffects import fit_statsmodels, get_dataset, predictions, slopes, comparisons

import statsmodels.api as sm

data = get_dataset("thornton")

# Model with robust standard errors {.unnumbered}
model_robust = fit_statsmodels(
    formula="outcome ~ distance + incentive",
    data=data,
    engine=sm.OLS,
    kwargs_fit={"cov_type": "HC3"}
)

predictions(model_robust)
slopes(model_robust)
comparisons(model_robust)
```

## Notes {.unnumbered}

The fitted model includes additional attributes:

- `data`: The processed data after listwise deletion
- `formula`: The original formula string
- `formula_engine`: Set to "statsmodels"
- `model`: The fitted statsmodels model object
