# `fit_linearmodels()` {.unnumbered}

Fit a linearmodels model with output that is compatible with pymarginaleffects.

This function streamlines the process of fitting linearmodels panel models by:

1. Parsing panel effects from the formula
2. Handling missing values
3. Creating model matrices
4. Fitting the model with specified options

## Parameters {.unnumbered}

`formula`: (str) Model formula with optional panel effects terms. 

- Supported effects are:
    - EntityEffects: Entity-specific fixed effects
    - TimeEffects: Time-specific fixed effects
    - FixedEffects: Alias for EntityEffects
- Example: `"y ~ x1 + x2 + EntityEffects"`

`data` : (pandas.DataFrame or polars.DataFrame) Panel data with MultiIndex (entity, time) or regular DataFrame with entity and time columns.

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

`engine`: (callable) linearmodels model class (e.g., PanelOLS, BetweenOLS, FirstDifferenceOLS)

`kwargs_engine`: (dict, default={}) Additional arguments passed to the model initialization.

* Example: `{'weights': weights_array}`

`kwargs_fit`: (dict, default={}) Additional arguments passed to the model's fit method.

* Example: `{'cov_type': 'robust'}`

## Returns {.unnumbered}

(ModelLinearmodels)
    A fitted model wrapped in the ModelLinearmodels class for compatibility
    with marginaleffects.

## Examples {.unnumbered}

```python
from linearmodels.panel import PanelOLS
from linearmodels.panel import generate_panel_data
from marginaleffects import *
data = generate_panel_data()
model_robust = fit_linearmodels(
    formula="y ~ x1 + EntityEffects",
    data=data.data,
    engine=PanelOLS,
    kwargs_fit={'cov_type': 'robust'}
)

predictions(model_robust)
```

## Notes {.unnumbered}

The fitted model includes additional attributes:

- `data`: The processed data after listwise deletion
- `formula`: The original formula string
- `formula_engine`: Set to "linearmodels"
- `model`: The fitted linearmodels model object
