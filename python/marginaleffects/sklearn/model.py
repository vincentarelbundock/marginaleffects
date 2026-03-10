import numpy as np
import warnings
import polars as pl
from ..docs import DocsModels
from ..utils import ingest
from ..formulaic_utils import listwise_deletion, model_matrices
from ..model_abstract import ModelAbstract


class ModelSklearn(ModelAbstract):
    def __init__(self, model, vault={}):
        super().__init__(model, vault)

    def get_predict(self, params, newdata):
        engine = self.get_engine_running()
        formula = self.get_formula()

        if isinstance(newdata, np.ndarray):
            exog = newdata
        elif callable(formula):
            _, exog = formula(newdata)
        elif (
            hasattr(newdata, "shape")
            and hasattr(newdata, "__array__")
            and not isinstance(newdata, pl.DataFrame)
        ):
            # newdata is already a matrix (e.g., ModelMatrix from formulaic)
            exog = newdata
        else:
            # Handle case where original data had an index that got converted to columns
            if isinstance(newdata, pl.DataFrame):
                pandas_newdata = newdata.to_pandas()

                # Check if we need to restore index structure for formulaic
                # This happens when polars converted an indexed pandas DataFrame
                original_columns = self.vault.get("original_columns")
                if original_columns is not None:
                    # Determine which columns were originally indices
                    modeldata_cols = set(newdata.columns)
                    original_cols = set(original_columns)
                    index_cols = list(modeldata_cols - original_cols)

                    if index_cols:
                        # Restore the original index structure
                        pandas_indexed = pandas_newdata.set_index(index_cols)
                        _, exog = model_matrices(
                            formula,
                            data=pandas_indexed,
                            formula_engine=self.get_formula_engine(),
                        )
                    else:
                        _, exog = model_matrices(
                            formula,
                            data=pandas_newdata,
                            formula_engine=self.get_formula_engine(),
                        )
                else:
                    # Fallback: no original structure info available
                    _, exog = model_matrices(
                        formula,
                        data=pandas_newdata,
                        formula_engine=self.get_formula_engine(),
                    )
            else:
                _, exog = model_matrices(
                    formula, data=newdata, formula_engine=self.get_formula_engine()
                )

        try:
            with warnings.catch_warnings():
                warnings.filterwarnings("ignore", message=".*valid feature names.*")
                p = engine.predict_proba(exog)
                # only keep the second column for binary classification since it is redundant info
                if p.shape[1] == 2:
                    p = p[:, 1]
        except (AttributeError, NotImplementedError):
            with warnings.catch_warnings():
                warnings.filterwarnings("ignore", message=".*valid feature names.*")
                p = engine.predict(exog)

        if p.ndim == 1:
            p = pl.DataFrame({"rowid": range(newdata.shape[0]), "estimate": p})
        elif p.ndim == 2 and p.shape[1] == 1:
            p = pl.DataFrame(
                {"rowid": range(newdata.shape[0]), "estimate": np.ravel(p)}
            )
        elif p.ndim == 2:
            colnames = {f"column_{i}": v for i, v in enumerate(engine.classes_)}
            p = (
                pl.DataFrame(p)
                .rename(colnames)
                .with_columns(
                    pl.Series(range(p.shape[0]), dtype=pl.Int32).alias("rowid")
                )
                .melt(id_vars="rowid", variable_name="group", value_name="estimate")
            )
        else:
            raise ValueError(
                "The `predict()` method must return an array with 1 or 2 dimensions."
            )

        p = p.with_columns(pl.col("rowid").cast(pl.Int32))

        return p


# @validate_types
def fit_sklearn(formula, data: pl.DataFrame, engine) -> ModelSklearn:
    """
    Fit a sklearn model with output that is compatible with pymarginaleffects.

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(fit_sklearn)`
    """

    d = listwise_deletion(formula, data=data)

    # Store original data structure info for later restoration
    original_columns = None
    if hasattr(data, "columns"):
        original_columns = list(data.columns)

    if isinstance(formula, str):
        d = listwise_deletion(formula, data=data)
        y, X = model_matrices(formula, d)
        # formulaic returns a matrix when the response is character or categorical
        if y.ndim == 2:
            response_var = formula.split("~")[0].strip()
            y = d[response_var]
        y = np.ravel(y)

        # Store the model_spec to preserve categorical variable ordering
        # This is crucial for sklearn models where feature names must match exactly
        model_spec = getattr(X, "model_spec", None)

    elif callable(formula):
        y, X = formula(d)
        model_spec = None

    else:
        raise ValueError("The formula must be a string or a callable function.")

    engine_running = engine.fit(X=X, y=y)

    vault = {
        "formula": formula,
        "modeldata": ingest(d),
        "package": "sklearn",
        "engine_running": engine_running,
        "original_columns": original_columns,  # Store for index restoration
        "model_spec": model_spec,  # Store for categorical ordering
    }
    return ModelSklearn(engine_running, vault)


docs_sklearn = (
    """
# `fit_sklearn()`

Fit a sklearn model with output that is compatible with pymarginaleffects.

This function streamlines the process of fitting sklearn models by:

1. Parsing the formula
2. Handling missing values
3. Creating model matrices
4. Fitting the model with specified options

## Parameters
"""
    + DocsModels.docstring_formula
    + """
`data`: (pandas.DataFrame or polars.DataFrame) Dataframe with the response variable and predictors.

**Important:** All categorical variables must be explicitly converted to `Categorical` or `Enum` dtype before fitting. String columns are not accepted in model formulas.

For Polars DataFrames:
```python
import polars as pl

# Option 1: Cast to Categorical (simplest)
df = df.with_columns(pl.col("region").cast(pl.Categorical))

# Option 2: Cast to Enum with explicit category order (recommended for control)
categories = ["<18", "18 to 35", ">35"]
df = df.with_columns(pl.col("age_group").cast(pl.Enum(categories)))
```

For pandas DataFrames:
```python
df["region"] = df["region"].astype("category")
```

`engine`: (callable) sklearn model class (e.g., LinearRegression, LogisticRegression)
"""
    + DocsModels.docstring_kwargs_engine
    + """
"""
    + DocsModels.docstring_fit_returns("Sklearn")
    + """
## Examples

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


# Linear regression: Scikit-learn
military = get_dataset("military")

# Convert categorical variables to proper dtypes
military = military.with_columns(
    pl.col("branch").cast(pl.Categorical)
)

mod_sk = fit_sklearn(
    "rank ~ officer + hisp + branch",
    data=military,
    engine=LinearRegression(),
)
avg_predictions(mod_sk, by="branch")

# Linear regression: Statsmodels
mod_sm = ols("rank ~ officer + hisp + branch", data=military.to_pandas()).fit()
avg_predictions(mod_sm, by="branch")

# XGBoost: Scikit-learn
airbnb = get_dataset("airbnb")

# Convert categorical variables to proper dtypes
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
"""
    + DocsModels.docstring_notes("sklearn")
)

fit_sklearn.__doc__ = docs_sklearn
