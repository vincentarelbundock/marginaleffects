import re
import numpy as np
import pandas as pd
import narwhals as nw
from typing import Any, Dict
import polars as pl
from ..docs import DocsModels
from ..utils import ingest
from formulaic.parser.algos.tokenize import tokenize
from ..model_abstract import ModelAbstract
from ..formulaic_utils import (
    listwise_deletion,
    model_matrices,
)


class ModelLinearmodels(ModelAbstract):
    def __init__(self, model, vault={}):
        super().__init__(model, vault)

    def _to_pandas(self, df):
        """
        Convert a DataFrame to pandas format with MultiIndex.

        Transforms a DataFrame containing index columns into a pandas DataFrame
        with these columns set as MultiIndex levels.

        Parameters
        ----------
        df : nw.IntoFrame
            DataFrame containing the original index variables as columns.
            Must include all columns specified in self.vault['multiindex'].

        Returns
        -------
        pandas.DataFrame
            DataFrame with MultiIndex constructed from the index columns.

        Raises
        ------
        ValueError
            If any of the required index columns are missing from the input DataFrame.
        """

        multiindex = self.vault.get("multiindex")
        if not set(multiindex).issubset(nw.from_native(df).columns):
            multiindex_str = ",".join(multiindex)
            raise ValueError(
                f"The DataFrame must contain the original multiindex ({multiindex_str}) as columns."
            )

        return nw.from_native(df).to_pandas().set_index(multiindex)

    def get_coef(self):
        return np.array(self.model.params)

    def get_coef_names(self):
        return np.array(self.model.params.index.to_numpy())

    def get_vcov(self, vcov=True):
        if isinstance(vcov, bool):
            if vcov is True:
                V = self.model.cov
            else:
                V = None

        if isinstance(vcov, str):
            supported_vcov = [
                "unadjusted",
                "homoskedastic",
                "robust",
                "heteroskedastic",
                "driscoll-kraay",
                "autocorrelated",
                "cluster",
                "kernel",
            ]
            if vcov not in supported_vcov:
                raise ValueError(
                    f"Unknown vcov type: {vcov}.\n"
                    f"Valid options are: {', '.join(supported_vcov)}"
                )

            V = self.fit(cov_type=vcov).cov

        if V is not None:
            V = np.array(V)
            if V.shape != (len(self.coef), len(self.coef)):
                raise ValueError(
                    "vcov must be a square numpy array with dimensions equal to the length of self.coef"
                )

        return V

    def find_response(self):
        return self.model.model.dependent.vars[0]

    def find_predictors(self):
        formula = self.get_formula()
        columns = self.get_modeldata().columns
        order = {}
        for var in columns:
            match = re.search(rf"\b{re.escape(var)}\b", formula.split("~")[1])
            if match:
                order[var] = match.start()
        variables = sorted(order, key=lambda i: order[i])
        return variables

    def get_predict(self, params, newdata):
        if isinstance(newdata, np.ndarray):
            exog = newdata
        else:
            y, exog = model_matrices(
                self.get_formula(),
                self._to_pandas(newdata),
                formula_engine="linearmodels",
            )

        p = self.model.model.predict(params=params, exog=exog).predictions.values

        if p.ndim == 1:
            p = pl.DataFrame({"rowid": range(newdata.shape[0]), "estimate": p})

        elif p.ndim == 2:
            colnames = {f"column_{i}": str(i) for i in range(p.shape[1])}
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

        return p.with_columns(pl.col("rowid").cast(pl.Int32))

    def get_df(self):
        return self.model.df_resid


def parse_linearmodels_formula(formula: str):
    """
    Parse a formula as linearmodels would and extract panel effects specifications.

    This function processes a formula containing potential EntityEffects, FixedEffects,
    and TimeEffects terms. It removes these effect terms from the formula and converts
    them into keyword arguments for linearmodels estimation functions.

    Parameters
    ----------
    formula : str
        A string representing a linearmodels formula (e.g., "y ~ x1 + x2 + EntityEffects").
        The formula may contain special terms: EntityEffects, FixedEffects, and TimeEffects.

    Returns
    -------
    tuple[str, dict[str, bool]]
        A tuple containing:
        - str: The cleaned formula with effects terms removed
        - dict: Keyword arguments for panel effects with keys:
            - 'entity_effects': True if EntityEffects or FixedEffects present
            - 'time_effects': True if TimeEffects present

    Raises
    ------
    ValueError
        If both EntityEffects and FixedEffects are present in the formula.

    Examples
    --------
    >>> formula = "y ~ x1 + FixedEffects"
    >>> parse_linearmodels_formula(formula)
    ('y ~ x1', {'entity_effects': True, 'time_effects': False})

    Notes
    -----
    - EntityEffects and FixedEffects are treated as equivalent for entity effects
    - The function assumes the first variable in the formula is the dependent variable
    - The returned formula will be in the format "y ~ x1 + x2 + ..."
    """

    effects_tokens = {
        "EntityEffects": False,
        "FixedEffects": False,
        "TimeEffects": False,
    }
    effects_kwargs = {"entity_effects": False, "time_effects": False}

    # add + 0 to start of the rhs of the formula to remove intercept by default
    # similar to linearmodels.model.panel.PanelFormulaParser
    # adding + 1 of - 1 to the formula will add/remove intercept as expected
    lhs, rhs = formula.split("~")
    formula = f"{lhs.strip()} ~ 0 + {rhs.strip()}"
    tokens = [token.token for token in tokenize(formula)]

    for effect in effects_tokens.keys():
        try:
            idx = tokens.index(effect)
            effects_tokens[effect] = True
            _ = tokens.pop(idx)

            # Check if previous token was a "+" and remove it
            if idx > 0 and tokens[idx - 1] == "+":
                _ = tokens.pop(idx - 1)
        except ValueError:
            pass

    if effects_tokens["EntityEffects"] and effects_tokens["FixedEffects"]:
        raise ValueError("Cannot use both FixedEffects and EntityEffects")

    effects_kwargs["entity_effects"] = (
        effects_tokens["EntityEffects"] or effects_tokens["FixedEffects"]
    )
    effects_kwargs["time_effects"] = effects_tokens["TimeEffects"]

    cleaned_formula = " ".join(tokens)

    return cleaned_formula, effects_kwargs


# @validate_types
def fit_linearmodels(
    formula: str,
    data: pd.DataFrame,
    engine: None,
    kwargs_engine: Dict[str, Any] = {},
    kwargs_fit: Dict[str, Any] = {},
) -> ModelLinearmodels:
    """
    Fit a linearmodels model with output that is compatible with pymarginaleffects.

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(fit_linearmodels)`
    """
    linearmodels_formula, effects = parse_linearmodels_formula(formula)

    d = listwise_deletion(linearmodels_formula, data=data)
    y, X = model_matrices(linearmodels_formula, d, formula_engine="linearmodels")
    out = engine(dependent=y, exog=X, **kwargs_engine, **effects).fit(**kwargs_fit)

    vault = {
        "formula_engine": "linearmodels",
        "multiindex": list(d.index.names),
        "formula": linearmodels_formula,
        "modeldata": ingest(d),
        "package": "linearmodels",
    }

    return ModelLinearmodels(out, vault)


docs_linearmodels = (
    """
# `fit_linearmodels()`

Fit a linearmodels model with output that is compatible with pymarginaleffects.

This function streamlines the process of fitting linearmodels panel models by:

1. Parsing panel effects from the formula
2. Handling missing values
3. Creating model matrices
4. Fitting the model with specified options

## Parameters

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

`engine`: (callable) linearmodels model class (e.g., PanelOLS, BetweenOLS, FirstDifferenceOLS)

`kwargs_engine`: (dict, default={}) Additional arguments passed to the model initialization.

* Example: `{'weights': weights_array}`

`kwargs_fit`: (dict, default={}) Additional arguments passed to the model's fit method.

* Example: `{'cov_type': 'robust'}`
"""
    + DocsModels.docstring_fit_returns("Linearmodels")
    + """
## Examples

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
"""
    + DocsModels.docstring_notes("linearmodels")
)

fit_linearmodels.__doc__ = docs_linearmodels
