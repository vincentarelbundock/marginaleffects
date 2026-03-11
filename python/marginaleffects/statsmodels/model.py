from typing import Optional, Dict, Any
import numpy as np
import polars as pl
import patsy
from ..docs import doc
from ..classes import ModelAbstract, ModelVault
from .. import formula as fml
from ..utils import ingest
from ..sanitize.utils import validate_types


class ModelStatsmodels(ModelAbstract):
    def __init__(self, model, vault=None):
        if vault is None:
            vault = ModelVault()

        # Store pandas categorical orders before ingesting to preserve them
        pandas_categorical_orders = {}
        if hasattr(model.model.data, "frame"):
            for col in model.model.data.frame.columns:
                if (
                    hasattr(model.model.data.frame[col].dtype, "name")
                    and model.model.data.frame[col].dtype.name == "category"
                ):
                    pandas_categorical_orders[col] = model.model.data.frame[
                        col
                    ].cat.categories.tolist()

        vault.coef = np.array(model.params)  # multinomial models are 2d
        vault.coefnames = np.array(model.params.index.to_numpy())
        vault.formula = model.model.formula
        vault.modeldata = ingest(model.model.data.frame)
        vault.pandas_categorical_orders = pandas_categorical_orders
        vault.variable_names = [
            model.model.endog_names
        ] + fml.extract_patsy_variable_names(vault.formula, vault.modeldata)
        if not hasattr(model, "formula"):
            vault.formula_engine = "patsy"
            if hasattr(model.model.data, "design_info"):
                vault.design_info_patsy = model.model.data.design_info
        super().__init__(model, vault)

    def get_exog(self, newdata: pl.DataFrame):
        """Build design matrix using patsy for statsmodels models."""
        import re

        if self.vault.design_info_patsy is not None:
            f = self.vault.design_info_patsy
        else:
            f = self.get_formula()

        if callable(f):
            _, exog = f(newdata)
        elif self.vault.formula_engine == "patsy":
            fml_rhs = re.sub(
                r".*~", "", f if isinstance(f, str) else self.get_formula()
            )
            exog = patsy.dmatrix(fml_rhs, newdata.to_pandas())
        else:
            from ..formula import model_matrices

            _, exog = model_matrices(
                f, newdata, formula_engine=self.get_formula_engine()
            )

        return exog

    def get_vcov(self, vcov=True):
        if isinstance(vcov, bool):
            if vcov is True:
                V = self.model.cov_params()
            else:
                V = None
        elif isinstance(vcov, str):
            # OLS/WLS/GLS have cov_HC0..cov_HC3 attributes directly
            lab = f"cov_{vcov}"
            if hasattr(self.model, lab):
                V = getattr(self.model, lab)
            # For all other statsmodels (GLM, Logit, Probit, Poisson, etc.),
            # robust vcov must be specified at fit time via cov_type=. If
            # the model was already fit with that cov_type, cov_params()
            # returns the robust matrix and vcov=True suffices.
            elif hasattr(self.model, "get_robustcov_results"):
                rob = self.model.get_robustcov_results(cov_type=vcov)
                V = rob.cov_params()
            else:
                raise ValueError(
                    f"This model does not support vcov='{vcov}' post-hoc. "
                    f"For non-linear statsmodels (GLM, Logit, Probit, etc.), "
                    f"specify robust standard errors at fit time:\n"
                    f"  model.fit(cov_type='{vcov}')\n"
                    f"Then use vcov=True in marginaleffects functions."
                )
        else:
            raise ValueError(
                '`vcov` must be a boolean, a string like "HC3", or a numpy array.'
            )

        if V is not None:
            V = np.array(V)
            n = len(self.get_coef().ravel())
            if V.shape != (n, n):
                raise ValueError(
                    f"vcov must be a square numpy array with dimensions equal to "
                    f"the number of coefficients ({n}). Got shape {V.shape}."
                )

        return V

    def _is_ordered_model(self):
        return "OrderedModel" in type(self.model.model).__name__

    def get_predict(self, params, newdata: pl.DataFrame):
        if isinstance(newdata, np.ndarray):
            exog = newdata
        elif hasattr(newdata, "to_numpy"):
            exog = newdata.to_numpy()
        else:
            newdata = newdata.to_pandas()
            y, exog = patsy.dmatrices(self.get_formula(), newdata)
        # OrderedModel uses thresholds instead of intercept, so strip the
        # intercept column that patsy adds to the design matrix.
        if self._is_ordered_model() and isinstance(exog, np.ndarray):
            if exog.shape[1] == self.model.model.k_vars + 1:
                exog = exog[:, 1:]
        p = self.model.model.predict(params, exog)
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
                .unpivot(index="rowid", variable_name="group", value_name="estimate")
            ).sort("group", "rowid")  # somehow very important for SEs
        else:
            raise ValueError(
                "The `predict()` method must return an array with 1 or 2 dimensions."
            )
        p = p.with_columns(pl.col("rowid").cast(pl.Int32))
        return p

    def get_df(self):
        return self.model.df_resid

    def get_autodiff_config(self) -> Optional[Dict[str, Any]]:
        """
        Return autodiff configuration if this model is JAX-compatible.

        Returns None if:
        - JAX is not installed
        - Model type unsupported (not OLS/GLM)
        - GLM has offset or exposure
        - Family/link combination unsupported

        Returns dict with:
        - model_type: "linear" or "glm"
        - family_type: int (Family enum) or None
        - link_type: int (Link enum) or None
        """
        # Check JAX availability
        try:
            from ..autodiff import Family, Link

            # Check if JAX is actually available (not dummy module)
            _ = int(Family.GAUSSIAN)
        except (ImportError, TypeError):
            return None

        inner_model = self.model.model  # the unfitted statsmodels model
        model_class = type(inner_model).__name__

        # === OLS (linear models) ===
        if model_class == "OLS":
            return {"model_type": "linear", "family_type": None, "link_type": None}

        # === GLM ===
        if model_class == "GLM":
            # Reject if offset is used
            if hasattr(inner_model, "offset") and inner_model.offset is not None:
                if not np.allclose(inner_model.offset, 0):
                    return None

            # Reject if exposure is used
            if hasattr(inner_model, "exposure") and inner_model.exposure is not None:
                if not np.allclose(inner_model.exposure, 0):
                    return None

            # Map statsmodels family to our enum
            family_map = {
                "Gaussian": Family.GAUSSIAN,
                "Binomial": Family.BINOMIAL,
                "Poisson": Family.POISSON,
                "Gamma": Family.GAMMA,
                "InverseGaussian": Family.INVERSE_GAUSSIAN,
            }

            # Map statsmodels link to our enum
            link_map = {
                "Identity": Link.IDENTITY,
                "identity": Link.IDENTITY,
                "Log": Link.LOG,
                "log": Link.LOG,
                "Logit": Link.LOGIT,
                "logit": Link.LOGIT,
                "Probit": Link.PROBIT,
                "probit": Link.PROBIT,
                "InversePower": Link.INVERSE,
                "inverse_power": Link.INVERSE,
                "InverseSquared": Link.INVERSE,
                "inverse_squared": Link.INVERSE,
                "Sqrt": Link.SQRT,
                "sqrt": Link.SQRT,
                "CLogLog": Link.CLOGLOG,
                "cloglog": Link.CLOGLOG,
            }

            family_name = inner_model.family.__class__.__name__
            link_name = inner_model.family.link.__class__.__name__

            if family_name not in family_map:
                return None
            if link_name not in link_map:
                return None

            return {
                "model_type": "glm",
                "family_type": int(family_map[family_name]),
                "link_type": int(link_map[link_name]),
            }

        return None


@doc("""
# `fit_statsmodels()`

Fit a statsmodels model with output that is compatible with pymarginaleffects.

This function streamlines the process of fitting statsmodels models by:
1. Parsing the formula
2. Handling missing values
3. Creating model matrices
4. Fitting the model with specified options

## Parameters

{models_formula}

- `data`: (pandas.DataFrame or polars.DataFrame) Dataframe with the response variable and predictors.

{models_categorical_requirement}

- `engine`: (callable) statsmodels model class (e.g., OLS, Logit).

{models_kwargs_engine}

- `kwargs_fit`: (dict, default={{}}) Additional arguments passed to the model's fit method.
    - Example: `{{'cov_type': 'HC3'}}`

{models_fit_returns_Statsmodels}

## Examples

```python
from marginaleffects import fit_statsmodels, get_dataset, predictions, slopes, comparisons

import statsmodels.api as sm

data = get_dataset("thornton")

# Model with robust standard errors
model_robust = fit_statsmodels(
    formula="outcome ~ distance + incentive",
    data=data,
    engine=sm.OLS,
    kwargs_fit={{"cov_type": "HC3"}}
)

predictions(model_robust)
slopes(model_robust)
comparisons(model_robust)
```

{models_notes_statsmodels}""")
@validate_types
def fit_statsmodels(
    formula: str, data: pl.DataFrame, engine, kwargs_engine={}, kwargs_fit={}
):
    d = fml.listwise_deletion(formula, data=data)
    y, X = fml.model_matrices(formula, d)
    mod = engine(endog=y, exog=X, **kwargs_engine)
    mod = mod.fit(**kwargs_fit)
    mod.model.formula = formula
    mod.model.data.frame = d
    vault = ModelVault(
        modeldata=d,
        formula=formula,
        package="statsmodels",
    )
    return ModelStatsmodels(mod, vault)
