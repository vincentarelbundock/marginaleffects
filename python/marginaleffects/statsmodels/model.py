import numpy as np
import patsy
import polars as pl

from .. import formula as fml
from ..classes import ModelAbstract, ModelVault
from ..docstrings import doc
from ..sanitize.utils import validate_types
from ..utils import ingest


def _fitted_exog_names(model):
    """Names of the fitted ``exog`` columns.

    ``exog_names`` can append extra parameters after the design columns (e.g.,
    OrderedModel thresholds), so keep only the first ``exog.shape[1]`` names.
    """
    names = list(getattr(model.model, "exog_names", None) or [])
    exog = getattr(model.model, "exog", None)
    if exog is not None and np.ndim(exog) == 2:
        names = names[: np.shape(exog)[1]]
    return names


def _fitted_design_spec(model):
    """Return the fitted Patsy DesignInfo or Formulaic ModelSpec, if any.

    Native formula fits store the spec on ``model.model.data`` (``model_spec``,
    or ``design_info`` in older statsmodels); ``fit_statsmodels()`` passes a
    Formulaic ``ModelMatrix`` whose ``model_spec`` stays on ``orig_exog``. A
    candidate is used only if it can produce every fitted ``exog`` column.
    """
    data = model.model.data
    exog_names = _fitted_exog_names(model)
    candidates = (
        getattr(data, "model_spec", None),
        getattr(data, "design_info", None),
        getattr(getattr(data, "orig_exog", None), "model_spec", None),
    )
    for spec in candidates:
        columns = getattr(spec, "column_names", None)
        if columns is not None and set(exog_names).issubset(columns):
            return spec
    return None


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

        # Replay the fitted encoding: Formulaic and Patsy can order the same
        # interaction terms differently, and transforms have fitted state.
        spec = _fitted_design_spec(model)
        if isinstance(spec, patsy.DesignInfo):
            vault.design_info_patsy = spec
        elif spec is not None:
            vault.model_spec = spec
        elif vault.model_spec is None:
            vault.formula_engine = "patsy"
        super().__init__(model, vault)

    def get_exog(self, newdata: pl.DataFrame):
        """Build a prediction matrix with the fitted model's encoding."""
        if self.vault.design_info_patsy is not None:
            exog = patsy.build_design_matrices(
                [self.vault.design_info_patsy], newdata.to_pandas()
            )[0]
            return self._align_to_fitted(exog, exog.design_info.column_names)

        if self.vault.model_spec is not None:
            exog = self._replay_model_spec(newdata)
            return self._align_to_fitted(exog, exog.columns)

        formula = self.get_formula()
        if self.vault.formula_engine == "patsy":
            return patsy.dmatrix(formula.split("~", 1)[1], newdata.to_pandas())

        from ..formula import model_matrices

        _, exog = model_matrices(
            formula, newdata, formula_engine=self.get_formula_engine()
        )
        return exog

    def _replay_model_spec(self, newdata):
        from formulaic.errors import FactorEvaluationError

        try:
            return self.vault.model_spec.get_model_matrix(
                newdata.to_pandas(), context={"np": np}
            )
        except FactorEvaluationError as exc:
            raise ValueError(
                "marginaleffects could not rebuild the design matrix of this "
                "statsmodels model with formulaic. statsmodels does not keep "
                "the namespace in which a formula was evaluated with the "
                "formulaic engine, so only columns of the data and `np` are "
                "available when building new design matrices. Create the "
                "transformed variable as a data column before fitting, or fit "
                "with statsmodels.formula.options.formula_engine = 'patsy'. "
                f"Formulaic error: {exc}"
            ) from exc

    def _align_to_fitted(self, exog, columns):
        """Select and order rebuilt columns like the fitted ``exog``.

        A replayed specification can order columns differently from the
        fitted design (OrderedModel subsets the spec to drop its intercept),
        so match by name instead of position.
        """
        import pandas as pd

        exog = pd.DataFrame(np.asarray(exog), columns=list(columns))
        fitted_names = _fitted_exog_names(self.model)
        missing = [name for name in fitted_names if name not in exog.columns]
        if missing:
            raise ValueError(
                "The design matrix rebuilt from the fitted formula specification "
                f"lacks these columns of the fitted model: {missing}. "
                f"Rebuilt columns: {list(exog.columns)}."
            )
        return exog[fitted_names]

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
            raise TypeError(
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
            exog = np.asarray(self.get_exog(newdata))
        # OrderedModel uses thresholds instead of an intercept. Designs replayed
        # from a fitted spec already match ``exog_names``; the formula fallback
        # in ``get_exog`` may still include an intercept column.
        if (
            self._is_ordered_model()
            and isinstance(exog, np.ndarray)
            and exog.shape[1] == self.model.model.k_vars + 1
        ):
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

    def get_analytic_args(self):
        if type(self.model.model).__name__ == "OLS":
            return {"model_type": "linear", "family": None, "link": None}
        return None

    def get_link_functions(self):
        inner_model = self.model.model
        if type(inner_model).__name__ != "GLM":
            return None
        for name in ("offset", "exposure"):
            value = getattr(inner_model, name, None)
            if value is not None and not np.allclose(value, 0):
                return None
        link = getattr(getattr(inner_model, "family", None), "link", None)
        linkinv = getattr(link, "inverse", None)
        mu_eta = getattr(link, "inverse_deriv", None)
        if not callable(linkinv) or not callable(mu_eta):
            return None
        return linkinv, mu_eta

    def get_exog_names(self, value):
        names = super().get_exog_names(value)
        if names is None:
            names = getattr(self.model.model, "exog_names", None)
        return None if names is None else list(names)


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
    formula: str, data: pl.DataFrame, engine, kwargs_engine=None, kwargs_fit=None
):
    kwargs_engine = {} if kwargs_engine is None else kwargs_engine
    kwargs_fit = {} if kwargs_fit is None else kwargs_fit
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
