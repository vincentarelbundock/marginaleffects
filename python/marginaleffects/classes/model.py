from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, Protocol, runtime_checkable

import numpy as np
import polars as pl

from .. import formula as fml
from ..sanitize.validation import ModelValidation


@dataclass
class ModelVault:
    """Typed container for model metadata shared across all adapters."""

    coef: np.ndarray | None = None
    coefnames: np.ndarray | None = None
    formula: str | None = None
    formula_engine: str = "formulaic"
    modeldata: pl.DataFrame | None = None
    package: str = "unknown"
    vcov: np.ndarray | None = None
    variables_type: dict[str, str] = field(default_factory=dict)
    variable_names: list[str] | None = None
    engine_running: Any | None = None
    # statsmodels-specific
    design_info_patsy: Any | None = None
    pandas_categorical_orders: dict[str, list] = field(default_factory=dict)
    # sklearn-specific
    model_spec: Any | None = None
    original_columns: list[str] | None = None
    # linearmodels-specific
    multiindex: list[str] | None = None


@runtime_checkable
class ModelAdapter(Protocol):
    """Explicit contract consumed by marginaleffects estimation code."""

    def get_modeldata(self) -> pl.DataFrame: ...
    def get_vcov(self, vcov=True) -> np.ndarray | None: ...
    def get_coef(self) -> np.ndarray: ...
    def get_coefnames(self) -> np.ndarray: ...
    def get_formula(self): ...
    def get_exog(self, newdata: pl.DataFrame): ...
    def get_predict(self, params: np.ndarray, newdata) -> pl.DataFrame: ...
    def get_df(self) -> float: ...


class ModelAbstract(ModelValidation, ABC):
    def __init__(self, model, vault: ModelVault):
        self.model = model
        self.vault = vault
        self.validation()

    def get_modeldata(self) -> pl.DataFrame | None:
        return self.vault.modeldata

    def get_vcov(self, vcov=False) -> np.ndarray | None:
        return self.vault.vcov

    def get_coef(self) -> np.ndarray | None:
        return self.vault.coef

    def get_coefnames(self) -> np.ndarray | None:
        return self.vault.coefnames

    def get_engine_running(self) -> Any | None:
        return self.vault.engine_running

    def get_formula(self) -> str | None:
        return self.vault.formula

    def get_formula_engine(self) -> str:
        return self.vault.formula_engine

    def get_package(self) -> str:
        return self.vault.package

    def get_variable_type(self, name=None) -> dict[str, str]:
        variables = self.vault.variables_type
        if isinstance(name, str) and name in variables:
            return variables[name]
        else:
            return variables

    def set_variable_type(self, name, value):
        self.vault.variables_type[name] = value

    def find_variables(self) -> list[str] | None:
        if self.vault.variable_names is not None:
            return self.vault.variable_names

        formula = self.get_formula()
        if isinstance(formula, str):
            out = fml.parse_variables(self.get_formula())
        else:
            out = None

        self.vault.variable_names = out

        return out

    def find_response(self) -> str | None:
        vars = self.find_variables()
        if vars is None:
            return None
        else:
            return vars[0]

    def find_predictors(self) -> list[str] | None:
        vars = self.find_variables()
        if vars is None:
            return None
        else:
            return vars[1:]

    def get_exog(self, newdata: pl.DataFrame):
        """Convert newdata into the design matrix format expected by get_predict.

        Subclasses may override this to handle model-specific formula engines.
        The default implementation uses the model's formula to build design matrices.
        """
        from ..formula import model_matrices

        if self.vault.design_info_patsy is not None:
            f = self.vault.design_info_patsy
        else:
            f = self.get_formula()

        if callable(f):
            _, exog = f(newdata)
        else:
            _, exog = model_matrices(
                f, newdata, formula_engine=self.get_formula_engine()
            )

        return exog

    @abstractmethod
    def get_predict(self, params: np.ndarray, newdata) -> pl.DataFrame:
        pass

    def get_analytic_args(self):
        """Return adapter metadata needed by the analytic Jacobian engine."""
        return

    def get_link_functions(self):
        """Return inverse-link and derivative callables for response predictions."""
        return

    def get_exog_names(self, value):
        """Return design-column names, or ``None`` when they are unavailable."""
        design_info = getattr(value, "design_info", None)
        columns = getattr(design_info, "column_names", None)
        if columns is None:
            columns = getattr(value, "columns", None)
        return None if columns is None else list(columns)

    def get_df(self) -> float:
        return np.inf

    def get_fitted_model(self) -> Any:
        """Return the wrapped engine object for adapter-specific integrations."""
        return self.model
