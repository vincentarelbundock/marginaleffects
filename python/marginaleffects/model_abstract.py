from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

import numpy as np
import polars as pl

from .validation import ModelValidation
from . import formulaic_utils as fml


@dataclass
class ModelVault:
    """Typed container for model metadata shared across all adapters."""

    coef: Optional[np.ndarray] = None
    coefnames: Optional[np.ndarray] = None
    formula: Optional[str] = None
    formula_engine: str = "formulaic"
    modeldata: Optional[pl.DataFrame] = None
    package: str = "unknown"
    vcov: Optional[np.ndarray] = None
    variables_type: Dict[str, str] = field(default_factory=dict)
    variable_names: Optional[List[str]] = None
    engine_running: Optional[Any] = None
    # statsmodels-specific
    design_info_patsy: Optional[Any] = None
    pandas_categorical_orders: Dict[str, list] = field(default_factory=dict)
    # sklearn-specific
    model_spec: Optional[Any] = None
    original_columns: Optional[List[str]] = None
    # linearmodels-specific
    multiindex: Optional[List[str]] = None


class ModelAbstract(ModelValidation, ABC):
    def __init__(self, model, vault: ModelVault):
        self.model = model
        self.vault = vault
        self.validation()

    def get_modeldata(self) -> Optional[pl.DataFrame]:
        return self.vault.modeldata

    def get_vcov(self, vcov=False) -> Optional[np.ndarray]:
        return self.vault.vcov

    def get_coef(self) -> Optional[np.ndarray]:
        return self.vault.coef

    def get_coefnames(self) -> Optional[np.ndarray]:
        return self.vault.coefnames

    def get_engine_running(self) -> Optional[Any]:
        return self.vault.engine_running

    def get_formula(self) -> Optional[str]:
        return self.vault.formula

    def get_formula_engine(self) -> str:
        return self.vault.formula_engine

    def get_package(self) -> str:
        return self.vault.package

    def get_variable_type(self, name=None) -> Dict[str, str]:
        variables = self.vault.variables_type
        if isinstance(name, str) and name in variables:
            return variables[name]
        else:
            return variables

    def set_variable_type(self, name, value):
        self.vault.variables_type[name] = value

    def find_variables(self) -> Optional[List[str]]:
        if self.vault.variable_names is not None:
            return self.vault.variable_names

        formula = self.get_formula()
        if isinstance(formula, str):
            out = fml.parse_variables(self.get_formula())
        else:
            out = None

        self.vault.variable_names = out

        return out

    def find_response(self) -> Optional[str]:
        vars = self.find_variables()
        if vars is None:
            return None
        else:
            return vars[0]

    def find_predictors(self) -> Optional[List[str]]:
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
        from .formulaic_utils import model_matrices

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

    def __getattr__(self, name: str) -> Any:
        """Forward attribute access to the underlying fitted model."""
        try:
            return object.__getattribute__(self, name)
        except AttributeError:
            # Forward to the wrapped model
            return getattr(self.model, name)
