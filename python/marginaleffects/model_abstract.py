from abc import ABC, abstractmethod
from typing import Any
from .validation import ModelValidation
from . import formulaic_utils as fml


class ModelAbstract(ModelValidation, ABC):
    def __init__(self, model, vault):
        self.model = model
        self.vault = vault
        self.validation()

    def get_modeldata(self):
        if "modeldata" in self.vault:
            out = self.vault.get("modeldata")
        else:
            out = None
        return out

    def get_vcov(self, vcov=False):
        return self.vault.get("vcov", None)

    def get_coef(self):
        return self.vault.get("coef", None)

    def get_coefnames(self):
        return self.vault.get("coefnames", None)

    def get_engine_running(self):
        return self.vault.get("engine_running", None)

    def get_formula(self):
        return self.vault.get("formula", None)

    def get_formula_engine(self):
        return self.vault.get("formula_engine", "formulaic")

    def get_package(self):
        return self.vault.get("package", "unknown")

    def get_variable_type(self, name=None):
        variables = self.vault.get("variables_type")
        if isinstance(name, str) and name in variables:
            return variables[name]
        else:
            return variables

    def set_variable_type(self, name, value):
        self.vault["variables_type"][name] = value

    def find_variables(self):
        if "variable_names" in self.vault:
            return self.vault.get("variable_names")

        formula = self.get_formula()
        if isinstance(formula, str):
            out = fml.parse_variables(self.get_formula())
        else:
            out = None

        self.vault.update(variable_names=out)

        return out

    def find_response(self):
        vars = self.find_variables()
        if vars is None:
            return None
        else:
            return vars[0]

    def find_predictors(self):
        vars = self.find_variables()
        if vars is None:
            return None
        else:
            return vars[1:]

    @abstractmethod
    def get_predict(self):
        pass

    def __getattr__(self, name: str) -> Any:
        """Forward attribute access to the underlying fitted model."""
        try:
            return object.__getattribute__(self, name)
        except AttributeError:
            # Forward to the wrapped model
            return getattr(self.model, name)
