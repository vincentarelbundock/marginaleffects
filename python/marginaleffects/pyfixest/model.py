import re

import numpy as np
import polars as pl

from ..classes import ModelAbstract, ModelVault
from ..classes.model import predictions_to_frame
from ..utils import ingest


class ModelPyfixest(ModelAbstract):
    def __init__(self, model, vault=None):
        if vault is None:
            vault = ModelVault()
        formula = model._fml
        modeldata = ingest(model._data)
        vault.modeldata = modeldata
        vault.formula = formula
        super().__init__(model, vault)

        # after super init & validation
        if hasattr(model, "_fixef"):
            fixef = model._fixef
            if isinstance(fixef, str) and fixef.strip().lower() in {"", "none"}:
                fixef = None
            if fixef:
                fe = str(fixef).split("+")
                for f in fe:
                    f = f.strip()
                    if f:
                        self.set_variable_type(f, "character")

    def is_linear_model(self):
        """Check if this is a linear regression model (feols) vs. non-linear (fepois, etc.)."""
        # Check the _method attribute: 'feols' for linear, 'fepois' for Poisson, etc.
        if hasattr(self.model, "_method"):
            return self.model._method == "feols"

        # Fallback: assume linear if no method attribute
        return True

    def get_coef(self):
        return np.array(self.model._beta_hat)

    def get_coefnames(self):
        return np.array(self.model._coefnames)

    def get_vcov(self, vcov=True):
        if isinstance(vcov, bool):
            if vcov is True:
                V = self.model._vcov
            else:
                V = None
        elif isinstance(vcov, (str, dict)):
            # pyfixest supports: "iid", "hetero", "HC1", "HC2", "HC3",
            # {"CRV1": "clustervar"}, {"CRV3": "clustervar"}
            self.model.vcov(vcov)
            V = self.model._vcov
        else:
            raise TypeError(
                '`vcov` must be a boolean, a string like "HC3" or "hetero", '
                'or a dict like {"CRV1": "clustervar"}.'
            )
        if V is not None:
            V = np.array(V)
        return V

    def get_exog(self, newdata: pl.DataFrame):
        """PyFixest uses raw data as input, converted to pandas."""
        return newdata.to_pandas()

    def find_predictors(self):
        modeldata = self.get_modeldata()
        variables = self.model._coefnames
        variables = [re.sub(r"\[.*\]", "", x) for x in variables]
        # Extract inner variable from C(...) or i(...) wrappers
        extracted = []
        for v in variables:
            m = re.match(r"^[Ci]\((\w+)\)$", v)
            if m:
                extracted.append(m.group(1))
            else:
                extracted.append(v)
        variables = [x for x in extracted if x in modeldata.columns]
        variables = pl.Series(variables).unique().to_list()
        return variables

    def get_predict(self, params, newdata: pl.DataFrame):
        # PyFixest does not expose prediction with arbitrary coefficients. Restore
        # the fitted coefficients even when prediction raises, so finite-difference
        # inference never mutates the user's model permanently.
        m = self.model
        original_params = m._beta_hat

        # pyfixest does not support polars
        try:
            newdata = newdata.to_pandas()
        except:  #  noqa
            pass

        try:
            m._beta_hat = params
            p = m.predict(newdata=newdata)
        finally:
            m._beta_hat = original_params
        return predictions_to_frame(p)
