import numpy as np
import polars as pl

from marginaleffects.classes import ModelAdapter


class CompleteAdapter:
    def get_modeldata(self):
        return pl.DataFrame({"y": [1.0], "x": [2.0]})

    def get_vcov(self, vcov=True):
        return np.eye(2) if vcov else None

    def get_coef(self):
        return np.ones(2)

    def get_coefnames(self):
        return np.asarray(["intercept", "x"])

    def get_formula(self):
        return "y ~ x"

    def get_exog(self, newdata):
        return np.column_stack([np.ones(newdata.height), newdata["x"]])

    def get_predict(self, params, newdata):
        return pl.DataFrame({"rowid": [0], "estimate": [3.0]})

    def get_df(self):
        return np.inf


def test_model_adapter_protocol_is_structural():
    assert isinstance(CompleteAdapter(), ModelAdapter)


def test_model_adapter_protocol_rejects_missing_capabilities():
    assert not isinstance(object(), ModelAdapter)
