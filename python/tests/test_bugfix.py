import numpy as np
import pandas as pd
import polars as pl
from statsmodels.formula.api import glm, ols

from marginaleffects import predictions, MarginaleffectsResult


def test_issue_25():
    d = pd.DataFrame(np.random.randint(0, 100, size=(100, 4)), columns=list("ABCD"))
    train = d.head(50)
    test = d.tail(50)
    m = ols("A ~ B + C + D", data=train).fit()
    p = predictions(m, newdata=test)
    assert isinstance(p, MarginaleffectsResult)
    assert isinstance(p.data, pl.DataFrame)


def test_issue_226_np_context():
    df = pd.DataFrame({"y": np.arange(5), "x": np.arange(5)})
    mod = glm("y ~ np.maximum(x, 1)", data=df).fit()
    out = predictions(mod, newdata=df)
    assert isinstance(out, MarginaleffectsResult)
    assert isinstance(out.data, pl.DataFrame)
