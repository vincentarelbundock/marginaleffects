import numpy as np
import pandas as pd
import pytest
import statsmodels.formula.api as sm

from marginaleffects import *
from marginaleffects import MarginaleffectsResult


def test_error():
    N = 1000
    dat = pd.DataFrame({"x": np.random.normal(size=N)})
    dat["y"] = 1 + 2 * dat["x"] + np.random.normal(size=N)
    mod = sm.ols("y ~ x", data=dat).fit()
    s = avg_slopes(mod)
    assert isinstance(s, MarginaleffectsResult)

    with pytest.raises(ValueError):
        mod = sm.ols("y ~ scale(x)", data=dat).fit()
        s = avg_slopes(mod)

    with pytest.raises(ValueError):
        mod = sm.ols("y ~ center(x)", data=dat).fit()
        s = avg_slopes(mod)
