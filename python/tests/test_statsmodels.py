import statsmodels.api as sm
from marginaleffects import *

data = get_dataset("thornton")
mod = fit_statsmodels(
    formula="outcome ~ distance + incentive",
    data=data,
    engine=sm.OLS,
    kwargs_fit={"cov_type": "HC3"},
)


def test_fit_statsmodels():
    p = predictions(mod)
    s = slopes(mod, variables="distance")
    c = comparisons(mod, variables="distance")
    assert p.shape[0] == 2825
    assert s.shape[0] == 2825
    assert c.shape[0] == 2825
