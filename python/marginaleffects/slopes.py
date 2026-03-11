from .docstrings import doc
from .comparisons import comparisons
from .classes import MarginaleffectsResult


@doc("""
# `slopes()`

`slopes()` and `avg_slopes()` estimate unit-level (conditional) partial derivative of the regression equation with respect to a regressor of interest.

- `slopes()`: unit-level (conditional) estimates.
- `avg_slopes()`: average (marginal) estimates.

The newdata argument and the `datagrid()` function can be used to control where statistics are evaluated in the predictor space: "at observed values", "at the mean", "at representative values", etc.

See the package website and vignette for examples:

- https://marginaleffects.com/chapters/slopes.html
- https://marginaleffects.com

## Parameters
{param_model}
- `variables`: (str, list, dictionary) Specifies what variables (columns) to vary in order to make the slopes.
    - str: Variable for which to compute the slopes for.
    - NoneType: Slopes are computed for all regressors in the model object (can be slow)

{param_newdata_slope}

{param_slope}

{param_vcov}

{param_conf_level}

{param_by}

{param_hypothesis}

{param_equivalence}

{param_wts}

{param_eps}

{param_eps_vcov}

{returns}

## Examples
```py
from marginaleffects import *
import statsmodels.api as sm
import statsmodels.formula.api as smf
import numpy as np

data = get_dataset("thornton")

# Create the squared distance term
data = data.with_columns(distance_sq = data['distance'] ** 2)

# Fit GLM with interactions and squared term
mod = smf.logit("outcome ~ incentive * distance * distance_sq", data=data).fit()

# Print summary
mod.summary()

# Slopes are computed for each regressor and at each observation
slopes(mod)

# Slopes are computed for each regressor and averaged over the observations
avg_slopes(mod)

# Slopes are computed with respect to the `distance` regressor at each observation
slopes(mod, variables = "distance")

# Slopes are computed with respect to the `distance` regressor and averaged over the observations
avg_slopes(mod, variables = "distance")
```

## Details
{details_tost}

{details_order_of_operations}""")
def slopes(
    model,
    variables=None,
    newdata=None,
    slope="dydx",
    vcov=True,
    conf_level=0.95,
    by=False,
    hypothesis=None,
    equivalence=None,
    wts=None,
    eps=1e-4,
    eps_vcov=None,
    **kwargs,
) -> MarginaleffectsResult:
    if callable(newdata):
        newdata = newdata(model)

    assert isinstance(eps, float)

    if slope not in ["dydx", "eyex", "eydx", "dyex"]:
        raise ValueError("slope must be one of 'dydx', 'eyex', 'eydx', 'dyex'")

    out = comparisons(
        model=model,
        variables=variables,
        newdata=newdata,
        comparison=slope,
        vcov=vcov,
        conf_level=conf_level,
        by=by,
        hypothesis=hypothesis,
        equivalence=equivalence,
        wts=wts,
        eps=eps,
        eps_vcov=eps_vcov,
        **kwargs,
    )
    return out


def avg_slopes(
    model,
    variables=None,
    newdata=None,
    slope="dydx",
    vcov=True,
    conf_level=0.95,
    by=True,
    wts=None,
    hypothesis=None,
    equivalence=None,
    eps=1e-4,
    eps_vcov=None,
    **kwargs,
) -> MarginaleffectsResult:
    if callable(newdata):
        newdata = newdata(model)

    if slope not in ["dydx", "eyex", "eydx", "dyex"]:
        raise ValueError("slope must be one of 'dydx', 'eyex', 'eydx', 'dyex'")
    out = slopes(
        model=model,
        variables=variables,
        newdata=newdata,
        slope=slope,
        vcov=vcov,
        conf_level=conf_level,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
        equivalence=equivalence,
        eps=eps,
        eps_vcov=eps_vcov,
        **kwargs,
    )

    return out


avg_slopes.__doc__ = slopes.__doc__
