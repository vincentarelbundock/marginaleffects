import polars as pl

from .docs import DocsDetails, DocsParameters

from .result import MarginaleffectsResult
from .equivalence import get_equivalence
from .hypothesis import get_hypothesis
from .sanity import sanitize_hypothesis_null, sanitize_vcov
from .sanitize_model import sanitize_model
from .uncertainty import get_jacobian, get_se, get_z_p_ci
from .utils import sort_columns
from .hypotheses_joint import joint_hypotheses


def hypotheses(
    model,
    hypothesis=None,
    conf_level=0.95,
    vcov=True,
    equivalence=None,
    eps_vcov=None,
    joint=False,
    joint_test="f",
):
    """
    (Non-)Linear Tests for Null Hypotheses, Joint Hypotheses, Equivalence, Non Superiority, and Non Inferiority.

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(hypotheses)`
    """
    model = sanitize_model(model)

    if joint:
        out = joint_hypotheses(
            model, joint_index=joint, joint_test=joint_test, hypothesis=hypothesis
        )
        return out

    hypothesis_null = sanitize_hypothesis_null(hypothesis)
    V = sanitize_vcov(vcov, model)

    # estimands
    def fun(x):
        out = pl.DataFrame({"term": model.get_coefnames(), "estimate": x})
        out = get_hypothesis(out, hypothesis=hypothesis)
        return out

    out = fun(model.get_coef())
    if vcov is not None:
        J = get_jacobian(fun, model.get_coef(), eps_vcov=eps_vcov)
        se = get_se(J, V)
        out = out.with_columns(pl.Series(se).alias("std_error"))
        out = get_z_p_ci(
            out, model, conf_level=conf_level, hypothesis_null=hypothesis_null
        )
    else:
        J = None
    out = get_equivalence(out, equivalence=equivalence)
    out = sort_columns(out, by=None)
    out = MarginaleffectsResult(out, conf_level=conf_level, jacobian=J)
    return out


hypotheses.__doc__ = (
    """
# `hypotheses()`

(Non-)Linear Tests for Null Hypotheses, Joint Hypotheses, Equivalence, Non Superiority, and Non Inferiority.

This function calculates uncertainty estimates as first-order approximate standard errors for linear or non-linear
functions of a vector of random variables with known or estimated covariance matrix. It emulates the behavior of
the excellent and well-established `car::deltaMethod` and `car::linearHypothesis` functions in R, but it supports
more models; requires fewer dependencies; expands the range of tests to equivalence and superiority/inferiority;
and offers convenience features like robust standard errors.

To learn more, visit the package website: <https://marginaleffects.com/>

## Parameters
* model : object
    Model object estimated by `statsmodels`
"""
    + DocsParameters.docstring_hypothesis
    + DocsParameters.docstring_conf_level
    + DocsParameters.docstring_vcov
    + DocsParameters.docstring_equivalence
    + DocsParameters.docstring_eps_vcov
    # add joint param docstring
    # add joint test param dosctring
    + """
* joint: (bool, str, List[str], default = `False`) Specifies the joint test of statistical significance. The null hypothesis value can be set using the hypothesis argument.
    - `False`: Hypothesis are not tested jointly
    - `True`: Hypothesis are tested jointly
    - str: A regular expression to match parameters to be tested jointly.
    - List[str]: Parameter names to be tested jointly as displayed by `mod.model.data.param_names`
    - List[int]: Parameter positions to test jointly where positions refer to the order specified by `mod.model.data.param_names`
    
* joint_test: (str, default=`"f"`) Chooses the type of test between `"f"` and `"chisq"`


## Returns
(MarginaleffectsResult)
* DataFrame containing the results of the hypothesis tests.

## Examples
```py
from marginaleffects import *

import statsmodels.api as sm
import statsmodels.formula.api as smf
data = get_dataset("thornton")
model = smf.ols("outcome ~ distance + incentive", data=data).fit()

# When `hypothesis` is `None`, `hypotheses()` returns a DataFrame of parameters
hypotheses(model)

# A different null hypothesis
hypotheses(model, hypothesis = 3)

# Test of equality between coefficients
hypotheses(model, hypothesis="distance = incentive")

# Non-linear function
hypotheses(model, hypothesis="(distance + incentive) = 0.1")

# Robust standard errors
hypotheses(model, hypothesis="distance = incentive", vcov="HC3")

# Equivalence, non-inferiority, and non-superiority tests
hypotheses(model, equivalence=(0.0, 10.0))

# Joint hypothesis tests
hypotheses(model, joint=["distance", "incentive"])

# Joint hypothesis tests with a regular expression
hypotheses(model, joint="distance|incentive")

# Joint hypothesis tests with a regular expression
hypotheses(model, joint="i$") # matches `incentive` and `distance` columns
```
## Warnings
* Warning #1: Tests are conducted directly on the scale defined by the `type` argument. For some models, it can make sense to conduct hypothesis or equivalence tests on the `"link"` scale instead of the `"response"` scale which is often the default.
* Warning #2: For hypothesis tests on objects produced by the `marginaleffects` package, it is safer to use the `hypothesis` argument of the original function.
* Warning #3: The tests assume that the `hypothesis` expression is (approximately) normally distributed, which for non-linear functions of the parameters may not be realistic. More reliable confidence intervals can be obtained using the `inferences()` (in R only) function with `method = "boot"`

## Details
"""
    + DocsDetails.docstring_tost
)
