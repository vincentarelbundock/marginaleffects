# `hypotheses()` {.unnumbered}

(Non-)Linear Tests for Null Hypotheses, Joint Hypotheses, Equivalence, Non Superiority, and Non Inferiority.

This function calculates uncertainty estimates as first-order approximate standard errors for linear or non-linear
functions of a vector of random variables with known or estimated covariance matrix. It emulates the behavior of
the excellent and well-established `car::deltaMethod` and `car::linearHypothesis` functions in R, but it supports
more models; requires fewer dependencies; expands the range of tests to equivalence and superiority/inferiority;
and offers convenience features like robust standard errors.

To learn more, visit the package website: <https://marginaleffects.com/>

## Parameters {.unnumbered}
* model : object
    Model object estimated by `statsmodels`

`hypothesis`: (str, int, float, list of str, numpy array) Specifies a hypothesis test or custom contrast

* Number to specify the null hypothesis.
* Numpy array with a number of rows equal to the number of estimates.
* String equation with an equal sign and estimate number in b0, b1, b2, etc. format. 
    - "b0 = b1"
    - "b0 - (b1 + b2) = 0"
* Two-side formula like "ratio ~ reference"
    - Left-hand side: "ratio", "difference"
    - Right-hand side: 'reference', 'sequential', 'pairwise', 'revreference', 'revsequential', 'revpairwise'

- int, float: The null hypothesis used in the computation of Z and p-values (before applying transform) 
- str: 
    * equation specifying linear or non-linear hypothesis tests. Use the names of the model variables, or use `b0`, `b1` to identify the position of each parameter. The `b*` wildcard can be used to test hypotheses on all estimates. Examples:
        - `hp = drat`
        - `hp + drat = 12`
        - `b0 + b1 + b2 = 0`
        - `b* / b0 = 1`
    * one of the following hypothesis test strings:
        - `pairwise` and `revpairwise`: pairwise differences between estimates in each row.
        - `reference` and `revreference`: differences between the estimates in each row and the estimate in the first row.
        - `sequential` and `revsequential`: differences between an estimate and the estimate in the next row.
- list of strings: Multiple hypotheses evaluated in sequence, each processed as if passed individually. The resulting rows are stacked in the order supplied. Example: `["b1 - b0 = 0", "b2 = 1"]`.
- numpy.ndarray: Each column is a vector of weights. The output is the dot product between these vectors of weights and the vectors of estimates. e.g. `hypothesis=np.array([[1, 1, 2], [2, 2, 3]]).T`
- See the Examples section and the vignette: https://marginaleffects.com/chapters/hypothesis.html

`conf_level`: (float, default=0.95) Numeric value specifying the confidence level for the confidence intervals.

`vcov`: (bool, np.ndarray, default=True) Type of uncertainty estimates to report (e.g. for robust standard errors). Acceptable values are:

- `True`: Use the model's default covariance matrix.
- `False`: Do not compute standard errors.
- String: Literal indicating the kind of uncertainty estimates to return:
    - Heteroskedasticity-consistent: `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`.
- np.ndarray: A custom square covariance matrix.

`equivalence`: (list, optional) List of 2 numeric float values specifying the bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. See the Details section below.

`eps_vcov`: (float) optional custom value for the finite difference approximation of the jacobian matrix. By default, the function uses the square root of the machine epsilon.

* joint: (bool, str, List[str], default = `False`) Specifies the joint test of statistical significance. The null hypothesis value can be set using the hypothesis argument.
    - `False`: Hypothesis are not tested jointly
    - `True`: Hypothesis are tested jointly
    - str: A regular expression to match parameters to be tested jointly.
    - List[str]: Parameter names to be tested jointly as displayed by `mod.model.data.param_names`
    - List[int]: Parameter positions to test jointly where positions refer to the order specified by `mod.model.data.param_names`
    
* joint_test: (str, default=`"f"`) Chooses the type of test between `"f"` and `"chisq"`


## Returns {.unnumbered}
(MarginaleffectsResult)
* DataFrame containing the results of the hypothesis tests.

## Examples {.unnumbered}
```py
from marginaleffects import *

import statsmodels.api as sm
import statsmodels.formula.api as smf
data = get_dataset("thornton")
model = smf.ols("outcome ~ distance + incentive", data=data).fit()

# When `hypothesis` is `None`, `hypotheses()` returns a DataFrame of parameters {.unnumbered}
hypotheses(model)

# A different null hypothesis {.unnumbered}
hypotheses(model, hypothesis = 3)

# Test of equality between coefficients {.unnumbered}
hypotheses(model, hypothesis="distance = incentive")

# Non-linear function {.unnumbered}
hypotheses(model, hypothesis="(distance + incentive) = 0.1")

# Robust standard errors {.unnumbered}
hypotheses(model, hypothesis="distance = incentive", vcov="HC3")

# Equivalence, non-inferiority, and non-superiority tests {.unnumbered}
hypotheses(model, equivalence=(0.0, 10.0))

# Joint hypothesis tests {.unnumbered}
hypotheses(model, joint=["distance", "incentive"])

# Joint hypothesis tests with a regular expression {.unnumbered}
hypotheses(model, joint="distance|incentive")

# Joint hypothesis tests with a regular expression {.unnumbered}
hypotheses(model, joint="i$") # matches `incentive` and `distance` columns
```
## Warnings {.unnumbered}
* Warning #1: Tests are conducted directly on the scale defined by the `type` argument. For some models, it can make sense to conduct hypothesis or equivalence tests on the `"link"` scale instead of the `"response"` scale which is often the default.
* Warning #2: For hypothesis tests on objects produced by the `marginaleffects` package, it is safer to use the `hypothesis` argument of the original function.
* Warning #3: The tests assume that the `hypothesis` expression is (approximately) normally distributed, which for non-linear functions of the parameters may not be realistic. More reliable confidence intervals can be obtained using the `inferences()` (in R only) function with `method = "boot"`

## Details {.unnumbered}

### Two-One-Sided Test (TOST) of Equivalence {.unnumbered}

The `equivalence` argument specifies the bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. The first element specifies the lower bound, and the second element specifies the upper bound. If `None`, equivalence tests are not performed.
