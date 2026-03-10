# `slopes()` {.unnumbered}

`slopes()` and `avg_slopes()` estimate unit-level (conditional) partial derivative of the regression equation with respect to a regressor of interest.
    
* `slopes()`: unit-level (conditional) estimates.
* `avg_slopes()`: average (marginal) estimates.

The newdata argument and the `datagrid()` function can be used to control where statistics are evaluated in the predictor space: "at observed values", "at the mean", "at representative values", etc.

See the package website and vignette for examples:

- https://marginaleffects.com/chapters/slopes.html
- https://marginaleffects.com

## Parameters {.unnumbered}

`model`: (model object) Object fitted using the `statsmodels` formula API.

`variables`: (str, list, dictionary) Specifies what variables (columns) to vary in order to make the slopes.

- str: Variable for which to compute the slopes for.
- NoneType: Slopes are computed for all regressors in the model object (can be slow)

`newdata`: (None, DataFrame, str) Data frame or string specifying where statistics are evaluated in the predictor space.

- None: Compute slopes at each observed value in the original dataset (empirical distribution)
- Dataframe: should be created with datagrid() function
- str:
    * "mean": Compute slopes at the mean of the regressor
    * "median": Compute slopes at the median of the regressor
    * "balanced": Compute slopes on a balanced grid with every combination of categories and numeric variables held at their means.
    * "tukey": Probably NotImplemented
    * "grid": Probably NotImplemented

`slope`: (str) The type of slope or (semi-)elasticity to compute. Acceptable values are:

- "dydx": dY/dX
- "eyex": dY/dX * Y / X
- "eydx": dY/dX * Y
- "dyex": dY/dX / X

`vcov`: (bool, np.ndarray, default=True) Type of uncertainty estimates to report (e.g. for robust standard errors). Acceptable values are:

- `True`: Use the model's default covariance matrix.
- `False`: Do not compute standard errors.
- String: Literal indicating the kind of uncertainty estimates to return:
    - Heteroskedasticity-consistent: `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`.
- np.ndarray: A custom square covariance matrix.

`conf_level`: (float, default=0.95) Numeric value specifying the confidence level for the confidence intervals.

`by`: (bool, List[str], optional) A logical value or a list of column names in `newdata`. 

- `True`: estimate is aggregated across the whole dataset. 
- list: estimates are aggregated for each unique combination of values in the columns. 

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

`equivalence`: (list, optional) List of 2 numeric float values specifying the bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. See the Details section below.

`wts`: (str, optional) Column name of weights to use for marginalization. Must be a column in `newdata`.

`eps`: (float, optional) step size to use when calculating numerical derivatives: (f(x+eps)-f(x))/eps. Default value is 1e-4 multiplied by the difference between the maximum and minimum values of the variable with respect to which we are taking the derivative. Changing eps may be necessary to avoid numerical problems in certain models.

`eps_vcov`: (float) optional custom value for the finite difference approximation of the jacobian matrix. By default, the function uses the square root of the machine epsilon.

## Returns {.unnumbered}

A Polars DataFrame with (some of) the following columns:

- `term`: the name of the variable.
- `contrast`: the comparison method used.
- `estimate`: the estimated contrast, difference, ratio, or other transformation between pairs of predictions.
- `std_error`: the standard error of the estimate.
- `statistic`: the test statistic (estimate / std.error).
- `p_value`: the p-value of the test.
- `s_value`: Shannon transform of the p value.
- `conf_low`: the lower confidence interval bound.
- `conf_high`: the upper confidence interval bound.
- `pred_low`: the lower prediction interval bound.
- `pred_high`: the upper prediction interval bound.

 
## Examples {.unnumbered}
```py
from marginaleffects import *
import statsmodels.api as sm
import statsmodels.formula.api as smf
import numpy as np

data = get_dataset("thornton")

# Create the squared distance term {.unnumbered}
data = data.with_columns(distance_sq = data['distance'] ** 2)

# Fit GLM with interactions and squared term {.unnumbered}
mod = smf.logit("outcome ~ incentive * distance * distance_sq", data=data).fit()

# Print summary {.unnumbered}
mod.summary()

# Slopes are computed for each regressor and at each observation {.unnumbered}
slopes(mod)

# Slopes are computed for each regressor and averaged over the observations {.unnumbered}
avg_slopes(mod)

# Slopes are computed with respect to the `distance` regressor at each observation {.unnumbered}
slopes(mod, variables = "distance")

# Slopes are computed with respect to the `distance` regressor and averaged over the observations {.unnumbered}
avg_slopes(mod, variables = "distance")
```

## Details {.unnumbered}

### Two-One-Sided Test (TOST) of Equivalence {.unnumbered}

The `equivalence` argument specifies the bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. The first element specifies the lower bound, and the second element specifies the upper bound. If `None`, equivalence tests are not performed.

### Order of operations.  {.unnumbered}

Behind the scenes, the arguments of `marginaleffects` functions are evaluated in this order:

1. `newdata`
2. `variables`
3. `comparison` and `slope`
4. `by`
5. `vcov`
6. `hypothesis`
7. `transform`
