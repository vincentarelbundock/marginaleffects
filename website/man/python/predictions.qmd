# `predictions()` {.unnumbered}

`predictions()` and `avg_predictions()` predict outcomes using a fitted model on a specified scale for given combinations of values of predictor variables, such as their observed values, means, or factor levels (reference grid).
    
* `predictions()`: unit-level (conditional) estimates.
* `avg_predictions()`: average (marginal) estimates.

See the package website and vignette for examples:

- https://marginaleffects.com/chapters/predictions.html
- https://marginaleffects.com

## Parameters {.unnumbered}

`model`: (model object) Object fitted using the `statsmodels` formula API.

`variables`: (str, list, dictionary) Specifies what variables (columns) to vary in order to make the prediction.

- `None`: predictions are computed for all regressors in the model object (can be slow). Acceptable values depend on the variable type. See the examples below.
- List[str] or str: List of variable names to compute predictions for.
- Dictionary: keys identify the subset of variables of interest, and values define the type of contrast to compute. Acceptable values depend on the variable type:
    - Categorical variables:
        * "reference": Each factor level is compared to the factor reference (base) level
        * "all": All combinations of observed levels
        * "sequential": Each factor level is compared to the previous factor level
        * "pairwise": Each factor level is compared to all other levels
        * "minmax": The highest and lowest levels of a factor.
        * "revpairwise", "revreference", "revsequential": inverse of the corresponding hypotheses.
        * Vector of length 2 with the two values to compare.
    - Boolean variables:
        * `None`: contrast between True and False
    - Numeric variables:
        * Numeric of length 1: Contrast for a gap of `x`, computed at the observed value plus and minus `x / 2`. For example, estimating a `+1` contrast compares adjusted predictions when the regressor is equal to its observed value minus 0.5 and its observed value plus 0.5.
        * Numeric of length equal to the number of rows in `newdata`: Same as above, but the contrast can be customized for each row of `newdata`.
        * Numeric vector of length 2: Contrast between the 2nd element and the 1st element of the `x` vector.
        * Data frame with the same number of rows as `newdata`, with two columns of "low" and "high" values to compare.
        * Function which accepts a numeric vector and returns a data frame with two columns of "low" and "high" values to compare. See examples below.
        * "iqr": Contrast across the interquartile range of the regressor.
        * "sd": Contrast across one standard deviation around the regressor mean.
        * "2sd": Contrast across two standard deviations around the regressor mean.
        * "minmax": Contrast between the maximum and the minimum values of the regressor.
- Examples:
    + `variables = "gear" : "pairwise", "hp" : 10`
    + `variables = "gear" : "sequential", "hp" : [100, 120]`

`newdata`: (None, DataFrame, str) Data frame or string specifying where statistics are evaluated in the predictor space.

- None: Compute predictions at each observed value in the original dataset (empirical distribution)
- Dataframe: should be created with datagrid() function
- str:
    * "mean": Compute predictions at the mean of the regressor
    * "median": Compute predictions at the median of the regressor
    * "balanced": Compute predictions on a balanced grid with every combination of categories and numeric variables held at their means.
    * "tukey": Probably NotImplemented
    * "grid": Probably NotImplemented

`by`: (bool, List[str], optional) A logical value or a list of column names in `newdata`. 

- `True`: estimate is aggregated across the whole dataset. 
- list: estimates are aggregated for each unique combination of values in the columns. 

`transform`: (function) Function specifying a transformation applied to unit-level estimates and confidence intervals just before the function returns results. Functions must accept a full column (series) of a Polars data frame and return a corresponding series of the same length. Ex:

- `transform = numpy.exp`
- `transform = lambda x: x.exp()`
- `transform = lambda x: x.map_elements()`

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

`wts`: (str, optional) Column name of weights to use for marginalization. Must be a column in `newdata`.

`vcov`: (bool, np.ndarray, default=True) Type of uncertainty estimates to report (e.g. for robust standard errors). Acceptable values are:

- `True`: Use the model's default covariance matrix.
- `False`: Do not compute standard errors.
- String: Literal indicating the kind of uncertainty estimates to return:
    - Heteroskedasticity-consistent: `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`.
- np.ndarray: A custom square covariance matrix.

`equivalence`: (list, optional) List of 2 numeric float values specifying the bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. See the Details section below.

`conf_level`: (float, default=0.95) Numeric value specifying the confidence level for the confidence intervals.

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
data = get_dataset("thornton")

mod = smf.ols("outcome ~ incentive + distance", data).fit()

predictions(mod)

avg_predictions(mod)

predictions(mod, by = "village")

avg_predictions(mod, by = "village")

predictions(mod, hypothesis = 3)

avg_predictions(mod, hypothesis = 3)
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
