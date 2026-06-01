# `plot_predictions()` {.unnumbered}

Plot predictions on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).

The `by` argument is used to plot marginal predictions, that is, predictions made on the original data, but averaged
by subgroups. This is analogous to using the `by` argument in the `predictions()` function.

The `condition` argument is used to plot conditional predictions, that is, predictions made on a user-specified grid.
This is analogous to using the `newdata` argument and `datagrid()` function in a `predictions()` call.

All unspecified variables are held at their mean or mode. This includes grouping variables in mixed-effects models, so analysts who fit such models may want to specify the groups of interest using the `variables` argument, or supply model-specific arguments to compute population-level estimates. See details below.

See the "Plots" vignette and website for tutorials and information on how to customize plots:

- https://marginaleffects.com/articles/plot.html
- https://marginaleffects.com


## Parameters {.unnumbered}

`model`: (model object) Object fitted using the `statsmodels` formula API.

`condition`: (str, list, dictionary) Conditional predictionss.

- Position's representation:
    1. x-axis. 
    2. color.  
    3. facet (wrap if no fourth variable, otherwise cols of grid). 
    4. facet (rows of grid).
- Argument types:
    - list : Names of the predictors to display
        - Numeric variables in position 1 is summarized by 100 numbers
        - Numeric variables in positions 2, 3 and 4 are summarized by Tukey’s five numbers
    - dictionary : Keys correspond to predictors. Values can be one of the two below depending on predictor's type:
        - Series or list of the same type as the original variable.
        - Numeric variables:
            - String: "minmax", "threenum", "fivenum".
    - string : Same as list of length 1.

`by`: (bool, str, list) Marginal predictionss. 

Names of the categorical predictors to marginalize across. Max length of list is 4, with position meanings:

1. x-axis.
2. color.
3. facet (wrap if no fourth variable, otherwise columns of grid).
4. facet (rows of grid)

`draw`: True returns a matplotlib plot. False returns a dataframe of the underlying data.

`newdata`: (dataframe) When newdata is `None`, the grid is determined by the condition argument. When newdata is not `None`, the argument behaves in the same way as in the predictions() function.

`vcov`: (bool, np.ndarray, default=True) Type of uncertainty estimates to report (e.g. for robust standard errors). Acceptable values are:

- `True`: Use the model's default covariance matrix.
- `False`: Do not compute standard errors.
- String: Literal indicating the kind of uncertainty estimates to return:
    - Heteroskedasticity-consistent: `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`.
- np.ndarray: A custom square covariance matrix.

`wts`: (str, optional) Column name of weights to use for marginalization. Must be a column in `newdata`.

`transform`: (function) Function specifying a transformation applied to unit-level estimates and confidence intervals just before the function returns results. Functions must accept a full column (series) of a Polars data frame and return a corresponding series of the same length. Ex:

- `transform = numpy.exp`
- `transform = lambda x: x.exp()`
- `transform = lambda x: x.map_elements()`

`points`: (float, default=0) Number between 0 and 1 which controls the transparency of raw data points. 0 (default) does not display any points.

Warning: The points displayed are raw data, so the resulting plot is not a "partial residual plot."

`gray`: True returns a gray scale adapted plot. False returns a plot in color. For the second position of the list in the `condition` or `by` argument, the list can have at most 5 elements.

## Examples {.unnumbered}
```py
from marginaleffects import *

import statsmodels.api as sm
import statsmodels.formula.api as smf
data = get_dataset("thornton")

mod = smf.ols("outcome ~ incentive + distance", data).fit()

plot_predictions(mod, condition = ["distance", "incentive"])
```
