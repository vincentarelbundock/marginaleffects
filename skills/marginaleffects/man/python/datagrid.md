# `datagrid()` {.unnumbered}

Generate a data grid of user-specified values for use in the 'newdata' argument of the 'predictions()', 'comparisons()', and 'slopes()' functions.

This is useful to define where in the predictor space we want to evaluate the quantities of interest. Ex: the predicted outcome or slope for a 37 year old college graduate.

## Parameters {.unnumbered}
* model: (object, optional)
    Model object.
    * (one and only one of the `model` and `newdata` arguments can be used.)
* newdata: (DataFrame, optional)
    Data frame used to define the predictor space.
    * (one and only one of the `model` and `newdata` arguments can be used.)
* grid_type: (str, optional)
    Determines the functions to apply to each variable. The defaults can be overridden by defining individual variables explicitly in the `**kwargs`, or by supplying a function to one of the `FUN_*` arguments.
    * "mean_or_mode": Character, factor, logical, and binary variables are set to their modes. Numeric, integer, and other variables are set to their means.
    * "balanced": Each unique level of character, factor, logical, and binary variables are preserved. Numeric, integer, and other variables are set to their means. Warning: When there are many variables and many levels per variable, a balanced grid can be very large. In those cases, it is better to use `grid_type="mean_or_mode"` and to specify the unique levels of a subset of named variables explicitly.
    * "counterfactual": the entire dataset is duplicated for each combination of the variable values specified in `**kwargs`. Variables not explicitly supplied to `datagrid()` are set to their observed values in the original dataset.
* FUN_numeric: (Callable, optional)
    The function to be applied to numeric variables.
* FUN_other: (Callable, optional)
    The function to be applied to other variable types.
* **kwargs
    * Named arguments where the name is the variable name and the value is a list of values to use in the grid. If a variable is not specified, it is set to its mean or mode depending on the `grid_type` argument.

## Returns {.unnumbered}
(polars.DataFrame)
* DataFrame where each row corresponds to one combination of the named predictors supplied by the user. Variables which are not explicitly defined are held at their mean or mode.

## Examples {.unnumbered}
```py
import polars as pl
import statsmodels.formula.api as smf
from marginaleffects import *
data = get_dataset("thornton")

# The output only has 2 rows, and all the variables except `hp` are at their mean or mode. {.unnumbered}
datagrid(newdata = data, village = [43, 11])

# We get the same result by feeding a model instead of a DataFrame {.unnumbered}
mod = smf.ols("outcome ~ incentive + distance", data).fit()
datagrid(model = mod, village = [43, 11])

# Use in `marginaleffects` to compute "Typical Marginal Effects". When used in `slopes()` or `predictions()` we do not need to specify the `model` or `newdata` arguments. {.unnumbered}
nd = datagrid(mod, village = [43, 11])
slopes(mod, newdata = nd)

# The full dataset is duplicated with each observation given counterfactual values of 43 and 11 for the `village` variable.  {.unnumbered}
# The original `thornton` includes 2884 rows, so the resulting dataset includes 5768 rows. {.unnumbered}
dg = datagrid(newdata = data, village = [43, 11], grid_type = "counterfactual")
dg.shape
```
