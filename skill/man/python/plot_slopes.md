# `plot_slopes()` {.unnumbered}

Plot slopes on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).

The `by` argument is used to plot marginal slopes, that is, slopes made on the original data, but averaged
by subgroups. This is analogous to using the `by` argument in the `slopes()` function.

The `condition` argument is used to plot conditional slopes, that is, slopes made on a user-specified grid.
This is analogous to using the `newdata` argument and `datagrid()` function in a `slopes()` call.

All unspecified variables are held at their mean or mode. This includes grouping variables in mixed-effects models, so analysts who fit such models may want to specify the groups of interest using the `variables` argument, or supply model-specific arguments to compute population-level estimates. See details below.

See the "Plots" vignette and website for tutorials and information on how to customize plots:

- https://marginaleffects.com/articles/plot.html
- https://marginaleffects.com


## Parameters {.unnumbered}

`model`: (model object) Object fitted using the `statsmodels` formula API.

`variables`: (str, list, dictionary) Name of the variable whose marginal effect (slope) we want to plot on the y-axis. Refer to the `comparisons()` documentation.

`condition`: (str, list, dictionary) Conditional slopess.

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

`by`: (bool, str, list) Marginal slopess. 

Names of the categorical predictors to marginalize across. Max length of list is 4, with position meanings:

1. x-axis.
2. color.
3. facet (wrap if no fourth variable, otherwise columns of grid).
4. facet (rows of grid)

`draw`: True returns a matplotlib plot. False returns a dataframe of the underlying data.

`newdata`: (dataframe) When newdata is `None`, the grid is determined by the condition argument. When newdata is not `None`, the argument behaves in the same way as in the slopes() function.

`wts`: (str, optional) Column name of weights to use for marginalization. Must be a column in `newdata`.

`vcov`: (bool, np.ndarray, default=True) Type of uncertainty estimates to report (e.g. for robust standard errors). Acceptable values are:

- `True`: Use the model's default covariance matrix.
- `False`: Do not compute standard errors.
- String: Literal indicating the kind of uncertainty estimates to return:
    - Heteroskedasticity-consistent: `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`.
- np.ndarray: A custom square covariance matrix.

`gray`: True returns a gray scale adapted plot. False returns a plot in color. For the second position of the list in the `condition` or `by` argument, the list can have at most 5 elements.
