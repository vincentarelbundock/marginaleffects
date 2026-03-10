import os
import inspect
import marginaleffects


class DocsParameters:
    docstring_slope = """
`slope`: (str) The type of slope or (semi-)elasticity to compute. Acceptable values are:

- "dydx": dY/dX
- "eyex": dY/dX * Y / X
- "eydx": dY/dX * Y
- "dyex": dY/dX / X
"""
    docstring_hypothesis = """
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
"""
    docstring_by = """
`by`: (bool, List[str], optional) A logical value or a list of column names in `newdata`. 

- `True`: estimate is aggregated across the whole dataset. 
- list: estimates are aggregated for each unique combination of values in the columns. 
"""
    docstring_conf_level = """
`conf_level`: (float, default=0.95) Numeric value specifying the confidence level for the confidence intervals.
"""

    def docstring_condition(x):
        return f"""
`condition`: (str, list, dictionary) Conditional {x}s.

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
"""

    docstring_cross = """
`cross`: `False` Contrasts represent the change in adjusted predictions when one predictor changes and all other variables are held constant. `True` Contrasts represent the changes in adjusted predictions when all the predictors specified in the variables argument are manipulated simultaneously (a "cross-contrast").
"""

    def docstring_variables_plot(x):
        return f"""
`variables`: (str, list, dictionary) Name of the variable whose {x} we want to plot on the y-axis. Refer to the `comparisons()` documentation.
"""

    def docstring_newdata_plot(x):
        return f"""
`newdata`: (dataframe) When newdata is `None`, the grid is determined by the condition argument. When newdata is not `None`, the argument behaves in the same way as in the {x}() function.
"""

    docstring_draw = """
`draw`: True returns a matplotlib plot. False returns a dataframe of the underlying data.
"""

    docstring_points = """
`points`: (float, default=0) Number between 0 and 1 which controls the transparency of raw data points. 0 (default) does not display any points.

Warning: The points displayed are raw data, so the resulting plot is not a "partial residual plot."
"""

    docstring_gray = """
`gray`: True returns a gray scale adapted plot. False returns a plot in color. For the second position of the list in the `condition` or `by` argument, the list can have at most 5 elements.
"""

    def docstring_by_plot(x):
        return f"""
`by`: (bool, str, list) Marginal {x}s. 

Names of the categorical predictors to marginalize across. Max length of list is 4, with position meanings:

1. x-axis.
2. color.
3. facet (wrap if no fourth variable, otherwise columns of grid).
4. facet (rows of grid)
"""

    docstring_wts = """
`wts`: (str, optional) Column name of weights to use for marginalization. Must be a column in `newdata`.
"""
    docstring_vcov = """
`vcov`: (bool, np.ndarray, default=True) Type of uncertainty estimates to report (e.g. for robust standard errors). Acceptable values are:

- `True`: Use the model's default covariance matrix.
- `False`: Do not compute standard errors.
- String: Literal indicating the kind of uncertainty estimates to return:
    - Heteroskedasticity-consistent: `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`.
- np.ndarray: A custom square covariance matrix.
"""
    docstring_equivalence = """
`equivalence`: (list, optional) List of 2 numeric float values specifying the bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. See the Details section below.
"""
    docstring_transform = """
`transform`: (function) Function specifying a transformation applied to unit-level estimates and confidence intervals just before the function returns results. Functions must accept a full column (series) of a Polars data frame and return a corresponding series of the same length. Ex:

- `transform = numpy.exp`
- `transform = lambda x: x.exp()`
- `transform = lambda x: x.map_elements()`
"""

    def docstring_newdata(x):
        return f"""
`newdata`: (None, DataFrame, str) Data frame or string specifying where statistics are evaluated in the predictor space.

- None: Compute {x}s at each observed value in the original dataset (empirical distribution)
- Dataframe: should be created with datagrid() function
- str:
    * "mean": Compute {x}s at the mean of the regressor
    * "median": Compute {x}s at the median of the regressor
    * "balanced": Compute {x}s on a balanced grid with every combination of categories and numeric variables held at their means.
    * "tukey": Probably NotImplemented
    * "grid": Probably NotImplemented
"""

    def docstring_plot_intro(x):
        return f"""
Plot {x} on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).

The `by` argument is used to plot marginal {x}, that is, {x} made on the original data, but averaged
by subgroups. This is analogous to using the `by` argument in the `{x}()` function.

The `condition` argument is used to plot conditional {x}, that is, {x} made on a user-specified grid.
This is analogous to using the `newdata` argument and `datagrid()` function in a `{x}()` call.

All unspecified variables are held at their mean or mode. This includes grouping variables in mixed-effects models, so analysts who fit such models may want to specify the groups of interest using the `variables` argument, or supply model-specific arguments to compute population-level estimates. See details below.

See the "Plots" vignette and website for tutorials and information on how to customize plots:

- https://marginaleffects.com/articles/plot.html
- https://marginaleffects.com

"""

    docstring_model = """
`model`: (model object) Object fitted using the `statsmodels` formula API.
"""
    docstring_eps_vcov = """
`eps_vcov`: (float) optional custom value for the finite difference approximation of the jacobian matrix. By default, the function uses the square root of the machine epsilon.
"""

    def docstring_variables(x):
        return f"""
`variables`: (str, list, dictionary) Specifies what variables (columns) to vary in order to make the {x}.

- `None`: {x}s are computed for all regressors in the model object (can be slow). Acceptable values depend on the variable type. See the examples below.
- List[str] or str: List of variable names to compute {x}s for.
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
"""

    docstring_eps = """
`eps`: (float, optional) step size to use when calculating numerical derivatives: (f(x+eps)-f(x))/eps. Default value is 1e-4 multiplied by the difference between the maximum and minimum values of the variable with respect to which we are taking the derivative. Changing eps may be necessary to avoid numerical problems in certain models.
"""


docstring_returns = """
## Returns

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

"""


class DocsModels:
    def docstring_notes(x):
        return f"""
## Notes

The fitted model includes additional attributes:

- `data`: The processed data after listwise deletion
- `formula`: The original formula string
- `formula_engine`: Set to "{x}"
- `model`: The fitted {x} model object
"""

    docstring_formula = """
`formula`: (str)
    Model formula

* Example: "outcome ~ distance + incentive"
"""

    docstring_categorical_requirement = """
**Important:** All categorical variables must be explicitly converted to `Categorical` or `Enum` dtype before fitting. String columns are not accepted in model formulas.

For Polars DataFrames:
```python
import polars as pl

# Option 1: Cast to Categorical (simplest)
df = df.with_columns(pl.col("region").cast(pl.Categorical))

# Option 2: Cast to Enum with explicit category order (recommended for control)
categories = ["<18", "18 to 35", ">35"]
df = df.with_columns(pl.col("age_group").cast(pl.Enum(categories)))
```

For pandas DataFrames:
```python
df["region"] = df["region"].astype("category")
```
"""
    docstring_kwargs_engine = """
`kwargs_engine`: (dict, default={}) Additional arguments passed to the model initialization.

* Example: `{'weights': weights_array}`
"""

    def docstring_fit_returns(x):
        return f"""
## Returns

(Model{x})
    A fitted model wrapped in the Model{x} class for compatibility
    with marginaleffects.
"""


class DocsDetails:
    docstring_tost = """
### Two-One-Sided Test (TOST) of Equivalence

The `equivalence` argument specifies the bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. The first element specifies the lower bound, and the second element specifies the upper bound. If `None`, equivalence tests are not performed.
"""

    docstring_order_of_operations = """
### Order of operations. 

Behind the scenes, the arguments of `marginaleffects` functions are evaluated in this order:

1. `newdata`
2. `variables`
3. `comparison` and `slope`
4. `by`
5. `vcov`
6. `hypothesis`
7. `transform`
"""


def docstrings_to_qmd(output_dir: str):
    """
    Loops over every name in marginaleffects.__all__ and writes the
    function's docstring (if it is indeed a function) to a .qmd file
    in the specified directory.

    Parameters
    ----------
    output_dir : str
        The directory to which the .qmd files will be saved.
    """
    os.makedirs(output_dir, exist_ok=True)

    for name in getattr(marginaleffects, "__all__", []):
        # Retrieve the object by name
        obj = getattr(marginaleffects, name, None)

        # Check if the object is a function
        if obj is not None and inspect.isfunction(obj):
            docstring = inspect.getdoc(obj) or ""

            # Construct the filepath as "output_dir/name.qmd"
            filepath = os.path.join(output_dir, f"{name}.qmd")

            # Write the docstring to the file
            with open(filepath, "w", encoding="utf-8") as f:
                f.write(docstring)
                print(f"File written: {f.name}")


if __name__ == "__main__":
    docstrings_to_qmd("qmd_files")
