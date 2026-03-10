from ..docs import DocsParameters
from .common import dt_on_condition, plot_labels, plot_common
from ..predictions import predictions
from ..sanitize_model import sanitize_model
import copy


def plot_predictions(
    model,
    condition=None,
    by=False,
    newdata=None,
    vcov=True,
    conf_level=0.95,
    transform=None,
    draw=True,
    wts=None,
    gray=False,
    points=0,
):
    """
    Plot predictions on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).

    For more information, visit the website: https://marginaleffects.com/

    Or type: `help(plot_predictions)`
    """
    model = sanitize_model(model)

    if points is None:
        points = 0
    try:
        points = float(points)
    except (TypeError, ValueError) as exc:
        raise TypeError("`points` must be a numeric value between 0 and 1.") from exc
    if points < 0 or points > 1:
        raise ValueError("`points` must be a numeric value between 0 and 1.")

    assert not (not by and newdata is not None), (
        "The `newdata` argument requires a `by` argument."
    )

    assert not (wts is not None and not by), (
        "The `wts` argument requires a `by` argument."
    )

    assert not (condition is None and not by), (
        "One of the `condition` and `by` arguments must be supplied, but not both."
    )

    # before dt_on_condition, which modifies in-place
    condition_input = copy.deepcopy(condition)

    if condition is not None:
        newdata = dt_on_condition(model, condition)

    dt = predictions(
        model,
        by=by,
        newdata=newdata,
        conf_level=conf_level,
        vcov=vcov,
        transform=transform,
        wts=wts,
    )

    dt = plot_labels(model, dt, condition_input)

    if not draw:
        return dt

    if isinstance(condition, str):
        var_list = [condition]
    elif isinstance(condition, list):
        var_list = condition
    elif isinstance(condition, dict):
        var_list = list(condition.keys())
    elif isinstance(by, str):
        var_list = [by]
    elif isinstance(by, list):
        var_list = by
    elif isinstance(by, dict):
        var_list = list(by.keys())

    # not sure why these get appended
    var_list = [x for x in var_list if x not in ["newdata", "model"]]

    assert len(var_list) < 5, (
        "The `condition` and `by` arguments can have a max length of 4."
    )

    return plot_common(
        model,
        dt,
        model.response_name,
        var_list=var_list,
        gray=gray,
        points=points,
    )


plot_predictions.__doc__ = (
    """
# `plot_predictions()`
"""
    + DocsParameters.docstring_plot_intro("predictions")
    + """
## Parameters
"""
    + DocsParameters.docstring_model
    + DocsParameters.docstring_condition("predictions")
    + DocsParameters.docstring_by_plot("predictions")
    + DocsParameters.docstring_draw
    + DocsParameters.docstring_newdata_plot("predictions")
    + DocsParameters.docstring_vcov
    + DocsParameters.docstring_wts
    + DocsParameters.docstring_transform
    + DocsParameters.docstring_points
    + DocsParameters.docstring_gray
    + """
## Examples
```py
from marginaleffects import *

import statsmodels.api as sm
import statsmodels.formula.api as smf
data = get_dataset("thornton")

mod = smf.ols("outcome ~ incentive + distance", data).fit()

plot_predictions(mod, condition = ["distance", "incentive"])
```
"""
)
