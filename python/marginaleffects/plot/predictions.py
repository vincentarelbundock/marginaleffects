from ..docstrings import doc
from .common import (
    dt_on_condition,
    plot_labels,
    plot_common,
    validate_plot_args,
    extract_var_list,
)
from ..predictions import predictions
from ..sanitize import sanitize_model
import copy


@doc("""
# `plot_predictions()`
{param_plot_intro_predictions}

## Parameters

{param_model}

{param_condition_predictions}

{param_by_plot_predictions}

{param_draw}

{param_newdata_plot_predictions}

{param_vcov}

{param_wts}

{param_transform}

{param_points}

{param_gray}

## Examples
```py
from marginaleffects import *

import statsmodels.api as sm
import statsmodels.formula.api as smf
data = get_dataset("thornton")

mod = smf.ols("outcome ~ incentive + distance", data).fit()

plot_predictions(mod, condition = ["distance", "incentive"])
```
""")
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
    model = sanitize_model(model)

    if points is None:
        points = 0
    try:
        points = float(points)
    except (TypeError, ValueError) as exc:
        raise TypeError("`points` must be a numeric value between 0 and 1.") from exc
    if points < 0 or points > 1:
        raise ValueError("`points` must be a numeric value between 0 and 1.")

    validate_plot_args(condition, by, newdata, wts)

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

    var_list = extract_var_list(condition, by)

    return plot_common(
        model,
        dt,
        model.response_name,
        var_list=var_list,
        gray=gray,
        points=points,
    )
