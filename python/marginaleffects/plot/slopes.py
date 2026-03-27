from .common import (
    dt_on_condition,
    plot_labels,
    plot_common,
    validate_plot_args,
    extract_var_list,
)
from ..slopes import slopes
from ..sanitize import sanitize_model
import copy
from ..docstrings import doc


@doc("""
Plot slopes (partial derivatives) of the regression equation.

{param_plot_intro_slopes}

Parameters
----------
{param_model}
{param_variables_plot_slope}
{param_condition_slopes}
{param_by_plot_slopes}
{param_draw}
{param_newdata_plot_slopes}
{param_wts}
{param_vcov}
{param_gray}
""")
def plot_slopes(
    model,
    condition=None,
    variables=None,
    newdata=None,
    slope="dydx",
    vcov=True,
    conf_level=0.95,
    by=False,
    wts=None,
    draw=True,
    eps=1e-4,
    eps_vcov=None,
    gray=False,
):
    model = sanitize_model(model)

    assert variables, "The `variables` argument must be supplied."

    validate_plot_args(condition, by, newdata, wts)

    # before dt_on_condition, which modifies in-place
    condition_input = copy.deepcopy(condition)

    if condition is not None:
        newdata = dt_on_condition(model, condition)

    dt = slopes(
        model,
        variables=variables,
        newdata=newdata,
        slope=slope,
        vcov=vcov,
        conf_level=conf_level,
        by=by,
        wts=wts,
        eps=eps,
        eps_vcov=eps_vcov,
    )

    dt = plot_labels(model, dt, condition_input)

    if not draw:
        return dt

    var_list = extract_var_list(condition, by)

    if "contrast" in dt.columns and dt["contrast"].unique().len() > 1:
        var_list = var_list + ["contrast"]

    return plot_common(model, dt, "Slope", var_list, gray=gray)
