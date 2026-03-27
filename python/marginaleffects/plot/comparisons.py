from ..docstrings import doc
from ..comparisons import comparisons
from .common import (
    dt_on_condition,
    plot_labels,
    plot_common,
    validate_plot_args,
    extract_var_list,
)
from ..sanitize import sanitize_model
import copy


@doc("""
Plot comparisons between predictions.

{param_plot_intro_comparisons}

Parameters
----------
{param_model}
{param_variables_plot_contrast}
{param_newdata_plot_comparisons}
{param_condition_comparisons}
{param_by_plot_comparisons}
{param_wts}
{param_vcov}
{param_transform}
{param_draw}
{param_gray}
""")
def plot_comparisons(
    model,
    condition=None,
    variables=None,
    newdata=None,
    comparison="difference",
    vcov=True,
    conf_level=0.95,
    by=False,
    wts=None,
    draw=True,
    hypothesis=None,
    equivalence=None,
    transform=None,
    eps=1e-4,
    gray=False,
):
    model = sanitize_model(model)

    validate_plot_args(condition, by, newdata, wts)

    # before dt_on_condition, which modifies in-place
    condition_input = copy.deepcopy(condition)

    if condition is not None:
        newdata = dt_on_condition(model, condition)

    dt = comparisons(
        model,
        variables=variables,
        newdata=newdata,
        comparison=comparison,
        vcov=vcov,
        conf_level=conf_level,
        by=by,
        wts=wts,
        hypothesis=hypothesis,
        equivalence=equivalence,
        transform=transform,
        eps=eps,
    )

    dt = plot_labels(model, dt, condition_input)

    if not draw:
        return dt

    var_list = extract_var_list(condition, by)

    if "contrast" in dt.columns and dt["contrast"].unique().len() > 1:
        var_list = var_list + ["contrast"]

    return plot_common(model, dt, "Comparison", var_list, gray=gray)
