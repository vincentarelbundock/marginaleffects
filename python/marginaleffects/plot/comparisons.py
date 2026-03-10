from ..docs import doc
from ..comparisons import comparisons
from .common import dt_on_condition, plot_common
from ..sanitize_model import sanitize_model
from .common import plot_labels
import copy


@doc("""
# `plot_comparisons()`
{param_plot_intro_comparisons}

## Parameters

{param_model}

{param_variables_plot_contrast}

{param_newdata_plot_comparisons}

{param_condition_comparisons}

{param_by_plot_comparisons}

{param_wts}

{param_vcov}

{param_transform}

{param_draw}

{param_gray}""")
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

    assert not (not by and newdata is not None), (
        "The `newdata` argument requires a `by` argument."
    )

    assert (condition is None and by) or (condition is not None and not by), (
        "One of the `condition` and `by` arguments must be supplied, but not both."
    )

    assert not (wts is not None and not by), (
        "The `wts` argument requires a `by` argument."
    )

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

    if "contrast" in dt.columns and dt["contrast"].unique().len() > 1:
        var_list = var_list + ["contrast"]

    if not draw:
        return dt

    return plot_common(model, dt, "Comparison", var_list, gray=gray)
