from ..docstrings import doc
from ..sanitize import sanitize_model
from ..slopes import slopes
from .common import plot_estimates


@doc("""
# `plot_slopes()`
{param_plot_intro_slopes}

## Parameters

{param_model}

{param_variables_plot_slope}

{param_condition_slopes}

{param_by_plot_slopes}

{param_draw}

{param_newdata_plot_slopes}

{param_wts}

{param_vcov}

{param_gray}""")
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
    eps=None,
    eps_vcov=None,
    gray=False,
):
    model = sanitize_model(model)

    assert variables, "The `variables` argument must be supplied."

    def estimate(newdata):
        return slopes(
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

    return plot_estimates(
        model,
        condition,
        by,
        newdata,
        wts,
        draw,
        estimate,
        "Slope",
        gray=gray,
        facet_contrast=True,
    )
