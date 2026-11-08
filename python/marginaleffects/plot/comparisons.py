from ..comparisons import comparisons
from ..docstrings import doc
from ..sanitize import sanitize_model
from .common import plot_estimates


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
    eps=None,
    gray=False,
):
    model = sanitize_model(model)

    def estimate(newdata):
        return comparisons(
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

    return plot_estimates(
        model,
        condition,
        by,
        newdata,
        wts,
        draw,
        estimate,
        "Comparison",
        gray=gray,
        facet_contrast=True,
    )
