from dataclasses import replace

import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf

from marginaleffects import get_dataset
from marginaleffects.autodiff.lower import lower_comparisons, lower_predictions
from marginaleffects.comparisons import (
    _build_comparison_frames,
    _collect_comparison_functions,
    _comparisons_build,
    _finalize_counterfactual_frames,
    _prepare_design_matrices,
)
from marginaleffects.predictions import _predictions_build
from marginaleffects.sanitize import sanitize_variables
from marginaleffects.utils import prepare_base_inputs


def _prediction_plan(model, *, by=False, hypothesis=None):
    model, by, _V, newdata, _hypothesis_null, _modeldata = prepare_base_inputs(
        model=model,
        vcov=True,
        by=by,
        newdata=None,
        wts=None,
        hypothesis=hypothesis,
    )
    exog = model.get_exog(newdata)
    _out, plan = _predictions_build(
        model=model,
        exog=exog,
        newdata=newdata,
        by=by,
        wts=None,
        hypothesis=hypothesis,
    )
    return model, plan


def _comparison_plan(
    model,
    *,
    variables="hp",
    comparison="difference",
    by=False,
    hypothesis=None,
):
    model, by, _V, newdata, _hypothesis_null, modeldata = prepare_base_inputs(
        model=model,
        vcov=True,
        by=by,
        newdata=None,
        wts=None,
        hypothesis=hypothesis,
        enforce_pyfixest_warning=False,
    )
    variables = sanitize_variables(
        variables=variables,
        model=model,
        newdata=newdata,
        comparison=comparison,
        eps=1e-4,
        by=by,
        wts=None,
        cross=False,
    )
    nd_frames, hi_frames, lo_frames = _build_comparison_frames(
        newdata, variables, cross=False
    )
    nd, hi, lo, pad_rows = _finalize_counterfactual_frames(
        nd_frames, hi_frames, lo_frames, [], modeldata
    )
    nd, hi, lo, nd_X, hi_X, lo_X = _prepare_design_matrices(model, nd, hi, lo, pad_rows)
    _out, plan = _comparisons_build(
        model=model,
        nd=nd,
        nd_X=nd_X,
        hi_X=hi_X,
        lo_X=lo_X,
        by=by,
        hypothesis=hypothesis,
        wts=None,
        eps=1e-4,
        comparison_functions=_collect_comparison_functions(variables),
    )
    return model, plan


def test_lower_predictions_serializes_aggregation_and_matrix_hypothesis():
    dat = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + wt", dat.to_pandas()).fit()
    hypothesis = np.ones((dat["cyl"].n_unique(), 1))
    model, plan = _prediction_plan(mod, by="cyl", hypothesis=hypothesis)

    lowered = lower_predictions(plan, model)

    assert lowered.ok
    assert lowered.kwargs["model_type"] == "linear"
    assert lowered.kwargs["X"].shape[1] == len(model.get_coef())
    assert lowered.kwargs["agg_num_segments"] == dat["cyl"].n_unique()
    assert lowered.kwargs["H"].shape == hypothesis.shape


def test_lower_comparisons_serializes_ratioavg_ops():
    dat = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + wt", dat.to_pandas()).fit()
    model, plan = _comparison_plan(
        mod,
        variables="hp",
        comparison="ratio",
        by="cyl",
    )

    lowered = lower_comparisons(plan, model)

    assert lowered.ok
    assert [op["op"] for op in lowered.kwargs["ops"]] == ["ratioavg"] * len(plan.groups)
    assert lowered.kwargs["X_hi"].shape[0] == sum(len(g.idx) for g in plan.groups)


def test_autodiff_comparison_registry_targets_pipeline_ops():
    from marginaleffects.autodiff.ops import COMPARISON_OPS, PIPELINE_OPS

    lowered_ops = {spec.pipeline_op for spec in COMPARISON_OPS.values()}

    assert lowered_ops == set(PIPELINE_OPS)


def test_lower_comparisons_rejects_missing_prediction_values():
    dat = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + wt", dat.to_pandas()).fit()
    model, plan = _comparison_plan(mod, variables="hp")
    plan = replace(plan, has_na=True)

    lowered = lower_comparisons(plan, model)

    assert not lowered.ok
    assert lowered.reason == "missing values in predictions"


def test_lower_comparisons_rejects_custom_callable():
    dat = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + wt", dat.to_pandas()).fit()
    model, plan = _comparison_plan(
        mod,
        variables="hp",
        comparison=lambda hi, lo: hi - lo,
        by=False,
    )

    lowered = lower_comparisons(plan, model)

    assert not lowered.ok
    assert lowered.reason == "custom comparison functions"


def test_lower_predictions_rejects_string_hypothesis():
    dat = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + wt", dat.to_pandas()).fit()
    model, plan = _prediction_plan(mod, by="cyl", hypothesis="b1 - b0 = 0")

    lowered = lower_predictions(plan, model)

    assert not lowered.ok
    assert lowered.reason == "this form of the `hypothesis` argument"


def test_get_autodiff_args_rejects_glm_offset_with_reason():
    dat = get_dataset("mtcars", "datasets").to_pandas()
    mod = sm.GLM.from_formula(
        "mpg ~ hp + wt",
        data=dat,
        family=sm.families.Gaussian(),
        offset=np.ones(dat.shape[0]),
    ).fit()
    model, _plan = _prediction_plan(mod, by=False)

    assert model.get_autodiff_args() == "models with offset or exposure"
