import numpy as np
import statsmodels.formula.api as smf

from marginaleffects import get_dataset
from marginaleffects.comparisons import (
    _build_comparison_frames,
    _collect_comparison_functions,
    _comparisons_build,
    _compute_fd_estimates,
    _finalize_counterfactual_frames,
    _prepare_design_matrices,
)
from marginaleffects.plan import comparison_plan_apply, comparison_plan_predict
from marginaleffects.sanitize import sanitize_variables
from marginaleffects.utils import prepare_base_inputs


def _prepared(model, *, variables="hp", comparison="difference", by=False, wts=None):
    model, by, _V, newdata, _hypothesis_null, modeldata = prepare_base_inputs(
        model=model,
        vcov=True,
        by=by,
        newdata=None,
        wts=wts,
        hypothesis=None,
        enforce_pyfixest_warning=False,
    )
    variables = sanitize_variables(
        variables=variables,
        model=model,
        newdata=newdata,
        comparison=comparison,
        eps=1e-4,
        by=by,
        wts=wts,
        cross=False,
    )
    nd_frames, hi_frames, lo_frames = _build_comparison_frames(
        newdata, variables, cross=False
    )
    nd, hi, lo, pad_rows = _finalize_counterfactual_frames(
        nd_frames,
        hi_frames,
        lo_frames,
        [],
        modeldata,
    )
    nd, hi, lo, nd_X, hi_X, lo_X = _prepare_design_matrices(model, nd, hi, lo, pad_rows)
    return model, by, nd, nd_X, hi_X, lo_X, _collect_comparison_functions(variables)


def test_comparison_build_plan_replays_ratio_by_group():
    dat = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + wt", dat.to_pandas()).fit()
    model, by, nd, nd_X, hi_X, lo_X, comparison_functions = _prepared(
        mod,
        variables="hp",
        comparison="ratio",
        by="cyl",
    )

    out, plan = _comparisons_build(
        model=model,
        nd=nd,
        nd_X=nd_X,
        hi_X=hi_X,
        lo_X=lo_X,
        by=by,
        hypothesis=None,
        wts=None,
        eps=1e-4,
        comparison_functions=comparison_functions,
    )

    hi, lo, y = comparison_plan_predict(plan, model, model.get_coef())
    replay = comparison_plan_apply(plan, hi, lo, y)
    np.testing.assert_allclose(replay, out["estimate"].to_numpy(), rtol=1e-12)
    assert plan.groups
    assert all(group.fun_key == "ratioavg" for group in plan.groups)


def test_comparison_plan_replay_matches_full_pipeline_after_perturbation():
    dat = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + wt", dat.to_pandas()).fit()
    model, by, nd, nd_X, hi_X, lo_X, comparison_functions = _prepared(
        mod,
        variables="hp",
        comparison="difference",
        by=False,
    )

    _out, plan = _comparisons_build(
        model=model,
        nd=nd,
        nd_X=nd_X,
        hi_X=hi_X,
        lo_X=lo_X,
        by=by,
        hypothesis=None,
        wts=None,
        eps=1e-4,
        comparison_functions=comparison_functions,
    )

    coefs = model.get_coef().copy()
    coefs[1] = coefs[1] + 1e-3
    hi, lo, y = comparison_plan_predict(plan, model, coefs)
    replay = comparison_plan_apply(plan, hi, lo, y)
    full = _compute_fd_estimates(
        coefs,
        model=model,
        nd=nd,
        nd_X=nd_X,
        hi_X=hi_X,
        lo_X=lo_X,
        by=by,
        hypothesis=None,
        wts=None,
        eps=1e-4,
        comparison_functions=comparison_functions,
    )
    np.testing.assert_allclose(replay, full["estimate"].to_numpy(), rtol=1e-12)
