import numpy as np
import polars as pl
import statsmodels.formula.api as smf

from marginaleffects import get_dataset
from marginaleffects.by import get_by
from marginaleffects.predictions import _predictions_build
from marginaleffects.test import get_hypothesis
from marginaleffects.plan import prediction_plan_apply, prediction_plan_predict
from marginaleffects.utils import prepare_base_inputs


def _prepared(model, *, by=False, wts=None, hypothesis=None):
    model, by, _V, newdata, _hypothesis_null, _modeldata = prepare_base_inputs(
        model=model,
        vcov=True,
        by=by,
        newdata=None,
        wts=wts,
        hypothesis=hypothesis,
    )
    return model, by, newdata, model.get_exog(newdata)


def _full_prediction_pipeline(model, exog, newdata, coefs, by, wts, hypothesis):
    out = model.get_predict(params=np.asarray(coefs), newdata=exog)
    if out.shape[0] == newdata.shape[0]:
        cols = [x for x in newdata.columns if x not in out.columns]
        out = out.hstack(newdata.select(cols))
    elif "group" in out.columns:
        meta = newdata.join(out.select("group").unique(), how="cross")
        cols = [x for x in meta.columns if x in out.columns]
        out = meta.join(out, on=cols, how="left")
    else:
        raise ValueError("bad prediction shape")
    out = get_by(model, out, newdata=newdata, by=by, wts=wts)
    return get_hypothesis(out, hypothesis=hypothesis, by=by)


def test_prediction_build_plan_replays_grouped_weighted_matrix_hypothesis():
    dat = (
        get_dataset("Guerry", "HistData")
        .drop_nulls()
        .with_columns(pl.col("Region").cast(pl.Categorical))
    )
    mod = smf.ols("Literacy ~ Pop1831 * Desertion", dat.to_pandas()).fit()
    model, by, newdata, exog = _prepared(
        mod,
        by="Region",
        wts="Donations",
        hypothesis=None,
    )
    hypothesis = np.eye(newdata["Region"].n_unique())[:, :2]

    out, plan = _predictions_build(
        model=model,
        exog=exog,
        newdata=newdata,
        by=by,
        wts="Donations",
        hypothesis=hypothesis,
    )

    replay = prediction_plan_apply(
        plan, prediction_plan_predict(plan, model, model.get_coef())
    )
    np.testing.assert_allclose(replay, out["estimate"].to_numpy(), rtol=1e-12)
    assert plan.agg is not None
    assert plan.hyp.kind == "matrix"


def test_prediction_plan_replay_matches_full_pipeline_after_perturbation():
    dat = get_dataset("mtcars", "datasets")
    mod = smf.ols("mpg ~ hp + cyl", dat.to_pandas()).fit()
    model, by, newdata, exog = _prepared(mod, by="cyl")

    _out, plan = _predictions_build(
        model=model,
        exog=exog,
        newdata=newdata,
        by=by,
        wts=None,
        hypothesis=None,
    )

    coefs = model.get_coef().copy()
    coefs[1] = coefs[1] + 1e-3
    replay = prediction_plan_apply(plan, prediction_plan_predict(plan, model, coefs))
    full = _full_prediction_pipeline(
        model=model,
        exog=exog,
        newdata=newdata,
        coefs=coefs,
        by=by,
        wts=None,
        hypothesis=None,
    )
    np.testing.assert_allclose(replay, full["estimate"].to_numpy(), rtol=1e-12)
