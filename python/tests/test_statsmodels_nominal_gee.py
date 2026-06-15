import numpy as np
import pandas as pd
import polars as pl
import pytest
import statsmodels.formula.api as smf

from marginaleffects import (
    avg_comparisons,
    avg_predictions,
    avg_slopes,
    predictions,
    slopes,
)


def _make_data(seed=42, n_groups=60, group_size=6):
    """Clustered data with a 3-category outcome that depends on x and z."""
    rng = np.random.default_rng(seed)
    rows = []
    for g in range(n_groups):
        x = rng.normal(size=group_size)
        z = rng.integers(0, 2, size=group_size)
        eta = np.column_stack([np.zeros(group_size), 0.9 * x, -0.7 * x + 0.6 * z])
        p = np.exp(eta)
        p /= p.sum(axis=1, keepdims=True)
        cat = np.array([rng.choice(3, p=p[i]) for i in range(group_size)])
        for i in range(group_size):
            rows.append({"cat": int(cat[i]), "x": x[i], "z": z[i], "gid": g})
    return pd.DataFrame(rows)


dat = _make_data()
mod = smf.nominal_gee("cat ~ x + z", "gid", dat).fit()
ncat = len(mod.model.endog_values)  # 3
ncut = mod.model.ncut  # 2


def test_predictions_match_statsmodels_fitted():
    # Predicted category probabilities must reproduce statsmodels' own fitted
    # per-cut means (its independent computation of the multinomial mean).
    pred = pl.DataFrame(predictions(mod))
    mat = (
        pred.select(["rowid", "group", "estimate"])
        .pivot(values="estimate", index="rowid", on="group")
        .sort("rowid")
    )
    me_cuts = mat.select([str(j) for j in range(ncut)]).to_numpy()
    sm_fitted = np.asarray(mod.fittedvalues).reshape(dat.shape[0], ncut)
    np.testing.assert_allclose(me_cuts, sm_fitted, atol=1e-8)

    # Probabilities over all categories sum to 1 for every row.
    all_probs = mat.select([str(j) for j in range(ncat)]).to_numpy()
    np.testing.assert_allclose(all_probs.sum(axis=1), 1.0, atol=1e-8)


def test_predictions_shape_and_groups():
    pred = pl.DataFrame(predictions(mod))
    assert pred.height == dat.shape[0] * ncat
    assert sorted(pred["group"].unique().to_list()) == [str(j) for j in range(ncat)]
    assert pred["estimate"].is_finite().all()


def test_avg_predictions_by_group():
    out = avg_predictions(mod, by="group")
    assert out.height == ncat
    # Average predicted probabilities over groups sum to 1.
    assert out["estimate"].sum() == pytest.approx(1.0, abs=1e-6)
    assert out["std_error"].is_finite().all()
    assert (out["std_error"] > 0).all()


def test_slopes_sum_to_zero_over_groups():
    # On the probability simplex the marginal effect of any predictor sums to
    # zero across categories; the delta-method SEs must be finite.
    out = avg_slopes(mod)
    assert out["std_error"].is_finite().all()
    by_term = out.group_by("term").agg(pl.col("estimate").sum())
    for s in by_term["estimate"].to_list():
        assert s == pytest.approx(0.0, abs=1e-6)


def test_avg_comparisons_runs():
    out = avg_comparisons(mod, variables="x")
    assert out.height == ncat
    assert out["estimate"].sum() == pytest.approx(0.0, abs=1e-6)
    assert out["std_error"].is_finite().all()


def test_unit_level_slopes_run():
    out = slopes(mod, variables="x")
    assert out.height == dat.shape[0] * ncat
    assert out["estimate"].is_finite().all()
