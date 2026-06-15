import numpy as np
import pandas as pd
import polars as pl
import pytest
import statsmodels.api as sm
import statsmodels.formula.api as smf

from marginaleffects import (
    avg_comparisons,
    avg_predictions,
    avg_slopes,
    predictions,
    slopes,
)


def _make_data(seed=0, n_groups=120, group_size=6):
    """Clustered data with an ordered 4-category outcome."""
    rng = np.random.default_rng(seed)
    thresholds = np.array([-1.0, 0.0, 1.0])
    rows = []
    for g in range(n_groups):
        x = rng.normal(size=group_size)
        z = rng.integers(0, 2, size=group_size)
        latent = 0.9 * x + 0.5 * z + rng.normal(size=group_size)
        cat = (latent[:, None] > thresholds[None, :]).sum(axis=1)
        for i in range(group_size):
            rows.append({"cat": int(cat[i]), "x": x[i], "z": z[i], "gid": g})
    return pd.DataFrame(rows)


dat = _make_data()
gor = sm.cov_struct.GlobalOddsRatio("ordinal")
# Ordinal GEE must be fit WITHOUT a model intercept: the cumulative-link
# thresholds already play that role (statsmodels' own example does this).
mod = smf.ordinal_gee("cat ~ 0 + x + z", "gid", dat, cov_struct=gor).fit()
ncat = len(mod.model.endog_values)  # 4
ncut = ncat - 1  # 3


def test_predictions_match_statsmodels_survival():
    # The reconstructed category probabilities must imply the same survival
    # probabilities P(Y > c_j) that statsmodels fits for each threshold.
    pred = pl.DataFrame(predictions(mod))
    mat = (
        pred.select(["rowid", "group", "estimate"])
        .pivot(values="estimate", index="rowid", on="group")
        .sort("rowid")
    )
    probs = mat.select([str(j) for j in range(ncat)]).to_numpy()
    # P(Y > c_j) = sum of probabilities for categories strictly above j.
    surv_me = np.cumsum(probs[:, ::-1], axis=1)[:, ::-1][:, 1:]
    sm_fitted = np.asarray(mod.fittedvalues).reshape(dat.shape[0], ncut)
    np.testing.assert_allclose(surv_me, sm_fitted, atol=1e-8)

    # Probabilities over all categories sum to 1 for every row.
    np.testing.assert_allclose(probs.sum(axis=1), 1.0, atol=1e-8)


def test_predictions_shape_and_groups():
    pred = pl.DataFrame(predictions(mod))
    assert pred.height == dat.shape[0] * ncat
    assert sorted(pred["group"].unique().to_list()) == [str(j) for j in range(ncat)]
    assert pred["estimate"].is_finite().all()


def test_avg_predictions_by_group():
    out = avg_predictions(mod, by="group")
    assert out.height == ncat
    assert out["estimate"].sum() == pytest.approx(1.0, abs=1e-6)
    assert out["std_error"].is_finite().all()
    assert (out["std_error"] > 0).all()


def test_slopes_sum_to_zero_over_groups():
    # The marginal effect of a predictor sums to zero across categories.
    out = avg_slopes(mod)
    assert out["std_error"].is_finite().all()
    by_term = out.group_by("term").agg(pl.col("estimate").sum())
    for s in by_term["estimate"].to_list():
        assert s == pytest.approx(0.0, abs=1e-6)


def test_slopes_are_ordinally_monotone():
    # A positive latent-scale effect of x should push probability mass from
    # the lowest category toward the highest one.
    out = avg_slopes(mod, variables="x").sort("group")
    est = out["estimate"].to_list()
    assert est[0] < 0  # P(lowest category) decreases
    assert est[-1] > 0  # P(highest category) increases


def test_avg_comparisons_runs():
    out = avg_comparisons(mod, variables="x")
    assert out.height == ncat
    assert out["estimate"].sum() == pytest.approx(0.0, abs=1e-6)
    assert out["std_error"].is_finite().all()


def test_unit_level_slopes_run():
    out = slopes(mod, variables="x")
    assert out.height == dat.shape[0] * ncat
    assert out["estimate"].is_finite().all()


def test_intercept_fit_raises_clear_error():
    # Fitting WITH an intercept is rank-deficient (collinear with thresholds);
    # marginaleffects must raise an explanatory error instead of returning
    # silently corrupted estimates from the ~1e15 coefficients.
    bad = smf.ordinal_gee("cat ~ x + z", "gid", dat, cov_struct=gor).fit()
    with pytest.raises(ValueError, match="intercept"):
        predictions(bad)
