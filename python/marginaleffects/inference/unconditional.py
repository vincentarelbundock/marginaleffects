"""Unconditional (influence-function) variance for averaged effects.

Standard delta-method errors treat the covariate distribution as fixed and
propagate only coefficient uncertainty. An average effect is also a sample
average, so it inherits sampling variation from the empirical distribution it
averages over. This module implements the two-part influence function

    IF_i(theta) = empirical_i(theta) + J_beta(theta) IF_i(beta)

and forms the covariance from the *sum*, which keeps the cross-covariance
between the two sources rather than discarding it.

The port follows the R implementation in `r/R/vcov_unconditional.R`. It fails
closed: anything it cannot linearize exactly raises instead of silently
returning a delta-method number under an unconditional label.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import polars as pl

SUPPORTED_TYPES = ("HC0", "HC1")

# Scalar contrasts with exact empirical influence functions. Everything else
# scalar falls back to delete-one jackknife pseudo-values.
KNOWN_SCALAR_KEYS = (
    "differenceavg",
    "ratioavg",
    "lnratioavg",
    "lnoravg",
    "liftavg",
)


@dataclass(frozen=True)
class VcovUnconditional:
    """A request for unconditional variance, passed as `vcov=`."""

    type: str = "HC0"
    cluster: str | None = None


def vcovUnconditional(type: str = "HC0", cluster: str | None = None):
    """EXPERIMENTAL: request unconditional variance in a marginaleffects call.

    Parameters
    ----------
    type : str
        Finite-sample adjustment, `"HC0"` (no model degrees-of-freedom factor)
        or `"HC1"` (the conventional `n / (n - k)` factor).
    cluster : str, optional
        Name of a column in the model data identifying clusters for one-way
        cluster-robust inference. As in `sandwich::vcovCL()`, clustered
        estimates carry the `G / (G - 1)` cluster-count adjustment.

    Returns
    -------
    VcovUnconditional
        Pass to the `vcov` argument of `avg_predictions()`,
        `avg_comparisons()`, or `avg_slopes()`.
    """
    if isinstance(type, str) and type.upper() in SUPPORTED_TYPES:
        type = type.upper()
    else:
        raise ValueError(
            f"`type` must be one of {', '.join(SUPPORTED_TYPES)}. "
            "HC2 through HC5 are not available because their regression-leverage "
            "adjustments are not defined for this combined influence function."
        )
    if cluster is not None:
        if not isinstance(cluster, str):
            raise ValueError(
                "`cluster` must be the name of a single column in the model data."
            )
        cluster = cluster.removeprefix("~").strip()
    return VcovUnconditional(type=type, cluster=cluster)


def is_unconditional(vcov) -> bool:
    """True for a `vcovUnconditional()` request, or the bare function itself."""
    return isinstance(vcov, VcovUnconditional) or vcov is vcovUnconditional


def as_unconditional(vcov) -> VcovUnconditional:
    return VcovUnconditional() if vcov is vcovUnconditional else vcov


# --- model-coefficient influence -------------------------------------------


def _score_obs(model, params):
    """Observation-level scores on the same scale as the model's covariance."""
    fitted = model.get_fitted_model()
    inner = getattr(fitted, "model", None)
    score_obs = getattr(inner, "score_obs", None)
    if callable(score_obs):
        try:
            out = np.asarray(score_obs(np.asarray(params, dtype=float)), dtype=float)
        except (ValueError, TypeError, AttributeError, NotImplementedError):
            out = None
        if out is not None and out.ndim == 2:
            return out

    # Linear models have no `score_obs`; their estimating function is the
    # residual-weighted design matrix, whose scale cancels against the
    # covariance it is multiplied by below.
    exog = getattr(inner, "exog", None)
    resid = getattr(fitted, "resid", None)
    if exog is None or resid is None:
        return None
    exog = np.asarray(exog, dtype=float)
    resid = np.asarray(resid, dtype=float).reshape(-1)
    if exog.ndim != 2 or exog.shape[0] != resid.shape[0]:
        return None
    scale = float(getattr(fitted, "scale", 1.0) or 1.0)
    return exog * (resid / scale)[:, None]


def beta_influence(model):
    """Observation-level influence function of the coefficients, IF_i(beta).

    Under the sandwich convention this is `scores @ bread`. Using the model's
    own non-robust covariance as the bread (times `n`) keeps whatever scale
    convention the fitting library chose, because the same scale appears in
    the scores and cancels.
    """
    params = np.asarray(model.get_coef(), dtype=float).reshape(-1)
    if not np.isfinite(params).all():
        raise ValueError(
            "`vcov=vcovUnconditional()` does not support models with aliased "
            "or missing coefficients."
        )
    scores = _score_obs(model, params)
    if scores is None:
        raise ValueError(
            "`vcov=vcovUnconditional()` could not extract observation-level "
            "scores for this model."
        )
    V0 = model.get_vcov(True)
    if V0 is None:
        raise ValueError(
            "`vcov=vcovUnconditional()` requires a model covariance matrix."
        )
    V0 = np.asarray(V0, dtype=float)
    if scores.shape[1] != V0.shape[0] or V0.shape[0] != V0.shape[1]:
        raise ValueError(
            "`vcov=vcovUnconditional()` could not align the model score matrix "
            f"({scores.shape[1]} columns) with its covariance "
            f"({V0.shape[0]} rows)."
        )
    n = scores.shape[0]
    return n * scores @ V0, scores.shape[1]


# --- empirical-distribution influence --------------------------------------


def _rowsum(values, group, n):
    """Accumulate contributions that map to the same original observation."""
    out = np.zeros(n, dtype=float)
    np.add.at(out, np.asarray(group, dtype=int), np.asarray(values, dtype=float))
    return out


def _known_scalar_phi(group, hi, lo, rowid, n):
    """Exact empirical influence for a built-in scalar average, else None."""
    key = group.fun_key
    if key is None:
        return None
    weighted = key.endswith("wts")
    base = key.removesuffix("wts") if weighted else key
    if base not in KNOWN_SCALAR_KEYS:
        return None

    idx = np.asarray(group.idx, dtype=int)
    h, lw = hi[idx], lo[idx]
    rid = np.asarray(rowid, dtype=int)[idx]
    w = np.ones(idx.size) if not weighted else np.asarray(group.w, dtype=float)
    if w.size != idx.size or not np.isfinite(w).all() or w.sum() == 0:
        return None

    mean_hi = float((w * h).sum() / w.sum())
    mean_lo = float((w * lw).sum() / w.sum())

    if base == "differenceavg":
        unit = (h - lw) - (mean_hi - mean_lo)
    elif base == "ratioavg":
        if mean_lo == 0:
            return None
        unit = (h - mean_hi) / mean_lo - mean_hi * (lw - mean_lo) / mean_lo**2
    elif base == "lnratioavg":
        ratio = mean_hi / mean_lo if mean_lo != 0 else np.nan
        if not np.isfinite(ratio) or ratio <= 0 or mean_hi == 0:
            return None
        unit = (h - mean_hi) / mean_hi - (lw - mean_lo) / mean_lo
    elif base == "lnoravg":
        if not (0 < mean_hi < 1 and 0 < mean_lo < 1):
            return None
        unit = (h - mean_hi) / (mean_hi * (1 - mean_hi)) - (lw - mean_lo) / (
            mean_lo * (1 - mean_lo)
        )
    else:  # liftavg
        if mean_lo == 0:
            return None
        unit = (h - mean_hi) / mean_lo - mean_hi * (lw - mean_lo) / mean_lo**2

    # For equal weights this multiplier is n/ng, matching 1/P(V = v) in the
    # subgroup influence function; with sampling weights it is n*w_i/sum(w).
    return _rowsum(n * w / w.sum() * unit, rid, n)


def _jackknife_scalar_phi(group, hi, lo, y, eps, rowid, n):
    """Delete-one pseudo-values for a scalar comparison with no closed form.

    Exact for group means and a first-order approximation otherwise, which is
    the only generic option for a user-supplied callable.
    """
    idx = np.asarray(group.idx, dtype=int)
    ng = idx.size
    out = np.zeros(n, dtype=float)
    if ng <= 1:
        return out

    x = None if group.x is None else np.asarray(group.x)
    w = None if group.w is None else np.asarray(group.w, dtype=float)
    theta_minus = np.empty(ng, dtype=float)
    for j in range(ng):
        keep = np.arange(ng) != j
        value = group.fun(
            hi=pl.Series("predicted_hi", hi[idx][keep]),
            lo=pl.Series("predicted_lo", lo[idx][keep]),
            eps=eps,
            x=None if x is None else pl.Series("x", x[keep]),
            y=None if y is None else pl.Series("predicted", y[idx][keep]),
            w=None if w is None else pl.Series("w", w[keep]),
        )
        value = np.asarray(value, dtype=float).reshape(-1)
        if value.size != 1 or not np.isfinite(value[0]):
            raise ValueError(
                "`vcov=vcovUnconditional()` could not linearize a scalar "
                "comparison function because a delete-one estimate was not finite."
            )
        theta_minus[j] = value[0]

    # Centre at the mean delete-one estimate: for a nonlinear statistic the
    # full-sample estimate need not equal that mean, and centring there would
    # leave influence values that do not sum to zero.
    centre = theta_minus.mean()
    rid = np.asarray(rowid, dtype=int)[idx]
    return _rowsum((n / ng) * (ng - 1) * (centre - theta_minus), rid, n)


def _aggregate_phi(agg, base_est, rowid, n, base_phi):
    """Push empirical influence through the recorded aggregation stage.

    For an unweighted subgroup the direct term is `n/m * (estimate_i - mean)`;
    `base_phi` carries influence a scalar comparison already produced through
    this second stage.
    """
    if agg is None:
        return base_phi

    out = np.zeros((n, agg.n), dtype=float)
    rowid = np.asarray(rowid)
    for block in agg.blocks:
        e = base_est[block.idx]
        rid = rowid[block.idx]
        w = block.w if agg.weighted else np.ones(block.idx.shape, dtype=float)
        for j, col in enumerate(block.cols):
            idx, ej, rj, wj = block.idx[:, j], e[:, j], rid[:, j], w[:, j]
            ok = ~np.isnan(ej) & ~np.isnan(wj) & (wj != 0)
            if not ok.any():
                continue
            # theta and alpha replay the displayed (possibly weighted) mean.
            theta = float((ej[ok] * wj[ok]).sum() / wj[ok].sum())
            alpha = np.zeros(ej.shape, dtype=float)
            alpha[ok] = wj[ok] / wj[ok].sum()
            out[:, col] += base_phi[:, idx] @ alpha

            ok_rowid = ok & (rj >= 0)
            if ok_rowid.any():
                contrib = n * wj[ok_rowid] / wj[ok].sum() * (ej[ok_rowid] - theta)
                out[:, col] += _rowsum(contrib, rj[ok_rowid], n)

    return np.nan_to_num(out, nan=0.0)


def _hypothesis_phi(phi, plan, theta):
    """Delta method through a post-aggregation hypothesis: IF(h) = H IF."""
    if plan.hyp is None:
        return phi
    from .delta import get_jacobian

    H = get_jacobian(plan.hyp.apply, np.asarray(theta, dtype=float))
    if H.shape[1] != phi.shape[1]:
        raise ValueError(
            "`vcov=vcovUnconditional()` could not differentiate the hypothesis "
            "stage: expected a Jacobian with "
            f"{phi.shape[1]} columns, got {H.shape[1]}."
        )
    return phi @ H.T


# --- validation ------------------------------------------------------------

SUPPORTED_MODEL_CLASSES = ("ModelStatsmodels",)


def validate_model(model):
    """Only explicitly validated model families are allowed, as in R."""
    name = type(model).__name__
    if name not in SUPPORTED_MODEL_CLASSES:
        raise ValueError(
            f"`vcov=vcovUnconditional()` is not currently supported for "
            f"models handled by `{name}`: only explicitly validated model "
            "classes are supported. Use a bootstrap method instead."
        )
    fitted = getattr(model.get_fitted_model(), "model", None)
    forbidden = ("PHReg", "Tobit", "SurvfuncRight")
    if type(fitted).__name__ in forbidden:
        raise ValueError(
            "`vcov=vcovUnconditional()` is not supported for censored or "
            f'survival models of class "{type(fitted).__name__}". These are '
            "outside the validated conditional-mean setup and can require "
            "additional nuisance-parameter influence functions. Use a "
            "bootstrap method instead."
        )


def validate_target(plan, kind):
    """Reject unit-level effects: the empirical influence is not identified."""
    scalar_comparison = kind == "comparisons" and any(
        group.scalar and np.asarray(group.idx).size > 1 for group in plan.groups
    )
    if plan.agg is not None or scalar_comparison:
        return
    if plan.hyp is not None:
        raise ValueError(
            "`vcov=vcovUnconditional()` does not support `hypothesis` applied "
            "directly to unit-level effects. Use an `avg_*()` function, a `by` "
            "argument, or a scalar comparison before `hypothesis`."
        )
    raise ValueError(
        "`vcov=vcovUnconditional()` is only supported for averaged or "
        "aggregated effects. Use an `avg_*()` function, a `by` argument, or a "
        "scalar comparison."
    )


IGNORED_MATCH_COLUMNS = (
    "rowid",
    "rowidcf",
    "rowid_dedup",
    "estimate",
    "predicted",
    "predicted_hi",
    "predicted_lo",
    "term",
    "contrast",
    "group",
    "by",
    "type",
    "marginaleffects_comparison",
)


def _match_columns(source, modeldata):
    return [
        column
        for column in source.columns
        if column in modeldata.columns and column not in IGNORED_MATCH_COLUMNS
    ]


def _columns_agree(source, modeldata, common, candidate) -> bool:
    """Do the evaluation rows really hold the model data at `candidate`?"""
    for column in common:
        left = source[column].to_numpy()
        right = modeldata[column].to_numpy()[candidate]
        if left.dtype.kind == "f" or right.dtype.kind == "f":
            if not np.allclose(left.astype(float), right.astype(float), equal_nan=True):
                return False
        elif not np.array_equal(left, right):
            return False
    return True


def _match_source(source, modeldata, common):
    """Recover the row mapping by matching evaluation rows to model-data rows."""
    if not common:
        return None
    # Counterfactual frames upcast the treatment column, so the join keys can
    # disagree on dtype with the model data they came from. Align them on the
    # model data's own types and drop any column that will not convert.
    casts, kept = [], []
    for column in common:
        target = modeldata[column].dtype
        if source[column].dtype == target:
            kept.append(column)
            continue
        try:
            source.select(pl.col(column).cast(target, strict=True))
        except pl.exceptions.PolarsError:
            continue
        casts.append(pl.col(column).cast(target, strict=True))
        kept.append(column)
    common = kept
    if not common:
        return None
    keys = modeldata.select(common)
    if keys.is_duplicated().any():
        # Ambiguous keys cannot identify a sampling unit.
        return None
    keys = keys.with_columns(
        pl.Series("_unconditional_rowid", range(keys.height), dtype=pl.Int64)
    )
    matched = (
        source.select(common).with_columns(casts).join(keys, on=common, how="left")
    )
    if matched.height != source.height:
        return None
    out = matched["_unconditional_rowid"]
    if out.null_count() > 0:
        return None
    return out.to_numpy().astype(int)


def resolve_rowid(plan, modeldata, n):
    """Map each estimand row to the original model-data row it came from.

    A recorded `rowid` is only trusted once the evaluation row is shown to
    actually hold that model-data row: `newdata` is sorted by the `by` columns
    before its ids are assigned, so the id is a position in the sorted frame,
    not in the model data. When it does not check out, the rows are matched on
    their shared columns instead, and anything still unresolved is an error --
    a wrong mapping would attribute influence to the wrong sampling unit.
    """
    source = plan.source
    if source is None:
        raise ValueError(
            "`vcov=vcovUnconditional()` requires effects evaluated over "
            "original model-data rows."
        )
    common = _match_columns(source, modeldata)

    candidate = plan.rowid
    if candidate is not None:
        candidate = np.asarray(candidate)
        if candidate.dtype.kind in "iu" and candidate.size == source.height:
            candidate = candidate.astype(int)
            in_range = (
                candidate.size > 0 and candidate.min() >= 0 and candidate.max() < n
            )
            if in_range and _columns_agree(source, modeldata, common, candidate):
                return candidate

    matched = _match_source(source, modeldata, common)
    if matched is not None:
        return matched

    raise ValueError(
        "`vcov=vcovUnconditional()` requires effects evaluated over original "
        "model-data rows, a subset of original rows with valid row IDs, or a "
        "full counterfactual grid that preserves `rowidcf`. Synthetic grids "
        'such as `newdata="mean"` are not supported.'
    )


def resolve_cluster(request, modeldata, n):
    if request.cluster is None:
        return None
    if request.cluster not in modeldata.columns:
        raise ValueError(
            f'Cluster variable "{request.cluster}" was not found in the model data.'
        )
    column = modeldata[request.cluster]
    if column.null_count() > 0:
        raise ValueError(
            f'Cluster variable "{request.cluster}" contains missing values.'
        )
    codes = (
        column.to_frame()
        .with_columns(
            pl.col(request.cluster).rank("dense").cast(pl.Int64).alias("_code")
        )["_code"]
        .to_numpy()
        - 1
    )
    if codes.shape[0] != n:
        raise ValueError(
            "`vcov=vcovUnconditional()` requires one cluster value per model-data row."
        )
    if np.unique(codes).size < 2:
        raise ValueError(
            "Cluster-robust unconditional variance requires at least two clusters."
        )
    return codes


# --- covariance assembly ---------------------------------------------------


def _correction(request, n, k, cluster):
    adjustment = 1.0
    if cluster is not None:
        G = int(np.unique(cluster).size)
        adjustment = G / (G - 1)
    if request.type == "HC0":
        return adjustment
    if n <= k:
        raise ValueError(
            "`vcov=vcovUnconditional()` requires more observations than model "
            'estimating functions for `type="HC1"`. Use '
            '`vcovUnconditional(type="HC0")` to omit the model '
            "degrees-of-freedom correction."
        )
    if cluster is not None:
        return adjustment * ((n - 1) / (n - k))
    return n / (n - k)


def unconditional_vcov(Phi, request, n, k, cluster):
    """Covariance of the sample estimator from its influence matrix."""
    correction = _correction(request, n, k, cluster)
    if cluster is not None:
        S = np.zeros((int(cluster.max()) + 1, Phi.shape[1]), dtype=float)
        np.add.at(S, cluster, Phi)
        return S.T @ S / n**2 * correction
    return Phi.T @ Phi / n**2 * correction


# --- entry point -----------------------------------------------------------


def _prediction_empirical(plan, model, rowid, n):
    from ..planning import prediction_plan_predict

    estimate = prediction_plan_predict(plan, model, model.get_coef())
    if plan.align is not None:
        aligned = np.full(plan.align.shape[0], np.nan, dtype=float)
        keep = plan.align >= 0
        aligned[keep] = estimate[plan.align[keep]]
        estimate = aligned
    base_phi = np.zeros((n, estimate.shape[0]), dtype=float)
    return estimate, base_phi


def _comparison_empirical(plan, model, rowid, n):
    """Comparison estimates at fixed beta, with per-estimand influence.

    Vector-valued comparisons stay at the unit level and are linearized later
    by the aggregation stage. Scalar comparisons have already collapsed several
    rows, so their influence must be built here while the rows are still known.
    """
    from ..planning import comparison_plan_predict

    hi, lo, y = comparison_plan_predict(plan, model, model.get_coef())

    def align(values):
        if values is None or plan.align is None:
            return values
        out = np.full(plan.align.shape[0], np.nan, dtype=float)
        keep = plan.align >= 0
        out[keep] = values[plan.align[keep]]
        return out

    hi, lo, y = align(hi), align(lo), align(y)

    est = np.zeros(plan.n_comp, dtype=float)
    est_rowid = np.full(plan.n_comp, -1, dtype=int)
    phi = np.zeros((n, plan.n_comp), dtype=float)

    from ..planning.core import _builtin_comparison, group_eps

    for group in plan.groups:
        idx = np.asarray(group.idx, dtype=int)
        yi = None if y is None else y[idx]
        eps = group_eps(plan, group)
        value = None
        if group.fun_key is not None:
            value = _builtin_comparison(
                group.fun_key, hi[idx], lo[idx], eps, group.x, yi, group.w
            )
        if value is None:
            value = group.fun(
                hi=pl.Series("predicted_hi", hi[idx]),
                lo=pl.Series("predicted_lo", lo[idx]),
                eps=eps,
                x=None if group.x is None else pl.Series("x", group.x),
                y=None if yi is None else pl.Series("predicted", yi),
                w=None if group.w is None else pl.Series("w", group.w),
            )
        value = np.asarray(value, dtype=float).reshape(-1)
        est[group.out_idx] = value

        if value.size == idx.size:
            est_rowid[group.out_idx] = np.asarray(rowid, dtype=int)[idx]
        elif value.size == 1 and idx.size > 1:
            if not np.isfinite(value[0]):
                raise ValueError(
                    "`vcov=vcovUnconditional()` requires scalar comparison "
                    "functions to return a finite value."
                )
            column = _known_scalar_phi(group, hi, lo, rowid, n)
            if column is None:
                column = _jackknife_scalar_phi(group, hi, lo, y, eps, rowid, n)
            phi[:, group.out_idx[0]] = column

    return est, est_rowid, phi


def unconditional_std_error(*, plan, model, kind, request, newdata, J, estimate):
    """Standard errors and covariance from the combined influence function.

    `J` is the derivative of the displayed estimand with respect to the model
    coefficients -- the same matrix the delta method uses. What differs is what
    it is combined with: the empirical influence of averaging over the observed
    covariates, added *before* the cross-product so their covariance survives.
    """
    request = as_unconditional(request)
    validate_model(model)
    validate_target(plan, kind)

    modeldata = model.get_modeldata()
    n = modeldata.height
    rowid = resolve_rowid(plan, modeldata, n)

    if kind == "predictions":
        base_est, base_phi = _prediction_empirical(plan, model, rowid, n)
        base_rowid = rowid
    else:
        base_est, base_rowid, base_phi = _comparison_empirical(plan, model, rowid, n)

    empirical = _aggregate_phi(plan.agg_blocks, base_est, base_rowid, n, base_phi)
    if plan.agg_blocks is not None:
        from ..planning import apply_plan_aggregation

        theta = apply_plan_aggregation(plan.agg_blocks, base_est)
    else:
        theta = base_est
    empirical = _hypothesis_phi(empirical, plan, theta)

    J = np.asarray(J, dtype=float)
    if empirical.shape[1] != J.shape[0]:
        raise ValueError(
            "Internal error: the empirical unconditional influence has "
            f"{empirical.shape[1]} columns, but the Jacobian has {J.shape[0]} rows."
        )

    beta_dot, score_dimension = beta_influence(model)
    if beta_dot.shape[0] != n:
        raise ValueError(
            "`vcov=vcovUnconditional()` requires one score row per model-data row."
        )

    # Sum before squaring: expanding this square keeps the two cross-covariance
    # terms that a component-wise variance would drop.
    Phi = empirical + beta_dot @ J.T
    cluster = resolve_cluster(request, modeldata, n)
    V = unconditional_vcov(Phi, request, n, score_dimension, cluster)

    se = np.sqrt(np.diag(V))
    se = np.where(se == 0, np.nan, se)
    if se.shape[0] != np.asarray(estimate).reshape(-1).shape[0]:
        raise ValueError(
            "Internal error: the unconditional covariance has "
            f"{se.shape[0]} rows but there are "
            f"{np.asarray(estimate).reshape(-1).shape[0]} estimates."
        )
    return se, V


def plan_jacobian(plan, model, kind, estimate, eps_vcov=None):
    """Derivative of the displayed estimand with respect to the coefficients.

    The unconditional path needs the Jacobian itself, not delta-method errors
    built from it, so `analytic_try` is asked for one against an identity
    covariance; only its `jacobian` is kept.
    """
    from .analytic import analytic_try
    from .delta import get_jacobian

    estimate = np.asarray(estimate, dtype=float).reshape(-1)
    if eps_vcov is None:
        k = np.asarray(model.get_coef(), dtype=float).reshape(-1).size
        found = analytic_try(
            plan=plan, model=model, V=np.eye(k), estimate=estimate, kind=kind
        )
        if found is not None:
            return found.jacobian, found.method

    if kind == "predictions":
        from ..planning import prediction_plan_apply, prediction_plan_predict

        def replay(beta):
            return prediction_plan_apply(
                plan, prediction_plan_predict(plan, model, beta)
            )
    else:
        from ..planning import comparison_plan_apply, comparison_plan_predict

        def replay(beta):
            hi, lo, y = comparison_plan_predict(plan, model, beta)
            return comparison_plan_apply(plan, hi, lo, y)

    return get_jacobian(replay, model.get_coef(), eps_vcov), "finite_difference"


def unconditional_result(*, plan, model, kind, request, newdata, out, eps_vcov=None):
    """Attach unconditional standard errors to an estimand table."""
    request = as_unconditional(request)
    # Validate before the Jacobian: an unsupported model should be reported as
    # such, not as whatever the derivative machinery trips over first.
    validate_model(model)
    validate_target(plan, kind)

    estimate = out["estimate"].to_numpy()
    J, method = plan_jacobian(plan, model, kind, estimate, eps_vcov)
    se, V = unconditional_std_error(
        plan=plan,
        model=model,
        kind=kind,
        request=request,
        newdata=newdata,
        J=J,
        estimate=estimate,
    )
    out = out.with_columns(pl.Series(se).alias("std_error"))
    label = "unconditional" if request.cluster is None else "unconditional_clustered"
    return out, J, f"{label}+{method}", V
