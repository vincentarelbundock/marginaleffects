"""Composable JAX pipeline for R-side plan lowering."""

from __future__ import annotations

import numpy as np
import jax
import jax.numpy as jnp

from .glm.families import Family, Link, linkinv, resolve_link


_FAMILY = {
    "gaussian": Family.GAUSSIAN,
    "binomial": Family.BINOMIAL,
    "poisson": Family.POISSON,
    "gamma": Family.GAMMA,
    "Gamma": Family.GAMMA,
}

_LINK = {
    "identity": Link.IDENTITY,
    "log": Link.LOG,
    "logit": Link.LOGIT,
    "probit": Link.PROBIT,
    "inverse": Link.INVERSE,
    "sqrt": Link.SQRT,
    "cloglog": Link.CLOGLOG,
}


def _resolve_family_link(family: str | None, link: str | None) -> int | None:
    if family is None and link is None:
        return None
    try:
        family_type = _FAMILY[family]
        link_type = _LINK[link] if link is not None else None
    except KeyError as exc:
        raise ValueError(f"Unsupported GLM family/link: {family}/{link}") from exc
    return resolve_link(family_type, link_type)


def _wmean(x, w):
    if w is None:
        return jnp.mean(x)
    w = jnp.asarray(w, dtype=jnp.float64)
    return jnp.sum(x * w) / jnp.sum(w)


def _apply_agg(est, segments, num_segments, weights):
    if segments is None:
        return est
    segments = jnp.asarray(segments, dtype=jnp.int32)
    if weights is None:
        weights = jnp.ones_like(est, dtype=jnp.float64)
    else:
        weights = jnp.asarray(weights, dtype=jnp.float64)
    numer = jax.ops.segment_sum(est * weights, segments, num_segments=num_segments)
    denom = jax.ops.segment_sum(weights, segments, num_segments=num_segments)
    return numer / denom


def _comparison_estimates(mu_hi, mu_lo, ops):
    pieces = []
    start = 0
    for op in ops:
        n = int(op["n"])
        stop = start + n
        hi = mu_hi[start:stop]
        lo = mu_lo[start:stop]
        w = op.get("w")
        name = op["op"]
        if name == "difference":
            pieces.append(hi - lo)
        elif name == "ratio":
            pieces.append(hi / lo)
        elif name == "differenceavg":
            pieces.append(jnp.asarray([_wmean(hi, w) - _wmean(lo, w)]))
        elif name == "ratioavg":
            pieces.append(jnp.asarray([_wmean(hi, w) / _wmean(lo, w)]))
        else:
            raise ValueError(f"Unsupported comparison op: {name}")
        start = stop
    if not pieces:
        return jnp.asarray([], dtype=jnp.float64)
    return jnp.concatenate(pieces)


def compute(
    beta,
    model_type,
    family=None,
    link=None,
    X=None,
    X_hi=None,
    X_lo=None,
    ops=None,
    est_keep=None,
    agg_segments=None,
    agg_num_segments=None,
    agg_weights=None,
    H=None,
):
    """Return estimate and Jacobian for a lowered autodiff plan."""
    beta = jnp.asarray(beta, dtype=jnp.float64)
    link_type = _resolve_family_link(family, link) if model_type == "glm" else None

    if X is not None:
        X = jnp.asarray(X, dtype=jnp.float64)
    if X_hi is not None:
        X_hi = jnp.asarray(X_hi, dtype=jnp.float64)
    if X_lo is not None:
        X_lo = jnp.asarray(X_lo, dtype=jnp.float64)
    if est_keep is not None:
        est_keep = jnp.asarray(est_keep, dtype=jnp.int32)
    if H is not None:
        H = jnp.asarray(H, dtype=jnp.float64)

    def predict(x, b):
        eta = x @ b
        if model_type == "linear":
            return eta
        if model_type == "glm":
            return linkinv(link_type, eta)
        raise ValueError(f"Unsupported model_type: {model_type}")

    def f(b):
        if X is not None:
            est = predict(X, b)
        else:
            mu_hi = predict(X_hi, b)
            mu_lo = predict(X_lo, b)
            est = _comparison_estimates(mu_hi, mu_lo, ops or [])

        if est_keep is not None:
            est = est[est_keep]

        est = _apply_agg(est, agg_segments, agg_num_segments, agg_weights)

        if H is not None:
            est = est @ H

        return jnp.atleast_1d(est)

    estimate = f(beta)
    jac_fun = jax.jacfwd if beta.size <= estimate.size else jax.jacrev
    jacobian = jac_fun(f)(beta)
    jacobian = jnp.reshape(jacobian, (estimate.size, beta.size))
    return {
        "estimate": np.asarray(estimate, dtype=np.float64),
        "jacobian": np.asarray(jacobian, dtype=np.float64),
    }
