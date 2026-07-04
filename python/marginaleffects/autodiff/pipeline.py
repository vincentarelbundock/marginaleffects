"""Composable JAX pipeline for R-side plan lowering."""

from __future__ import annotations

from functools import partial

import numpy as np
import jax
import jax.numpy as jnp

from .glm.families import Family, Link, linkinv, resolve_link
from .ops import PIPELINE_OPS


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


def _asarray1(x):
    return jnp.asarray(x, dtype=jnp.float64)


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


def _comparison_estimates(mu_hi, mu_lo, ops_meta, ops_weights):
    pieces = []
    start = 0
    w_iter = iter(ops_weights)
    for name, n, has_w in ops_meta:
        stop = start + n
        hi = mu_hi[start:stop]
        lo = mu_lo[start:stop]
        w = next(w_iter) if has_w else None
        spec = PIPELINE_OPS[name]
        pieces.append(spec.estimate(hi, lo, w, _asarray1, _wmean))
        start = stop
    if not pieces:
        return jnp.asarray([], dtype=jnp.float64)
    return jnp.concatenate(pieces)


def _predict(model_type, link_type, x, b):
    eta = x @ b
    if model_type == "glm":
        return linkinv(link_type, eta)
    return eta


# The whole estimate+Jacobian computation is compiled with XLA. Traced arrays
# only affect the cache key through shape/dtype, so repeated calls at the same
# problem size recompile nothing; a new shape or plan structure pays one
# compilation.
@partial(
    jax.jit,
    static_argnames=(
        "model_type",
        "link_type",
        "ops_meta",
        "agg_num_segments",
        "use_fwd",
    ),
)
def _estimate_and_jacobian(
    beta,
    model_type,
    link_type,
    X,
    X_hi,
    X_lo,
    ops_meta,
    ops_weights,
    est_keep,
    agg_segments,
    agg_num_segments,
    agg_weights,
    H,
    use_fwd,
):
    def f(b):
        if X is not None:
            est = _predict(model_type, link_type, X, b)
        else:
            mu_hi = _predict(model_type, link_type, X_hi, b)
            mu_lo = _predict(model_type, link_type, X_lo, b)
            est = _comparison_estimates(mu_hi, mu_lo, ops_meta, ops_weights)

        if est_keep is not None:
            est = est[est_keep]

        est = _apply_agg(est, agg_segments, agg_num_segments, agg_weights)

        if H is not None:
            est = est @ H

        return jnp.atleast_1d(est)

    estimate = f(beta)
    jac_fun = jax.jacfwd if use_fwd else jax.jacrev
    jacobian = jac_fun(f)(beta)
    jacobian = jnp.reshape(jacobian, (estimate.size, beta.size))
    return estimate, jacobian


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
    if model_type not in ("linear", "glm"):
        raise ValueError(f"Unsupported model_type: {model_type}")
    link_type = _resolve_family_link(family, link) if model_type == "glm" else None

    if X is not None:
        X = jnp.asarray(X, dtype=jnp.float64)
    if X_hi is not None:
        X_hi = jnp.asarray(X_hi, dtype=jnp.float64)
    if X_lo is not None:
        X_lo = jnp.asarray(X_lo, dtype=jnp.float64)
    if est_keep is not None:
        est_keep = jnp.asarray(est_keep, dtype=jnp.int32)
    if agg_segments is not None:
        agg_segments = jnp.asarray(agg_segments, dtype=jnp.int32)
        agg_num_segments = int(agg_num_segments)
    else:
        agg_num_segments = None
    if agg_weights is not None:
        agg_weights = jnp.asarray(agg_weights, dtype=jnp.float64)
    if H is not None:
        H = jnp.asarray(H, dtype=jnp.float64)

    # Split ops into a hashable static structure (jit cache key) and traced
    # weight arrays, while tallying the pre-keep estimate length.
    ops_meta = ()
    ops_weights = ()
    if X is not None:
        n_est = X.shape[0]
    else:
        n_est = 0
        for op in ops or []:
            name = op["op"]
            if name not in PIPELINE_OPS:
                raise ValueError(f"Unsupported comparison op: {name}")
            spec = PIPELINE_OPS[name]
            n = int(op["n"])
            w = op.get("w")
            ops_meta = ops_meta + ((name, n, w is not None),)
            if w is not None:
                ops_weights = ops_weights + (jnp.asarray(w, dtype=jnp.float64),)
            n_est += spec.output_size(n)

    # Output size is known from shapes alone; it picks forward vs reverse mode
    # before tracing.
    n_out = n_est
    if est_keep is not None:
        n_out = est_keep.shape[0]
    if agg_segments is not None:
        n_out = agg_num_segments
    if H is not None:
        n_out = H.shape[1]
    use_fwd = beta.size <= max(n_out, 1)

    estimate, jacobian = _estimate_and_jacobian(
        beta,
        model_type,
        link_type,
        X,
        X_hi,
        X_lo,
        ops_meta,
        ops_weights,
        est_keep,
        agg_segments,
        agg_num_segments,
        agg_weights,
        H,
        use_fwd,
    )
    return {
        "estimate": np.asarray(estimate, dtype=np.float64),
        "jacobian": np.asarray(jacobian, dtype=np.float64),
    }
