from functools import partial

import jax.numpy as jnp
import numpy as np
from jax import jit, jacrev
from ..comparisons import (
    _compute_comparison,
    _compute_comparison_scalar,
    ComparisonType,
)
from ..utils import group_reducer, jacobian, standard_errors


def _comparison_core(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    comparison_type: int,
) -> jnp.ndarray:
    """Core comparison function for linear models - single source of truth."""
    pred_hi = X_hi @ beta
    pred_lo = X_lo @ beta
    return _compute_comparison(comparison_type, pred_hi, pred_lo)


def _comparison_byG(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
    comparison_type: int,
) -> jnp.ndarray:
    pred_hi = group_reducer(X_hi @ beta, groups, num_groups)
    pred_lo = group_reducer(X_lo @ beta, groups, num_groups)
    return _compute_comparison(comparison_type, pred_hi, pred_lo)


@jit
def _comparisons_core(
    beta: jnp.ndarray, X_hi: jnp.ndarray, X_lo: jnp.ndarray, comparison_type: int
) -> tuple[jnp.ndarray, jnp.ndarray]:
    comp = _comparison_core(beta, X_hi, X_lo, comparison_type)

    def fun(b):
        return _comparison_core(b, X_hi, X_lo, comparison_type)

    jac = jacobian(fun, beta, comp)
    return comp, jac


def comparisons(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    vcov: jnp.ndarray,
    comparison_type: int,
) -> dict[str, np.ndarray]:
    # Handle DIFFERENCEAVG separately (returns scalar)
    if comparison_type == ComparisonType.DIFFERENCEAVG:

        @jit
        def _scalar_core(b, X_h, X_l):
            pred_hi = X_h @ b
            pred_lo = X_l @ b
            return _compute_comparison_scalar(0, pred_hi, pred_lo)

        comp = _scalar_core(beta, X_hi, X_lo)
        jac = jacrev(lambda b: _scalar_core(b, X_hi, X_lo))(beta)
        se = standard_errors(jac.reshape(1, -1), vcov)
        return {
            "estimate": np.array(comp),
            "jacobian": np.array(jac),
            "std_error": se[0],
        }

    # Handle element-wise comparisons
    comp, jac = _comparisons_core(beta, X_hi, X_lo, comparison_type)
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(comp),
        "jacobian": np.array(jac),
        "std_error": se,
    }


@partial(jit, static_argnames=("num_groups", "comparison_type"))
def _comparisons_byG_core(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
    comparison_type: int,
) -> tuple[jnp.ndarray, jnp.ndarray]:
    comp = _comparison_byG(beta, X_hi, X_lo, groups, num_groups, comparison_type)
    jac = jacrev(
        lambda b: _comparison_byG(b, X_hi, X_lo, groups, num_groups, comparison_type)
    )(beta)
    return comp, jac


def comparisons_byG(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    vcov: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
    comparison_type: int,
) -> dict[str, np.ndarray]:
    comparison_type = int(comparison_type)
    comp, jac = _comparisons_byG_core(
        beta, X_hi, X_lo, groups, int(num_groups), comparison_type
    )
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(comp),
        "jacobian": np.array(jac),
        "std_error": se,
    }
