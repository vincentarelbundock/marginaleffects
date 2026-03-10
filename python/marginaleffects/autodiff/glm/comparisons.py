import jax.numpy as jnp
import numpy as np
from jax import jit, jacrev
from .families import linkinv, resolve_link
from ..comparisons import (
    _compute_comparison,
    _compute_comparison_scalar,
    ComparisonType,
)
from ..utils import group_reducer, standard_errors


def _comparison_core(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    comparison_type: int,
    family_type: int,
    link_type: int,
) -> jnp.ndarray:
    """Core comparison function - single source of truth for comparison vector computation."""
    pred_hi = linkinv(link_type, X_hi @ beta)
    pred_lo = linkinv(link_type, X_lo @ beta)
    return _compute_comparison(comparison_type, pred_hi, pred_lo)


@jit
def _comparison_byT(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    comparison_type: int,
    family_type: int,
    link_type: int = None,
) -> jnp.ndarray:
    """Averaged comparison: averages predictions first, then applies comparison."""
    pred_hi = linkinv(link_type, X_hi @ beta)
    pred_lo = linkinv(link_type, X_lo @ beta)
    return _compute_comparison_scalar(comparison_type, pred_hi, pred_lo)


def _comparison_byG(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
    comparison_type: int,
    family_type: int,
    link_type: int = None,
) -> jnp.ndarray:
    comp = _comparison_core(beta, X_hi, X_lo, comparison_type, family_type, link_type)
    return group_reducer(comp, groups, num_groups)


@jit
def _comparisons_core(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    comparison_type: int,
    family_type: int,
    link_type: int = None,
) -> tuple[jnp.ndarray, jnp.ndarray]:
    comp = _comparison_core(beta, X_hi, X_lo, comparison_type, family_type, link_type)
    jac = jacrev(
        lambda b: _comparison_core(
            b, X_hi, X_lo, comparison_type, family_type, link_type
        )
    )(beta)
    return comp, jac


def comparisons(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    vcov: jnp.ndarray,
    comparison_type: int,
    family_type: int,
    link_type: int = None,
) -> dict[str, np.ndarray]:
    link_type = resolve_link(family_type, link_type)

    # Handle DIFFERENCEAVG separately (returns scalar)
    if comparison_type == ComparisonType.DIFFERENCEAVG:

        @jit
        def _scalar_core(b, X_h, X_l, lt):
            pred_hi = linkinv(lt, X_h @ b)
            pred_lo = linkinv(lt, X_l @ b)
            return _compute_comparison_scalar(0, pred_hi, pred_lo)

        comp = _scalar_core(beta, X_hi, X_lo, link_type)
        jac = jacrev(lambda b: _scalar_core(b, X_hi, X_lo, link_type))(beta)
        se = standard_errors(jac.reshape(1, -1), vcov)
        return {
            "estimate": np.array(comp),
            "jacobian": np.array(jac),
            "std_error": se[0],
        }

    # Handle element-wise comparisons
    comp, jac = _comparisons_core(
        beta, X_hi, X_lo, comparison_type, family_type, link_type
    )
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(comp),
        "jacobian": np.array(jac),
        "std_error": se,
    }


@jit
def _comparisons_byT_core(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    comparison_type: int,
    family_type: int,
    link_type: int = None,
) -> tuple[jnp.ndarray, jnp.ndarray]:
    comp = _comparison_byT(beta, X_hi, X_lo, comparison_type, family_type, link_type)
    jac = jacrev(
        lambda b: _comparison_byT(
            b, X_hi, X_lo, comparison_type, family_type, link_type
        )
    )(beta)
    return comp, jac


def comparisons_byT(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    vcov: jnp.ndarray,
    comparison_type: int,
    family_type: int,
    link_type: int = None,
) -> dict[str, np.ndarray]:
    link_type = resolve_link(family_type, link_type)
    comp, jac = _comparisons_byT_core(
        beta, X_hi, X_lo, comparison_type, family_type, link_type
    )
    se = standard_errors(jac.reshape(1, -1), vcov)
    return {
        "estimate": np.array(comp),
        "jacobian": np.array(jac),
        "std_error": se[0],
    }


def _comparisons_byG_core(
    beta: jnp.ndarray,
    X_hi: jnp.ndarray,
    X_lo: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
    comparison_type: int,
    family_type: int,
    link_type: int = None,
) -> tuple[jnp.ndarray, jnp.ndarray]:
    comp = _comparison_byG(
        beta, X_hi, X_lo, groups, num_groups, comparison_type, family_type, link_type
    )
    jac = jacrev(
        lambda b: _comparison_byG(
            b, X_hi, X_lo, groups, num_groups, comparison_type, family_type, link_type
        )
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
    family_type: int,
    link_type: int = None,
) -> dict[str, np.ndarray]:
    link_type = resolve_link(family_type, link_type)
    comp, jac = _comparisons_byG_core(
        beta, X_hi, X_lo, groups, num_groups, comparison_type, family_type, link_type
    )
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(comp),
        "jacobian": np.array(jac),
        "std_error": se,
    }
