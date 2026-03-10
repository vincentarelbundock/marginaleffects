import jax.numpy as jnp
import numpy as np
from jax import jacfwd, jacrev, jit
from .families import linkinv, resolve_link
from ..utils import group_reducer, standard_errors


def _predict_core(
    beta: jnp.ndarray,
    X: jnp.ndarray,
    family_type: int,
    link_type: int,
) -> jnp.ndarray:
    """Core prediction function - single source of truth for prediction computation."""
    linear_pred = X @ beta
    return linkinv(link_type, linear_pred)


@jit
def _predict(
    beta: jnp.ndarray, X: jnp.ndarray, family_type: int, link_type: int = None
) -> jnp.ndarray:
    return _predict_core(beta, X, family_type, link_type)


@jit
def _predict_byT(
    beta: jnp.ndarray, X: jnp.ndarray, family_type: int, link_type: int = None
) -> jnp.ndarray:
    pred = _predict_core(beta, X, family_type, link_type)
    return jnp.mean(pred)


def _predict_byG(
    beta: jnp.ndarray,
    X: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
    family_type: int,
    link_type: int = None,
) -> jnp.ndarray:
    pred = _predict_core(beta, X, family_type, link_type)
    return group_reducer(pred, groups, num_groups)


@jit
def _predictions_core(
    beta: jnp.ndarray, X: jnp.ndarray, family_type: int, link_type: int = None
) -> tuple[jnp.ndarray, jnp.ndarray]:
    pred = _predict_core(beta, X, family_type, link_type)
    jac = jacfwd(_predict, argnums=0)(beta, X, family_type, link_type)
    return pred, jac


def predictions(
    beta: jnp.ndarray,
    X: jnp.ndarray,
    vcov: jnp.ndarray,
    family_type: int,
    link_type: int = None,
) -> dict[str, np.ndarray]:
    link_type = resolve_link(family_type, link_type)
    pred, jac = _predictions_core(beta, X, family_type, link_type)
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(pred),
        "jacobian": np.array(jac),
        "std_error": se,
    }


@jit
def _predictions_byT_core(
    beta: jnp.ndarray, X: jnp.ndarray, family_type: int, link_type: int = None
) -> tuple[jnp.ndarray, jnp.ndarray]:
    pred = _predict_byT(beta, X, family_type, link_type)
    jac = jacrev(_predict_byT, argnums=0)(beta, X, family_type, link_type)
    return pred, jac


def predictions_byT(
    beta: jnp.ndarray,
    X: jnp.ndarray,
    vcov: jnp.ndarray,
    family_type: int,
    link_type: int = None,
) -> dict[str, np.ndarray]:
    link_type = resolve_link(family_type, link_type)
    pred, jac = _predictions_byT_core(beta, X, family_type, link_type)
    se = standard_errors(jac.reshape(1, -1), vcov)
    return {
        "estimate": np.array(pred),
        "jacobian": np.array(jac),
        "std_error": se[0],
    }


def _predictions_byG_core(
    beta: jnp.ndarray,
    X: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
    family_type: int,
    link_type: int = None,
) -> tuple[jnp.ndarray, jnp.ndarray]:
    pred = _predict_byG(beta, X, groups, num_groups, family_type, link_type)
    jac = jacrev(
        lambda b: _predict_byG(b, X, groups, num_groups, family_type, link_type)
    )(beta)
    return pred, jac


def predictions_byG(
    beta: jnp.ndarray,
    X: jnp.ndarray,
    vcov: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
    family_type: int,
    link_type: int = None,
) -> dict[str, np.ndarray]:
    link_type = resolve_link(family_type, link_type)
    pred, jac = _predictions_byG_core(
        beta, X, groups, num_groups, family_type, link_type
    )
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(pred),
        "jacobian": np.array(jac),
        "std_error": se,
    }
