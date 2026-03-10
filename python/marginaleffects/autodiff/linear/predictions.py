import jax.numpy as jnp
import numpy as np
from jax import jit
from ..utils import group_reducer, standard_errors


@jit
def _predictions_core(
    beta: jnp.ndarray, X: jnp.ndarray
) -> tuple[jnp.ndarray, jnp.ndarray]:
    pred = X @ beta
    jac = X
    return pred, jac


def predictions(
    beta: jnp.ndarray, X: jnp.ndarray, vcov: jnp.ndarray
) -> dict[str, np.ndarray]:
    pred, jac = _predictions_core(beta, X)
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(pred),
        "jacobian": np.array(jac),
        "std_error": se,
    }


@jit
def _predictions_byT_core(
    beta: jnp.ndarray, X: jnp.ndarray
) -> tuple[jnp.ndarray, jnp.ndarray]:
    pred = jnp.mean(X @ beta)
    jac = jnp.mean(X, axis=0)
    return pred, jac


def predictions_byT(
    beta: jnp.ndarray, X: jnp.ndarray, vcov: jnp.ndarray
) -> dict[str, np.ndarray]:
    pred, jac = _predictions_byT_core(beta, X)
    se = standard_errors(jac.reshape(1, -1), vcov)
    return {
        "estimate": np.array(pred),
        "jacobian": np.array(jac),
        "std_error": se[0],
    }


def _predictions_byG_core(
    beta: jnp.ndarray, X: jnp.ndarray, groups: jnp.ndarray, num_groups: int
) -> tuple[jnp.ndarray, jnp.ndarray]:
    pred = X @ beta
    pred_byG = group_reducer(pred, groups, num_groups)
    jac_byG = group_reducer(X, groups, num_groups)
    return pred_byG, jac_byG


def predictions_byG(
    beta: jnp.ndarray,
    X: jnp.ndarray,
    vcov: jnp.ndarray,
    groups: jnp.ndarray,
    num_groups: int,
) -> dict[str, np.ndarray]:
    pred, jac = _predictions_byG_core(beta, X, groups, num_groups)
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(pred),
        "jacobian": np.array(jac),
        "std_error": se,
    }
