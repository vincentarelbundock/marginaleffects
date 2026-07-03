from functools import partial

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


@partial(jit, static_argnames=("num_groups",))
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
    pred, jac = _predictions_byG_core(beta, X, groups, int(num_groups))
    se = standard_errors(jac, vcov)
    return {
        "estimate": np.array(pred),
        "jacobian": np.array(jac),
        "std_error": se,
    }
