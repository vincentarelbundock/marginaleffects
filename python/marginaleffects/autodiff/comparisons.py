"""Comparison types and functions using enum-based approach for JAX compatibility."""

import jax.numpy as jnp
from jax import lax
from enum import IntEnum


class ComparisonType(IntEnum):
    """Comparison types for marginal effects."""

    DIFFERENCE = 0
    RATIO = 1
    LNRATIO = 2
    LNOR = 3
    LIFT = 4
    DIFFERENCEAVG = 5


def _compute_comparison_vector(
    comparison_type: int, pred_hi: jnp.ndarray, pred_lo: jnp.ndarray
) -> jnp.ndarray:
    """Apply comparison function element-wise (returns N-length array)."""
    return lax.switch(
        comparison_type,
        [
            lambda hi, lo: hi - lo,  # difference
            lambda hi, lo: hi / lo,  # ratio
            lambda hi, lo: jnp.log(hi / lo),  # lnratio
            lambda hi, lo: jnp.log((hi / (1 - hi)) / (lo / (1 - lo))),  # lnor
            lambda hi, lo: (hi - lo) / lo,  # lift
        ],
        pred_hi,
        pred_lo,
    )


def _compute_comparison_scalar(
    comparison_type: int, pred_hi: jnp.ndarray, pred_lo: jnp.ndarray
) -> jnp.ndarray:
    """Apply comparison function with aggregation (returns scalar).

    For non-linear comparisons (ratio, lnratio, lnor, lift), this averages
    the predictions first, then applies the comparison function.
    This matches the behavior of the 'avg' estimands in estimands.py.
    """
    hi_mean = jnp.mean(pred_hi)
    lo_mean = jnp.mean(pred_lo)
    return lax.switch(
        comparison_type,
        [
            lambda hi, lo: hi - lo,  # difference (differenceavg)
            lambda hi, lo: hi / lo,  # ratio (ratioavg)
            lambda hi, lo: jnp.log(hi / lo),  # lnratio (lnratioavg)
            lambda hi, lo: jnp.log((hi / (1 - hi)) / (lo / (1 - lo))),  # lnor (lnoravg)
            lambda hi, lo: (hi - lo) / lo,  # lift (liftavg)
        ],
        hi_mean,
        lo_mean,
    )


def _compute_comparison(
    comparison_type: int, pred_hi: jnp.ndarray, pred_lo: jnp.ndarray
) -> jnp.ndarray:
    """Apply comparison function element-wise (returns N-length array).

    Note: DIFFERENCEAVG should use _compute_comparison_scalar directly.
    """
    return _compute_comparison_vector(comparison_type, pred_hi, pred_lo)
