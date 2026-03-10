"""GLM families and link functions using enum-based approach for JAX compatibility."""

import jax.numpy as jnp
from jax import lax
from jax.scipy.stats import norm
from enum import IntEnum


class Family(IntEnum):
    """GLM family types."""

    GAUSSIAN = 0
    BINOMIAL = 1
    POISSON = 2
    GAMMA = 3
    INVERSE_GAUSSIAN = 4


class Link(IntEnum):
    """Link function types."""

    IDENTITY = 0
    LOG = 1
    LOGIT = 2
    PROBIT = 3
    INVERSE = 4
    SQRT = 5
    CLOGLOG = 6


# Default links for each family
DEFAULT_LINKS = {
    Family.GAUSSIAN: Link.IDENTITY,
    Family.BINOMIAL: Link.LOGIT,
    Family.POISSON: Link.LOG,
    Family.GAMMA: Link.INVERSE,
    Family.INVERSE_GAUSSIAN: Link.INVERSE,
}


def linkinv(link_type: int, eta: jnp.ndarray) -> jnp.ndarray:
    """Inverse link function: transform linear predictor to mean."""
    return lax.switch(
        link_type,
        [
            lambda x: x,  # identity
            lambda x: jnp.exp(x),  # log
            lambda x: 1 / (1 + jnp.exp(-x)),  # logit
            lambda x: norm.cdf(x),  # probit
            lambda x: 1.0 / x,  # inverse
            lambda x: x**2,  # sqrt
            lambda x: 1 - jnp.exp(-jnp.exp(x)),  # cloglog
        ],
        eta,
    )


def linkfun(link_type: int, mu: jnp.ndarray) -> jnp.ndarray:
    """Link function: transform mean to linear predictor."""
    return lax.switch(
        link_type,
        [
            lambda x: x,  # identity
            lambda x: jnp.log(x),  # log
            lambda x: jnp.log(x / (1 - x)),  # logit
            lambda x: norm.ppf(x),  # probit
            lambda x: 1.0 / x,  # inverse
            lambda x: jnp.sqrt(x),  # sqrt
            lambda x: jnp.log(-jnp.log(1 - x)),  # cloglog
        ],
        mu,
    )


# Valid link functions for each family
VALID_LINKS = {
    Family.GAUSSIAN: [Link.IDENTITY, Link.LOG, Link.INVERSE],
    Family.BINOMIAL: [Link.LOGIT, Link.PROBIT, Link.CLOGLOG, Link.LOG],
    Family.POISSON: [Link.LOG, Link.IDENTITY, Link.SQRT],
    Family.GAMMA: [Link.INVERSE, Link.IDENTITY, Link.LOG],
    Family.INVERSE_GAUSSIAN: [Link.INVERSE, Link.IDENTITY, Link.LOG],
}


def validate_family_link(family_type: int, link_type: int) -> bool:
    """Check if link function is valid for the given family."""
    if family_type not in VALID_LINKS:
        return False
    return link_type in VALID_LINKS[family_type]


def resolve_link(family_type: int, link_type: int = None) -> int:
    """Resolve link type, using default if None, and validate the combination."""
    if link_type is None:
        return DEFAULT_LINKS.get(family_type, Link.IDENTITY)
    if not validate_family_link(family_type, link_type):
        default = DEFAULT_LINKS.get(family_type, Link.IDENTITY)
        raise ValueError(
            f"Invalid link {link_type} for family {family_type}. "
            f"Valid links: {VALID_LINKS.get(family_type, [])}. "
            f"Using default: {default}"
        )
    return link_type


# Convenience instances for direct use
family = Family
link = Link
