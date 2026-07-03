"""
JAX-based automatic differentiation module for marginal effects.

This module provides high-performance computation of predictions, comparisons,
and standard errors using JAX's automatic differentiation capabilities.

Note: This module requires JAX to be installed. Install with:
    pip install marginaleffects[autodiff]
    or
    pip install jax
"""

import importlib.util

_JAX_AVAILABLE = importlib.util.find_spec("jax") is not None


def _raise_jax_error():
    raise ImportError(
        "The autodiff module requires JAX to be installed. "
        "Install with: pip install marginaleffects[autodiff] or pip install jax"
    )


if _JAX_AVAILABLE:
    # Enable 64-bit precision in JAX by default (#1)
    # https://docs.jax.dev/en/latest/notebooks/Common_Gotchas_in_JAX.html#double-64bit-precision
    import jax

    jax.config.update("jax_enable_x64", True)

    from . import glm as glm
    from . import pipeline as pipeline

    from .glm.families import Family as Family, Link as Link

    __all__ = [
        "glm",
        "pipeline",
        "Family",
        "Link",
    ]
else:
    # Create dummy module objects that raise helpful errors
    class _DummyModule:
        def __getattr__(self, name):
            _raise_jax_error()

    glm = _DummyModule()
    pipeline = _DummyModule()

    class Family:
        def __getattribute__(self, name):
            _raise_jax_error()

    class Link:
        def __getattribute__(self, name):
            _raise_jax_error()

    __all__ = ["glm", "pipeline", "Family", "Link"]
