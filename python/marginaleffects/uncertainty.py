"""Compatibility façade for inference helpers.

New internal code should import from :mod:`marginaleffects.inference`.
"""

from .inference import add_standard_errors, get_jacobian, get_se, get_z_p_ci

__all__ = ["add_standard_errors", "get_jacobian", "get_se", "get_z_p_ci"]
