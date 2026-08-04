"""Compatibility façade for estimation plans.

New internal code should import from :mod:`marginaleffects.planning`.
"""

from .planning.core import *  # noqa: F403
from .planning.core import _nan_weighted_mean  # noqa: F401
