"""
Global settings for marginaleffects.
"""

import os
from typing import Optional

# Internal state
_settings = {
    "autodiff": None,  # None = auto-detect, True = force on, False = force off
}
_MISSING = object()


def autodiff(enabled: Optional[bool] = _MISSING) -> Optional[bool]:
    """
    Configure or inspect JAX-based automatic differentiation.

    Parameters
    ----------
    enabled : bool or None, optional
        - True: Force JAX usage (error if JAX not installed)
        - False: Disable JAX, always use finite differences
        - None: Reset to auto-detect (use JAX if available and model compatible)
        - Omitted: Return the current autodiff state without mutating it

    Returns
    -------
    bool or None
        The effective autodiff setting after applying the request.

    Examples
    --------
    >>> import marginaleffects as me
    >>> me.autodiff(False)   # Disable JAX
    >>> me.autodiff(True)    # Force JAX (raises if not installed)
    >>> me.autodiff(None)    # Reset to auto-detect
    >>> me.autodiff()        # Inspect current state
    """
    if enabled is _MISSING:
        return _get_autodiff()

    if enabled is not None and not isinstance(enabled, bool):
        raise TypeError("`enabled` must be True, False, None, or omitted")

    _settings["autodiff"] = enabled

    return _get_autodiff()


def set_autodiff(enabled: Optional[bool]) -> None:
    """
    Backwards-compatible wrapper around autodiff().
    """
    autodiff(enabled)


def get_autodiff() -> Optional[bool]:
    """
    Backwards-compatible accessor that returns autodiff() state.
    """
    return autodiff()


def _get_autodiff() -> Optional[bool]:
    """
    Internal helper returning the current autodiff setting without warnings.
    """
    # Programmatic setting takes precedence
    if _settings["autodiff"] is not None:
        return _settings["autodiff"]

    # Check environment variable
    env_val = os.environ.get("MARGINALEFFECTS_AUTODIFF", "").lower()
    if env_val in ("0", "false", "no", "off"):
        return False
    if env_val in ("1", "true", "yes", "on"):
        return True

    # Default: auto-detect (None)
    return None


def is_autodiff_enabled() -> bool:
    """
    Check if JAX autodiff should be attempted.

    Returns False if:
    - User explicitly disabled via autodiff(False) or env var
    - JAX is not installed and user didn't force it on

    Returns True if:
    - User explicitly enabled via autodiff(True)
    - Auto-detect mode and JAX is available
    """
    setting = _get_autodiff()

    if setting is False:
        return False

    if setting is True:
        # User wants JAX - check if available
        try:
            from .autodiff import _JAX_AVAILABLE

            if not _JAX_AVAILABLE:
                raise ImportError(
                    "JAX autodiff was explicitly enabled but JAX is not installed. "
                    "Install with: pip install marginaleffects[autodiff]"
                )
            return True
        except ImportError as e:
            raise ImportError(str(e)) from None

    # Auto-detect mode (setting is None)
    try:
        from .autodiff import _JAX_AVAILABLE

        return _JAX_AVAILABLE
    except ImportError:
        return False
