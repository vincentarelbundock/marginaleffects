"""Input sanitizers with lazy exports to keep dependency direction acyclic.

Every submodule is named after the noun it sanitizes (`by`, `vcov`,
`model`, ...), never after the exported function. A submodule sharing a
name with an export is a trap: importing it binds the *module* onto this
package, `__getattr__` then never fires for that name, and every later
`from .sanitize import <name>` silently receives a module instead of the
function. Keep the two namespaces disjoint.
"""

from importlib import import_module

_EXPORTS = {
    "handle_deprecated_hypotheses_argument": (
        ".deprecated",
        "handle_deprecated_hypotheses_argument",
    ),
    "handle_pyfixest_vcov_limitation": (
        ".deprecated",
        "handle_pyfixest_vcov_limitation",
    ),
    "HiLo": (".variables", "HiLo"),
    "by_frame_keys": (".by", "by_frame_keys"),
    "by_is_frame": (".by", "by_is_frame"),
    "sanitize_by": (".by", "sanitize_by"),
    "sanitize_comparison": (".comparison", "sanitize_comparison"),
    "sanitize_hypothesis_null": (".hypothesis_null", "sanitize_hypothesis_null"),
    "sanitize_model": (".model", "sanitize_model"),
    "sanitize_newdata": (".newdata", "sanitize_newdata"),
    "sanitize_variables": (".variables", "sanitize_variables"),
    "sanitize_vcov": (".vcov", "sanitize_vcov"),
}

__all__ = list(_EXPORTS)


def __getattr__(name):
    if name not in _EXPORTS:
        raise AttributeError(
            f"module 'marginaleffects.sanitize' has no attribute {name!r}"
        )
    module_name, attribute = _EXPORTS[name]
    value = getattr(import_module(module_name, __name__), attribute)
    globals()[name] = value
    return value


def __dir__():
    return sorted(set(globals()) | set(__all__))
