import sys
from importlib import import_module


def lazy_exports(package: str, exports: dict[str, tuple[str, str]]):
    """Build module-level ``__getattr__``/``__dir__`` for lazily imported names.

    ``exports`` maps each public name to ``(module, attribute)``; relative
    module names resolve against ``package``. The first access imports the
    module and caches the value in the package namespace.
    """
    namespace = sys.modules[package].__dict__

    def __getattr__(name):
        if name not in exports:
            raise AttributeError(f"module {package!r} has no attribute {name!r}")
        module_name, attribute = exports[name]
        value = getattr(import_module(module_name, package), attribute)
        namespace[name] = value
        return value

    def __dir__():
        return sorted(set(namespace) | set(exports))

    return __getattr__, __dir__
