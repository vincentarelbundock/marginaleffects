from .params import SHARED_PARAMS


def doc(docstring):
    """Decorator that sets a function's docstring by interpolating SHARED_PARAMS."""

    def decorator(func):
        func.__doc__ = docstring.format(**SHARED_PARAMS)
        return func

    return decorator
