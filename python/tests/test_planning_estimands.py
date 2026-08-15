"""Guard against drift between the numpy fast paths and `estimands`.

`planning.core._builtin_comparison` reimplements the comparison functions in
numpy so plan replay avoids polars overhead. The two definitions must agree, and
a divergence would surface as wrong estimates rather than an error.
"""

import numpy as np
import pytest

from marginaleffects.estimands import estimands
from marginaleffects.planning.core import _builtin_comparison, _series

EPS = 1e-4


def _inputs(seed=0, n=6):
    rng = np.random.default_rng(seed)
    return {
        "hi": rng.uniform(0.2, 0.8, n),
        "lo": rng.uniform(0.2, 0.8, n),
        "x": rng.uniform(1.0, 3.0, n),
        "y": rng.uniform(1.0, 3.0, n),
        "w": rng.uniform(0.5, 2.0, n),
    }


@pytest.mark.parametrize("key", sorted(estimands))
def test_builtin_comparison_matches_estimands(key):
    args = _inputs()
    fast = _builtin_comparison(key, eps=EPS, **args)
    if fast is None:
        # Unsupported keys must fall back to the canonical implementation.
        return
    reference = estimands[key](
        eps=EPS, **{name: _series(value, name) for name, value in args.items()}
    )
    reference = np.asarray(reference, dtype=float).reshape(-1)
    fast = np.asarray(fast, dtype=float).reshape(-1)
    assert fast.shape == reference.shape
    np.testing.assert_allclose(fast, reference, rtol=1e-12, atol=0)


def test_builtin_comparison_falls_back_on_unknown_key():
    assert _builtin_comparison("nonexistent", **_inputs(), eps=EPS) is None
