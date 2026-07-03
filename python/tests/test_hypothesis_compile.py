import numpy as np
import polars as pl
from polars.testing import assert_frame_equal

from marginaleffects.hypothesis_compile import hypothesis_compile
from marginaleffects.test.core import get_hypothesis


def test_matrix_hypothesis_compiles_replay_function():
    base = pl.DataFrame({"term": ["a", "b", "c"], "estimate": [1.0, 2.0, 4.0]})
    H = np.array([[1.0, 0.0], [-1.0, 1.0], [0.0, 2.0]])

    out, hyp = hypothesis_compile(base, H)

    assert hyp.kind == "matrix"
    np.testing.assert_allclose(hyp.H, H)
    assert_frame_equal(out, get_hypothesis(base, H))
    np.testing.assert_allclose(hyp.apply(np.array([2.0, 3.0, 5.0])), [-1.0, 13.0])


def test_string_hypothesis_compiles_duplicate_term_replay():
    base = pl.DataFrame(
        {"term": ["a", "a", "b"], "estimate": [1.0, 3.0, 5.0]},
    )

    out, hyp = hypothesis_compile(base, "b - a = 0")

    assert hyp.kind == "string"
    assert_frame_equal(out, get_hypothesis(base, "b - a = 0"))
    np.testing.assert_allclose(hyp.apply(np.array([2.0, 4.0, 9.0])), [6.0])


def test_sequence_hypothesis_compiles_concatenated_replay():
    base = pl.DataFrame({"term": ["a", "b", "c"], "estimate": [1.0, 2.0, 4.0]})

    out, hyp = hypothesis_compile(base, ["b - a = 0", "c - b = 0"])

    assert hyp.kind == "list"
    assert_frame_equal(out, get_hypothesis(base, ["b - a = 0", "c - b = 0"]))
    np.testing.assert_allclose(hyp.apply(np.array([10.0, 13.0, 21.0])), [3.0, 8.0])
