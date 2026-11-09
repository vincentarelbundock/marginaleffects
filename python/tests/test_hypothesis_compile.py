import numpy as np
import polars as pl
import pytest
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


def test_string_hypothesis_rejects_duplicate_terms():
    # As in R: term names are ambiguous when a term appears on several rows.
    base = pl.DataFrame({"term": ["a", "a", "b"], "estimate": [1.0, 3.0, 5.0]})
    with pytest.raises(ValueError, match="unique row identifiers"):
        hypothesis_compile(base, "b - a = 0")


def test_sequence_hypothesis_compiles_concatenated_replay():
    base = pl.DataFrame({"term": ["a", "b", "c"], "estimate": [1.0, 2.0, 4.0]})

    out, hyp = hypothesis_compile(base, ["b - a = 0", "c - b = 0"])

    assert hyp.kind == "matrix"
    np.testing.assert_array_equal(hyp.H, [[-1.0, 0.0], [1.0, -1.0], [0.0, 1.0]])
    assert_frame_equal(out, get_hypothesis(base, ["b - a = 0", "c - b = 0"]))
    np.testing.assert_allclose(hyp.apply(np.array([10.0, 13.0, 21.0])), [3.0, 8.0])


def test_affine_hypothesis_promotes_with_offset():
    """An affine map compiles to H plus an offset; the derivative is H alone.

    Differentiating the constant numerically cancels the probe step when the
    offset dwarfs the estimates: "b1 + 1e16 = 0" reported SE = 0.
    """
    import statsmodels.formula.api as smf

    from marginaleffects import avg_predictions, get_dataset

    base = pl.DataFrame({"term": ["a", "b"], "estimate": [1.0, 2.0]})
    _, hyp = hypothesis_compile(base, "b1 + 1e16 = 0")
    assert hyp.kind == "matrix"
    np.testing.assert_allclose(np.asarray(hyp.H).reshape(-1), [1.0, 0.0])
    assert float(np.atleast_1d(hyp.offset)[0]) == 1e16

    mtcars = get_dataset("mtcars", "datasets").to_pandas()
    mod = smf.ols("mpg ~ hp + wt", data=mtcars).fit()
    ref = avg_predictions(mod).data
    for hstring in ["b1 + 1e16 = 0", "b1 + 1e8 = 0", "b1 = 5"]:
        aff = avg_predictions(mod, hypothesis=hstring).data
        np.testing.assert_allclose(
            aff["std_error"].to_numpy(),
            ref["std_error"].to_numpy(),
            rtol=1e-12,
        )
