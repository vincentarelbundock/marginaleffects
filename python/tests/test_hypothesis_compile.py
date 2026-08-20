import numpy as np
import polars as pl
from polars.testing import assert_frame_equal

import marginaleffects.hypothesis_compile as hypc
from marginaleffects.hypothesis_compile import hypothesis_compile
from marginaleffects.test.core import get_hypothesis


def test_group_term_indices_preserves_first_occurrence_order():
    group_terms = getattr(hypc, "_group_term_indices", None)
    assert group_terms is not None

    rowlabels, groups = group_terms(["b", "a", "b", "c", "a"])

    assert rowlabels == ["b", "a", "c"]
    assert groups == [[0, 2], [1, 4], [3]]


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

    assert hyp.kind == "matrix"
    np.testing.assert_array_equal(hyp.H, [[-0.5], [-0.5], [1.0]])
    assert_frame_equal(out, get_hypothesis(base, "b - a = 0"))
    np.testing.assert_allclose(hyp.apply(np.array([2.0, 4.0, 9.0])), [6.0])


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
    offset dwarfs the estimates: "b0 + 1e16 = 0" reported SE = 0.
    """
    import statsmodels.formula.api as smf

    from marginaleffects import avg_predictions, get_dataset

    base = pl.DataFrame({"term": ["a", "b"], "estimate": [1.0, 2.0]})
    _, hyp = hypothesis_compile(base, "b0 + 1e16 = 0")
    assert hyp.kind == "matrix"
    np.testing.assert_allclose(np.asarray(hyp.H).reshape(-1), [1.0, 0.0])
    assert float(np.atleast_1d(hyp.offset)[0]) == 1e16

    mtcars = get_dataset("mtcars", "datasets").to_pandas()
    mod = smf.ols("mpg ~ hp + wt", data=mtcars).fit()
    ref = avg_predictions(mod).data
    for hstring in ["b0 + 1e16 = 0", "b0 + 1e8 = 0", "b0 = 5"]:
        aff = avg_predictions(mod, hypothesis=hstring).data
        np.testing.assert_allclose(
            aff["std_error"].to_numpy(),
            ref["std_error"].to_numpy(),
            rtol=1e-12,
        )
