import numpy as np
import polars as pl
import pytest
import statsmodels.formula.api as smf

from marginaleffects import avg_predictions
from marginaleffects.test.formula import contr_helmert, contr_poly

# Read the same vendored CSV as tests/r/test_hypothesis_shortcuts.R.
mtcars = pl.read_csv("tests/data/mtcars.csv", null_values="NA")
mod = smf.ols("mpg ~ hp + wt + am", data=mtcars.to_pandas()).fit()
fixture = pl.read_csv("tests/r/test_hypothesis_shortcuts.csv")


@pytest.mark.parametrize("spec", fixture["spec"].unique(maintain_order=True))
def test_shortcuts_match_r(spec):
    r = fixture.filter(pl.col("spec") == spec)
    grouped = "|" in spec
    by = ["am", "carb"] if grouped else "carb"
    py = avg_predictions(mod, by=by, hypothesis=spec)
    if grouped:
        py = py.sort("am", maintain_order=True)
    else:
        # Grouped labels also carry the `by` value, so compare labels only here.
        assert py["term"].to_list() == r["hypothesis"].to_list()
    np.testing.assert_allclose(py["estimate"], r["estimate"], rtol=1e-6)
    np.testing.assert_allclose(py["std_error"], r["std.error"], rtol=1e-5)


def test_poly_helmert_contrasts():
    # Values from R: stats::contr.poly(3) and stats::contr.helmert(4)
    np.testing.assert_allclose(
        contr_poly(3),
        [
            [-np.sqrt(0.5), np.sqrt(1 / 6)],
            [0, -np.sqrt(2 / 3)],
            [np.sqrt(0.5), np.sqrt(1 / 6)],
        ],
        atol=1e-12,
    )
    np.testing.assert_array_equal(
        contr_helmert(4), [[-1, -1, -1], [1, -1, -1], [0, 2, -1], [0, 0, 3]]
    )


@pytest.mark.parametrize(
    "spec, message",
    [
        ("difference ~ poly", "must be empty or `dotproduct`"),
        ("ratio ~ helmert", "must be empty or `dotproduct`"),
        ("dotproduct ~ meandev", "must be empty, `difference`, or `ratio`"),
        ("~ nonsense", "must be element of"),
    ],
)
def test_invalid_shortcut_formulas(spec, message):
    with pytest.raises(AssertionError, match=message):
        avg_predictions(mod, by="carb", hypothesis=spec)
