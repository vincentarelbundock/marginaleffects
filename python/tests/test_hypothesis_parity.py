"""Every kind of `hypothesis` against R: tests/r/test_hypothesis_parity.R.

Each case ID below matches one in the R script, which runs the same call on
the same vendored CSV. For each case we compare whether it errors, the output
columns (Python's snake_case statistics are a deliberate convention), the
labels, the `| am` groups, the estimates and the standard errors.

Positional indices are the one deliberate difference: R's start at b1, Python's
at b0. The positional cases call Python with every index one lower than in R
(R "b2 - b3 = 0" is Python "b1 - b2 = 0"), and their labels, which echo the
equation, are compared after the same shift.
"""

import re

import numpy as np
import polars as pl
import pytest
import statsmodels.api as sm
import statsmodels.formula.api as smf

from marginaleffects import (
    avg_comparisons,
    avg_predictions,
    avg_slopes,
    comparisons,
    datagrid,
    hypotheses,
)

dat = pl.read_csv("tests/data/mtcars.csv", null_values="NA")
m1 = smf.ols("mpg ~ hp + wt + am", data=dat.to_pandas()).fit()
m2 = smf.glm("am ~ hp + wt", data=dat.to_pandas(), family=sm.families.Binomial()).fit()

CASES = {}
for r in [
    "reference",
    "revreference",
    "sequential",
    "revsequential",
    "pairwise",
    "revpairwise",
    "meandev",
    "meanotherdev",
    "trt_vs_ctrl",
]:
    for lhs in ["difference", "ratio"]:
        CASES[f"F/{lhs}~{r}"] = lambda f=f"{lhs} ~ {r}": avg_predictions(
            m1, by="carb", hypothesis=f
        )
        CASES[f"G/{lhs}~{r}|am"] = lambda g=f"{lhs} ~ {r} | am": avg_predictions(
            m1, by=["am", "carb"], hypothesis=g
        )
for r in ["poly", "helmert"]:
    CASES[f"F/~{r}"] = lambda r=r: avg_predictions(m1, by="carb", hypothesis=f"~ {r}")
    CASES[f"G/~{r}|am"] = lambda r=r: avg_predictions(
        m1, by=["am", "carb"], hypothesis=f"~ {r} | am"
    )
CASES["L/multicol"] = lambda: avg_predictions(
    m1, by=["am", "gear"], hypothesis="~reference"
)
CASES["L/nolabel"] = lambda: comparisons(
    m1,
    variables="hp",
    newdata=datagrid(m1, am=[0, 1], wt=[2, 3]),
    hypothesis="~reference",
)
CASES["L/comparisons_by"] = lambda: avg_comparisons(
    m1, variables=["hp", "wt"], by="am", hypothesis="~reference"
)
CASES["L/slopes_pairwise"] = lambda: avg_slopes(
    m1, variables="hp", by="gear", hypothesis="~pairwise"
)
CASES["L/bare_string"] = lambda: avg_predictions(m1, by="carb", hypothesis="pairwise")
CASES["S/terms"] = lambda: hypotheses(m1, hypothesis="hp = wt")
CASES["S/positional"] = lambda: hypotheses(m1, hypothesis="b1 - b2 = 0")
CASES["S/nonlinear"] = lambda: hypotheses(m1, hypothesis="exp(b1) = 1")
CASES["S/wildcard"] = lambda: hypotheses(m1, hypothesis="b* = 0")
CASES["S/multiple"] = lambda: hypotheses(m1, hypothesis=["b1 = 0", "b2 = b3"])
CASES["S/preds_positional"] = lambda: avg_predictions(
    m1, by="carb", hypothesis="b0 = b1"
)
CASES["S/glm"] = lambda: avg_comparisons(m2, variables="hp", hypothesis="b0 = 0")
CASES["M/vector"] = lambda: hypotheses(m1, hypothesis=np.array([0, 1, -1, 0]))
CASES["M/matrix_nonames"] = lambda: avg_predictions(
    m1, by="gear", hypothesis=np.column_stack([[1, -1, 0], [1, 0, -1]])
)
CASES["M/matrix_names"] = lambda: avg_predictions(
    m1, by="gear", hypothesis=pl.DataFrame({"a": [1, -1, 0], "b": [1, 0, -1]})
)
CASES["N/null"] = lambda: avg_predictions(m1, by="gear", hypothesis=20)
CASES["X/function"] = lambda: avg_predictions(
    m1,
    by="gear",
    hypothesis=lambda x: pl.DataFrame(
        {
            "hypothesis": ["max - min"],
            "estimate": [x["estimate"].max() - x["estimate"].min()],
        }
    ),
)

# Cases whose labels contain positional indices (0-based here, 1-based in R).
POSITIONAL = {
    "S/positional",
    "S/nonlinear",
    "S/wildcard",
    "S/multiple",
    "S/preds_positional",
    "S/glm",
}


def r_label(case, label):
    if case not in POSITIONAL:
        return label
    return re.sub(r"\bb(\d+)\b", lambda m: f"b{int(m.group(1)) - 1}", label)


R_META = pl.read_csv("tests/r/test_hypothesis_parity_meta.csv")
R_ROWS = pl.read_csv("tests/r/test_hypothesis_parity_rows.csv", infer_schema_length=0)
SNAKE = {
    "std_error": "std.error",
    "p_value": "p.value",
    "s_value": "s.value",
    "conf_low": "conf.low",
    "conf_high": "conf.high",
}


def test_cases_match_r_script():
    assert sorted(CASES) == sorted(R_META["case"].to_list())


@pytest.mark.parametrize("case", sorted(CASES))
def test_hypothesis_matches_r(case):
    meta = R_META.filter(pl.col("case") == case).row(0, named=True)
    if meta["error"]:
        with pytest.raises((ValueError, AssertionError, TypeError)):
            CASES[case]()
        return

    py = CASES[case]().data
    r = R_ROWS.filter(pl.col("case") == case)

    # R adds a `df` column for some models; Python reports z statistics.
    r_cols = [c for c in meta["columns"].split("|") if c != "df"]
    assert [SNAKE.get(c, c) for c in py.columns] == r_cols

    labcol = next((c for c in ("hypothesis", "term") if c in py.columns), None)
    if labcol is not None:
        assert [str(v) for v in py[labcol]] == [
            r_label(case, lab) for lab in r["label"].to_list()
        ]
    if "am" in py.columns:
        assert [str(v) for v in py["am"]] == r["group"].to_list()

    np.testing.assert_allclose(
        py["estimate"], r["estimate"].cast(pl.Float64), rtol=1e-5, atol=1e-8
    )

    # An exactly-zero SE is reported as NaN on both sides, but either side may
    # land on floating-point noise (~1e-14) instead of an exact zero.
    def zero_as_nan(se):
        return np.where(np.abs(se) < 1e-12, np.nan, se)

    se_py = zero_as_nan(py["std_error"].to_numpy())
    se_r = zero_as_nan(r["std.error"].cast(pl.Float64).to_numpy())
    np.testing.assert_allclose(se_py, se_r, rtol=1e-5, atol=1e-8, equal_nan=True)


def test_positional_indices_are_zero_based():
    # b0 is the first estimate (Intercept), b3 the last of four.
    out = hypotheses(m1, hypothesis="b0 = 0")
    np.testing.assert_allclose(out["estimate"], m1.params.iloc[0])
    with pytest.raises(ValueError, match="b4 cannot be used"):
        hypotheses(m1, hypothesis="b4 = 0")


def test_fallback_labels_are_zero_based():
    # No column identifies the rows, so they are labelled b0, b1, ...
    nd = datagrid(m1, hp=[100, 150, 200])
    nd.datagrid_explicit = []
    out = comparisons(m1, variables="wt", newdata=nd, hypothesis="~reference")
    assert out["hypothesis"].to_list() == ["(b1) - (b0)", "(b2) - (b0)"]


def test_function_hypothesis_numeric_return_and_errors():
    # A numeric return is labelled with the `term` column when lengths match.
    out = hypotheses(m1, hypothesis=lambda x: x["estimate"] * 2)
    assert out["term"].to_list() == ["Intercept", "hp", "wt", "am"]
    np.testing.assert_allclose(out["estimate"], m1.params.to_numpy() * 2)
    with pytest.raises(ValueError, match="must accept an `x` argument"):
        hypotheses(m1, hypothesis=lambda y: y)
    with pytest.raises(ValueError, match="allowable arguments"):
        hypotheses(m1, hypothesis=lambda x, draws: x)


def test_weights_with_wrong_number_of_rows():
    with pytest.raises(ValueError, match="must have 4 rows"):
        hypotheses(m1, hypothesis=np.array([1, -1]))


@pytest.mark.parametrize("index", [None, ["x", "y", "z"]])
def test_pandas_weights_ignore_the_index(index):
    pd = pytest.importorskip("pandas")
    H = pd.DataFrame({"a": [1, -1, 0], "b": [1, 0, -1]}, index=index)
    out = avg_predictions(m1, by="gear", hypothesis=H)
    expected = avg_predictions(
        m1, by="gear", hypothesis=pl.DataFrame({"a": [1, -1, 0], "b": [1, 0, -1]})
    )
    assert out["term"].to_list() == ["a", "b"]
    np.testing.assert_allclose(out["estimate"], expected["estimate"])


def test_function_returning_pandas_frame_has_no_index_column():
    pd = pytest.importorskip("pandas")
    out = avg_predictions(
        m1,
        by="gear",
        hypothesis=lambda x: pd.DataFrame(
            {
                "hypothesis": ["range"],
                "estimate": [x["estimate"].max() - x["estimate"].min()],
            },
            index=[7],
        ),
    )
    assert "index" not in out.columns
    assert out["hypothesis"].to_list() == ["range"]


@pytest.mark.parametrize("expr", ["carb*2 = 0", "carb**2 = 0", "2*carb = 0"])
def test_wildcard_does_not_match_inside_names(expr):
    # `b*` ending a term name, or starting a power, is not the wildcard.
    m = smf.ols("mpg ~ carb", data=dat.to_pandas()).fit()
    out = hypotheses(m, hypothesis=expr)
    carb = m.params["carb"]
    expected = {"carb*2 = 0": carb * 2, "carb**2 = 0": carb**2, "2*carb = 0": 2 * carb}
    assert out.shape[0] == 1
    np.testing.assert_allclose(out["estimate"], expected[expr])
