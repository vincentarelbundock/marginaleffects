import formulaic
import numpy as np
import polars as pl
from formulaic import Formula


def reference_ratio_comparison(x):
    return (x / x[0])[1:]


def reference_ratio_label(x):
    return [f"({xi}) / ({x[0]})" for xi in x[1:]]


def reference_difference_comparison(x):
    return (x - x[0])[1:]


def reference_difference_label(x):
    return [f"({xi}) - ({x[0]})" for xi in x[1:]]


def revreference_ratio_comparison(x):
    return (x[0] / x)[1:]


def revreference_ratio_label(x):
    return [f"({x[0]}) / ({xi})" for xi in x[1:]]


def revreference_difference_comparison(x):
    return (x[0] - x)[1:]


def revreference_difference_label(x):
    return [f"({x[0]}) - ({xi})" for xi in x[1:]]


def sequential_ratio_comparison(x):
    shifted_x = np.roll(x, 1)
    return (x / shifted_x)[1:]


def sequential_ratio_label(x):
    shifted_x = np.roll(x, 1)
    return [f"({xi}) / ({shifted_xi})" for xi, shifted_xi in zip(x[1:], shifted_x[1:])]


def sequential_difference_comparison(x):
    shifted_x = np.roll(x, 1)
    return (x - shifted_x)[1:]


def sequential_difference_label(x):
    shifted_x = np.roll(x, 1)
    return [f"({xi}) - ({shifted_xi})" for xi, shifted_xi in zip(x[1:], shifted_x[1:])]


def revsequential_ratio_comparison(x):
    shifted_x = np.roll(x, 1)
    return (shifted_x / x)[1:]


def revsequential_ratio_label(x):
    shifted_x = np.roll(x, 1)
    return [f"({shifted_xi}) / ({xi})" for xi, shifted_xi in zip(x[1:], shifted_x[1:])]


def revsequential_difference_comparison(x):
    shifted_x = np.roll(x, 1)
    return (shifted_x - x)[1:]


def revsequential_difference_label(x):
    shifted_x = np.roll(x, 1)
    return [f"({shifted_xi}) - ({xi})" for xi, shifted_xi in zip(x[1:], shifted_x[1:])]


def _pair_index(n, reverse):
    """Index pairs ``(i, j)`` for the pairwise hypotheses, in R's order.

    R takes ``outer(x, x, op)`` and keeps its lower triangle (``pairwise``,
    ``i > j``) or upper triangle (``revpairwise``, ``i < j``) in column-major
    order: ``j`` is the outer loop and ``i`` the inner one.
    """
    pairs = [
        (i, j) for j in range(n) for i in (range(j) if reverse else range(j + 1, n))
    ]
    idx = np.array(pairs, dtype=int).reshape(-1, 2)
    return idx[:, 0], idx[:, 1]


def _pairwise_comparison(op, reverse):
    def comparison(x):
        x = np.asarray(x, dtype=float)
        i, j = _pair_index(len(x), reverse)
        return op(x[i], x[j])

    return comparison


def _pairwise_label(symbol, reverse):
    def label(x):
        x = list(x)
        i, j = _pair_index(len(x), reverse)
        return [f"({x[a]}) {symbol} ({x[b]})" for a, b in zip(i, j)]

    return label


pairwise_ratio_comparison = _pairwise_comparison(np.divide, reverse=False)
pairwise_ratio_label = _pairwise_label("/", reverse=False)
pairwise_difference_comparison = _pairwise_comparison(np.subtract, reverse=False)
pairwise_difference_label = _pairwise_label("-", reverse=False)
revpairwise_ratio_comparison = _pairwise_comparison(np.divide, reverse=True)
revpairwise_ratio_label = _pairwise_label("/", reverse=True)
revpairwise_difference_comparison = _pairwise_comparison(np.subtract, reverse=True)
revpairwise_difference_label = _pairwise_label("-", reverse=True)


# The shortcuts below mirror `hypothesis_formula_list` in the R package
# (r/R/hypothesis_formula.R); keep the two in sync.


def trt_vs_ctrl_ratio_comparison(x):
    x = np.asarray(x, dtype=float)
    return np.atleast_1d(np.mean(x[1:] / x[0]))


def trt_vs_ctrl_ratio_label(x):
    return ["Mean(Trt) / Ctrl"]


def trt_vs_ctrl_difference_comparison(x):
    x = np.asarray(x, dtype=float)
    return np.atleast_1d(np.mean(x[1:] - x[0]))


def trt_vs_ctrl_difference_label(x):
    return ["Mean(Trt) - Ctrl"]


def meandev_ratio_comparison(x):
    x = np.asarray(x, dtype=float)
    return x / np.mean(x)


def meandev_ratio_label(x):
    return [f"({xi}) / Mean" for xi in x]


def meandev_difference_comparison(x):
    x = np.asarray(x, dtype=float)
    return x - np.mean(x)


def meandev_difference_label(x):
    return [f"({xi}) - Mean" for xi in x]


def _mean_other(x):
    """Mean of every other element: (sum(x) - x_i) / (n - 1)."""
    return (np.sum(x) - x) / (len(x) - 1)


def meanotherdev_ratio_comparison(x):
    x = np.asarray(x, dtype=float)
    return x / _mean_other(x)


def meanotherdev_ratio_label(x):
    return [f"({xi}) / Mean (other)" for xi in x]


def meanotherdev_difference_comparison(x):
    x = np.asarray(x, dtype=float)
    return x - _mean_other(x)


def meanotherdev_difference_label(x):
    return [f"({xi}) - Mean (other)" for xi in x]


def contr_poly(n):
    """Orthogonal polynomial contrasts, equivalent to R's ``stats::contr.poly(n)``.

    Returns an ``n x (n - 1)`` matrix with orthonormal columns for the linear,
    quadratic, ... trends over equally spaced scores. As in R, column ``j`` is
    the QR basis vector times the sign of ``R[j, j]``, which makes the sign
    convention independent of the QR implementation.
    """
    if n < 2:
        raise ValueError("Polynomial contrasts require at least 2 estimates.")
    scores = np.arange(1, n + 1, dtype=float)
    X = (scores - scores.mean())[:, None] ** np.arange(n)
    Q, R = np.linalg.qr(X)
    return (Q * np.sign(np.diag(R)))[:, 1:]


def contr_helmert(n):
    """Helmert contrasts, equivalent to R's ``stats::contr.helmert(n)``.

    Column ``j`` (1-based) contrasts level ``j + 1`` with the first ``j`` levels:
    ``-1`` in rows ``1..j``, ``j`` in row ``j + 1``, and ``0`` below.
    """
    if n < 2:
        raise ValueError("Helmert contrasts require at least 2 estimates.")
    H = np.zeros((n, n - 1))
    for j in range(1, n):
        H[:j, j - 1] = -1.0
        H[j, j - 1] = j
    return H


_POLY_LABELS = ["Linear", "Quadratic", "Cubic", "Quartic", "Quintic"]


def poly_dotproduct_comparison(x):
    x = np.asarray(x, dtype=float)
    w = contr_poly(len(x))[:, : len(_POLY_LABELS)]
    return w.T @ x


def poly_dotproduct_label(x):
    return _POLY_LABELS[: min(len(_POLY_LABELS), len(x) - 1)]


def helmert_dotproduct_comparison(x):
    x = np.asarray(x, dtype=float)
    return x @ contr_helmert(len(x))


def helmert_dotproduct_label(x):
    return [f"Helmert {i}" for i in range(1, len(x))]


_RHS_DIFFERENCE_OR_RATIO = [
    "reference",
    "revreference",
    "sequential",
    "revsequential",
    "pairwise",
    "revpairwise",
    "meandev",
    "meanotherdev",
    "trt_vs_ctrl",
]
_RHS_DOTPRODUCT = ["poly", "helmert"]


def parse_hypothesis_formula(hypothesis):
    # Sanity checks
    assert isinstance(hypothesis, str), "Input must be a string."
    assert "~" in hypothesis, "Input must contain a '~' symbol."

    hypothesis = hypothesis.strip()
    # An empty left-hand side defaults to `difference`, or to `dotproduct` for
    # the contrast-weight shortcuts (decided once the right-hand side is known).
    lhs_empty = hypothesis.startswith("~")
    if lhs_empty:
        hypothesis = "difference" + hypothesis

    # Parse the formula using formulaic
    formula = Formula(hypothesis)

    # right-hand side. The shortcut name is read from the string because
    # formulaic treats some names (e.g. `poly`) as built-in transforms.
    rhs = hypothesis.split("~", 1)[1].split("|", 1)[0].strip()
    if isinstance(formula.rhs, formulaic.formula.SimpleFormula):
        by = None
    elif isinstance(formula.rhs, tuple):
        by = list(formula.rhs[1].required_variables)

    rhs_ok = _RHS_DIFFERENCE_OR_RATIO + _RHS_DOTPRODUCT
    msg = f"The right-hand side of the `hypothesis` formula must be element of: {', '.join(rhs_ok)}."
    assert rhs in rhs_ok, msg

    # left-hand side
    lhs = list(formula.lhs.required_variables)
    if rhs in _RHS_DOTPRODUCT:
        lhs_ok = ["dotproduct"]
        msg = "The left-hand side of the `hypothesis` formula must be empty or `dotproduct`."
        if lhs_empty:
            lhs = ["dotproduct"]
    else:
        lhs_ok = ["ratio", "difference"]
        msg = "The left-hand side of the `hypothesis` formula must be empty, `difference`, or `ratio`."
    assert len(lhs) == 1, msg
    lhs = lhs[0]
    assert lhs in lhs_ok, msg

    return lhs, rhs, by


def eval_hypothesis_formula(x, hypothesis, lab):
    lhs, rhs, by_hypothesis = parse_hypothesis_formula(hypothesis)
    # e.g. "ratio ~ pairwise" -> pairwise_ratio_comparison / pairwise_ratio_label
    hyp_fun = globals()[f"{rhs}_{lhs}_comparison"]
    lab_fun = globals()[f"{rhs}_{lhs}_label"]

    if not by_hypothesis:
        out = pl.DataFrame(
            {
                "term": lab_fun(lab),
                "estimate": hyp_fun(x["estimate"]),
            }
        )
    else:
        x = x.with_columns(pl.Series(lab).alias("term"))
        # maintain_order: groups come out in a fixed order, which the exact
        # Jacobian relies on (it re-evaluates this function and stacks results)
        out = x.group_by(*by_hypothesis, maintain_order=True).agg(
            pl.col("estimate").map_batches(hyp_fun),
            pl.col("term").map_batches(lambda x: pl.Series(lab_fun(x))),
        )
        out = out.explode(["estimate", "term"], empty_as_null=True)

    return out
