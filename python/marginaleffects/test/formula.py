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


def pairwise_ratio_comparison(x):
    x = x.to_numpy()
    out = np.tril(np.divide.outer(x, x), -1)
    return out[out != 0]


def pairwise_ratio_label(x):
    labels = [
        f"({xi}) / ({xj})" for i, xi in enumerate(x) for j, xj in enumerate(x) if i > j
    ]
    return labels


def pairwise_difference_comparison(x):
    x = x.to_numpy()
    out = np.tril(np.subtract.outer(x, x), -1)
    return out[out != 0]


def pairwise_difference_label(x):
    labels = [
        f"({xi}) - ({xj})" for i, xi in enumerate(x) for j, xj in enumerate(x) if i > j
    ]
    return labels


def revpairwise_ratio_comparison(x):
    x = x.to_numpy()
    out = np.triu(np.divide.outer(x, x), 1)
    return out[out != 0]


def revpairwise_ratio_label(x):
    labels = [
        f"({xi}) / ({xj})" for i, xi in enumerate(x) for j, xj in enumerate(x) if i < j
    ]
    return labels


def revpairwise_difference_comparison(x):
    x = x.to_numpy()
    out = np.triu(np.subtract.outer(x, x), 1)
    return out[out != 0]


def revpairwise_difference_label(x):
    labels = [
        f"({xi}) - ({xj})" for i, xi in enumerate(x) for j, xj in enumerate(x) if i < j
    ]
    return labels


def parse_hypothesis_formula(hypothesis):
    # Sanity checks
    assert isinstance(hypothesis, str), "Input must be a string."
    assert "~" in hypothesis, "Input must contain a '~' symbol."

    hypothesis = hypothesis.strip()
    if hypothesis.startswith("~"):
        hypothesis = "difference" + hypothesis

    rhs_ok = [
        "pairwise",
        "reference",
        "sequential",
        "revpairwise",
        "revreference",
        "revsequential",
    ]
    lhs_ok = ["ratio", "difference"]

    # Parse the formula using formulaic
    formula = Formula(hypothesis)

    # left-hand side
    lhs = list(formula.lhs.required_variables)
    msg = f"The left-hand side of the `hypothesis` formula must contain one of: {', '.join(lhs_ok)}."
    assert len(lhs) == 1, msg
    lhs = lhs[0]
    assert lhs in lhs_ok, msg

    # right-hand side
    if isinstance(formula.rhs, formulaic.formula.SimpleFormula):
        rhs = next(iter(formula.rhs.required_variables))
        by = None
    elif isinstance(formula.rhs, tuple):
        rhs = next(iter(formula.rhs[0].required_variables))
        by = list(formula.rhs[1].required_variables)

    msg = f"The right-hand side of the `hypothesis` formula must be element of: {', '.join(rhs_ok)}."
    assert rhs in rhs_ok, msg

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
        out = x.group_by(*by_hypothesis).agg(
            pl.col("estimate").map_batches(hyp_fun),
            pl.col("term").map_batches(lambda x: pl.Series(lab_fun(x))),
        )
        out = out.explode(["estimate", "term"], empty_as_null=True)

    return out
