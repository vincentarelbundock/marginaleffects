from formulaic import Formula
import formulaic
import numpy as np
import polars as pl


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


def pairwise_ratio_comparison(x, safe_mode=True):
    x = x.to_numpy()
    out = np.tril(np.divide.outer(x, x), -1)
    return out[out != 0]


def pairwise_ratio_label(x):
    labels = [
        f"({xi}) / ({xj})" for i, xi in enumerate(x) for j, xj in enumerate(x) if i > j
    ]
    return labels


def pairwise_difference_comparison(x, safe_mode=True):
    x = x.to_numpy()
    out = np.tril(np.subtract.outer(x, x), -1)
    return out[out != 0]


def pairwise_difference_label(x):
    labels = [
        f"({xi}) - ({xj})" for i, xi in enumerate(x) for j, xj in enumerate(x) if i > j
    ]
    return labels


def revpairwise_ratio_comparison(x, safe_mode=True):
    x = x.to_numpy()
    out = np.triu(np.divide.outer(x, x), 1)
    return out[out != 0]


def revpairwise_ratio_label(x):
    labels = [
        f"({xi}) / ({xj})" for i, xi in enumerate(x) for j, xj in enumerate(x) if i < j
    ]
    return labels


def revpairwise_difference_comparison(x, safe_mode=True):
    x = x.to_numpy()
    out = np.triu(np.subtract.outer(x, x), 1)
    return out[out != 0]


def revpairwise_difference_label(x):
    labels = [
        f"({xi}) - ({xj})" for i, xi in enumerate(x) for j, xj in enumerate(x) if i < j
    ]
    return labels


def trt_vs_ctrl_ratio_comparison(x):
    return np.mean(x[1:] / x[0])


def trt_vs_ctrl_ratio_label(x):
    return "Mean(Trt) / Ctrl"


def trt_vs_ctrl_difference_comparison(x):
    return np.mean(x[1:] - x[0])


def trt_vs_ctrl_difference_label(x):
    return "Mean(Trt) - Ctrl"


def meandev_ratio_comparison(x):
    mean_x = np.mean(x)
    return x / mean_x


def meandev_ratio_label(x):
    return [f"({xi}) / Mean" for xi in x]


def meandev_difference_comparison(x):
    mean_x = np.mean(x)
    return x - mean_x


def meandev_difference_label(x):
    return [f"({xi}) - Mean" for xi in x]


def meanotherdev_ratio_comparison(x):
    s = np.sum(x)
    m_other = (s - x) / (len(x) - 1)
    return x / m_other


def meanotherdev_ratio_label(x):
    return [f"({xi}) / Mean (other)" for xi in x]


def meanotherdev_difference_comparison(x):
    s = np.sum(x)
    m_other = (s - x) / (len(x) - 1)
    return x - m_other


def meanotherdev_difference_label(x):
    return [f"({xi}) - Mean (other)" for xi in x]


def poly_dotproduct_comparison(x):
    nx = len(x)
    w = np.polynomial.polynomial.polyvander(x, min(5, nx - 1))
    return np.dot(w.T, x)


def poly_dotproduct_label(x):
    return ["Linear", "Quadratic", "Cubic", "Quartic", "Quintic"][: min(5, len(x) - 1)]


def helmert_dotproduct_comparison(x):
    nx = len(x)
    w = np.polynomial.polynomial.polyvander(x, nx - 1)
    out = np.dot(x, w)
    return out


def helmert_dotproduct_label(x):
    return x


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
        rhs = list(formula.rhs.required_variables)[0]
        by = None
    elif isinstance(formula.rhs, tuple):
        rhs = list(formula.rhs[0].required_variables)[0]
        by = list(formula.rhs[1].required_variables)

    msg = f"The right-hand side of the `hypothesis` formula must be element of: {', '.join(rhs_ok)}."
    assert rhs in rhs_ok, msg

    return lhs, rhs, by


def eval_hypothesis_formula(x, hypothesis, lab):
    lhs, rhs, by_hypothesis = parse_hypothesis_formula(hypothesis)
    if lhs == "ratio":
        if rhs == "reference":
            hyp_fun = reference_ratio_comparison
            lab_fun = reference_ratio_label
        elif rhs == "sequential":
            hyp_fun = sequential_ratio_comparison
            lab_fun = sequential_ratio_label
        elif rhs == "pairwise":
            hyp_fun = pairwise_ratio_comparison
            lab_fun = pairwise_ratio_label
        if rhs == "revreference":
            hyp_fun = revreference_ratio_comparison
            lab_fun = revreference_ratio_label
        elif rhs == "revsequential":
            hyp_fun = revsequential_ratio_comparison
            lab_fun = revsequential_ratio_label
        elif rhs == "revpairwise":
            hyp_fun = revpairwise_ratio_comparison
            lab_fun = revpairwise_ratio_label
    if lhs == "difference":
        if rhs == "reference":
            hyp_fun = reference_difference_comparison
            lab_fun = reference_difference_label
        elif rhs == "sequential":
            hyp_fun = sequential_difference_comparison
            lab_fun = sequential_difference_label
        elif rhs == "pairwise":
            hyp_fun = pairwise_difference_comparison
            lab_fun = pairwise_difference_label
        if rhs == "revreference":
            hyp_fun = revreference_difference_comparison
            lab_fun = revreference_difference_label
        elif rhs == "revsequential":
            hyp_fun = revsequential_difference_comparison
            lab_fun = revsequential_difference_label
        elif rhs == "revpairwise":
            hyp_fun = revpairwise_difference_comparison
            lab_fun = revpairwise_difference_label

    # out = x.with_columns(pl.Series(lab).alias("term"))
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
        out = out.explode(["estimate", "term"])

    return out
