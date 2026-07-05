from __future__ import annotations

import re
from itertools import compress

import numpy as np
import polars as pl

from .plan import Hyp


def _group_term_indices(terms):
    groups_by_term = {}
    has_duplicates = False
    for i, term in enumerate(terms):
        idx = groups_by_term.setdefault(term, [])
        if idx:
            has_duplicates = True
        idx.append(i)
    if not has_duplicates:
        return terms, None
    return list(groups_by_term), list(groups_by_term.values())


def _eval_string_function(vec, hypothesis, rowlabels):
    import scipy
    import numpy

    hypothesis = re.sub(r"Q\(([^()]*(?:\([^()]*\)[^()]*)*)\)", r'Q("\1")', hypothesis)
    term_values = {rowlabel: vec[i] for i, rowlabel in enumerate(rowlabels)}

    def Q(name):
        if name not in term_values:
            msg = (
                f"Term '{name}' used in Q() not found. "
                f"Available terms: {list(term_values.keys())}"
            )
            raise ValueError(msg)
        return term_values[name]

    env = dict(term_values)
    env["Q"] = Q
    env["numpy"] = numpy
    env["scipy"] = scipy
    env["np"] = numpy
    hypothesis = hypothesis.replace("=", "==")
    return eval(hypothesis, env)


def _compile_string_hypothesis(x: pl.DataFrame, hypothesis: str, lab: str) -> Hyp:
    expr = re.sub("=", "-(", hypothesis) + ")"

    if re.search(r"\bb\d+\b", expr):
        bmax = max(
            [
                int(re.sub("b", "", match.group()))
                for match in re.finditer(r"\bb\d+\b", lab)
            ],
            default=0,
        )
        if bmax > x.shape[0]:
            msg = (
                f"b{bmax} cannot be used in `hypothesis` because the call produced "
                f"just {x.shape[0]} estimate(s). Try executing the exact same "
                "command without the `hypothesis` argument to see which estimates "
                "are available for hypothesis testing."
            )
            raise ValueError(msg)

        for i in range(x.shape[0]):
            tmp = f"marginaleffects__{i}"
            expr = re.sub(f"b{i}", tmp, expr)

        rowlabels = [f"marginaleffects__{i}" for i in range(x.shape[0])]
        groups = None
    else:
        if "term" not in x.columns:
            msg = (
                "To use term names in a `hypothesis` string, the same function "
                "call without `hypothesis` argument must produce a `term` column "
                "with unique row identifiers. You can use `b0`, `b1`, etc. "
                "indices instead of term names in the `hypotheses` string Ex: "
                '"b0 + b1 = 0" Alternatively, you can use the `newdata`, '
                "`variables`, or `by` arguments:"
            )
            raise ValueError(msg)

        terms = x["term"].to_list()
        rowlabels, groups = _group_term_indices(terms)

    def apply(est):
        est = np.asarray(est, dtype=float).reshape(-1)
        if groups is not None:
            est = np.asarray([np.mean(est[idx]) for idx in groups], dtype=float)
        out = _eval_string_function(est, hypothesis=expr, rowlabels=rowlabels)
        return np.atleast_1d(np.asarray(out, dtype=float))

    return Hyp(kind="string", apply=apply)


def eval_string_hypothesis(x: pl.DataFrame, hypothesis: str, lab: str) -> pl.DataFrame:
    hyp = _compile_string_hypothesis(x, hypothesis, lab)
    out = hyp.apply(x["estimate"].to_numpy())
    return pl.DataFrame({"term": [re.sub(r"\s+", "", lab)], "estimate": out})


def lincom_multiply(x, lincom):
    estimates = x["estimate"].to_numpy()
    H = np.asarray(lincom, dtype=float)
    if H.ndim == 1:
        H = H.reshape(-1, 1)
    multiplied_results = np.asarray(estimates @ H, dtype=float).reshape(-1)
    return pl.DataFrame({"estimate": multiplied_results})


def get_hypothesis_row_labels(x, by=None):
    pattern = re.compile(r"^(term|by|group|value|contrast|contrast_)$")
    lab = [col for col in x.columns if pattern.match(col)]

    lab = [col for col in lab if len(x[col].unique()) > 1]

    if by is not None and isinstance(by, bool) is False:
        if isinstance(by, str):
            by = [by]
        lab = [e for e in list(set(lab) | set(by)) if e != "group"]

    if len(lab) == 0:
        return [f"{i}" for i in range(len(x))]

    lab_df = x[lab]
    idx = [x[col].n_unique() > 1 for col in lab_df.columns]

    if any(idx):
        lab_df = lab_df.select(list(compress(lab_df.columns, idx)))
    lab = lab_df.select(
        pl.concat_str(lab_df.columns, separator=", ").alias("concatenated")
    )["concatenated"].to_list()

    return [f"({label})" if "-" in label else label for label in lab]


def _compile_formula_hypothesis(x, hypothesis, lab) -> Hyp:
    template = x.clone()

    def apply(est):
        from .test.formula import eval_hypothesis_formula

        tmp = template.with_columns(pl.Series(np.asarray(est)).alias("estimate"))
        out = eval_hypothesis_formula(tmp, hypothesis, lab=lab)
        return out["estimate"].to_numpy()

    return Hyp(kind="formula", apply=apply)


def hypothesis_compile(x: pl.DataFrame, hypothesis, by=None):
    msg = "Invalid `hypothesis` argument"

    lab = get_hypothesis_row_labels(x, by=by)
    if hypothesis is None or isinstance(hypothesis, (int, float)):
        return x, None

    if isinstance(hypothesis, np.ndarray):
        H = np.asarray(hypothesis, dtype=float)
        if H.ndim == 1:
            H = H.reshape(-1, 1)

        def apply(est):
            return np.asarray(np.asarray(est, dtype=float).reshape(-1) @ H).reshape(-1)

        hyp = Hyp(kind="matrix", apply=apply, H=H)
        out = pl.DataFrame({"estimate": hyp.apply(x["estimate"].to_numpy())})
        labels = [f"H{i + 1}" for i in range(out.shape[0])]
        out = out.with_columns(pl.Series(labels).alias("term"))
        return out, hyp

    if isinstance(hypothesis, str) and "~" in hypothesis:
        from .test.formula import eval_hypothesis_formula

        hyp = _compile_formula_hypothesis(x, hypothesis, lab)
        out = eval_hypothesis_formula(x, hypothesis, lab=lab)
        return out, hyp

    if isinstance(hypothesis, str) and "=" in hypothesis:
        hyp = _compile_string_hypothesis(x, hypothesis, lab=hypothesis)
        out = pl.DataFrame(
            {
                "term": [re.sub(r"\s+", "", hypothesis)],
                "estimate": hyp.apply(x["estimate"].to_numpy()),
            }
        )
        return out, hyp

    if isinstance(hypothesis, (list, tuple)):
        if len(hypothesis) == 0:
            raise ValueError(msg)
        frames = []
        hyps = []
        for item in hypothesis:
            if not isinstance(item, str):
                raise ValueError(
                    "When `hypothesis` is a sequence, every element must be a string."
                )
            out, hyp = hypothesis_compile(x, item, by=by)
            frames.append(out)
            hyps.append(hyp)

        def apply(est):
            pieces = [hyp.apply(est) for hyp in hyps if hyp is not None]
            return np.concatenate(pieces) if pieces else np.asarray(est, dtype=float)

        return pl.concat(frames, how="vertical"), Hyp(kind="list", apply=apply)

    raise ValueError(msg)
