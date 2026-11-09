from __future__ import annotations

import ast
import inspect
import re

import numpy as np
import polars as pl

from .planning import Hyp
from .utils import ingest


def _linear_ast(expr, vectors):
    """Return ``(weights, constant)`` for a provably affine expression."""

    def walk(node):
        if isinstance(node, ast.Expression):
            return walk(node.body)
        if isinstance(node, ast.Constant) and isinstance(node.value, (int, float)):
            return np.zeros(len(next(iter(vectors.values())))), float(node.value)
        if isinstance(node, ast.Name) and node.id in vectors:
            return vectors[node.id].copy(), 0.0
        if (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Name)
            and node.func.id == "Q"
            and len(node.args) == 1
            and isinstance(node.args[0], ast.Constant)
            and node.args[0].value in vectors
        ):
            return vectors[node.args[0].value].copy(), 0.0
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, (ast.UAdd, ast.USub)):
            value = walk(node.operand)
            if value is None:
                return None
            w, c = value
            return (w, c) if isinstance(node.op, ast.UAdd) else (-w, -c)
        if isinstance(node, ast.BinOp):
            left, right = walk(node.left), walk(node.right)
            if left is None or right is None:
                return None
            lw, lc = left
            rw, rc = right
            if isinstance(node.op, ast.Add):
                return lw + rw, lc + rc
            if isinstance(node.op, ast.Sub):
                return lw - rw, lc - rc
            if isinstance(node.op, ast.Mult):
                if not np.any(lw):
                    return rw * lc, rc * lc
                if not np.any(rw):
                    return lw * rc, lc * rc
                return None
            if isinstance(node.op, ast.Div) and not np.any(rw) and rc != 0:
                return lw / rc, lc / rc
        return None

    try:
        return walk(ast.parse(expr, mode="eval"))
    except (SyntaxError, TypeError, ValueError):
        return None


def _linear_hypothesis_matrix(expr, rowlabels, n_est):
    """Return ``(H, offset)`` for a provably affine expression, else None.

    The estimates are ``estimate @ H + offset``; the derivative is H alone,
    independent of the constant. Differentiating the constant numerically
    instead is catastrophic: a large offset cancels the probe step and
    incorrectly reports a constant result instead of preserving the derivative.
    """
    vectors = {label: row for label, row in zip(rowlabels, np.eye(n_est))}
    compiled = _linear_ast(expr, vectors)
    if compiled is None:
        return None
    weights, constant = compiled
    if not np.isfinite(constant) or not np.isfinite(weights).all():
        return None
    return weights.reshape(-1, 1), float(constant)


# Math functions usable without the `np.` prefix, as in R expressions.
_MATH_FUNCTIONS = (
    "exp",
    "log",
    "log10",
    "log2",
    "log1p",
    "expm1",
    "sqrt",
    "abs",
    "sin",
    "cos",
    "tan",
    "arcsin",
    "arccos",
    "arctan",
    "pi",
)


def _eval_string_function(vec, hypothesis, rowlabels):
    import numpy
    import scipy

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

    env = {name: getattr(numpy, name) for name in _MATH_FUNCTIONS}
    env.update(term_values)
    env["Q"] = Q
    env["numpy"] = numpy
    env["scipy"] = scipy
    env["np"] = numpy
    hypothesis = hypothesis.replace("=", "==")
    return eval(hypothesis, env)


def _compile_string_hypothesis(x: pl.DataFrame, hypothesis: str, lab: str) -> Hyp:
    # R syntax: `^` is a power (in Python it would be a bitwise xor).
    expr = re.sub("=", "-(", hypothesis.replace("^", "**")) + ")"

    if re.search(r"\bb\d+\b", expr):
        # Positional indices are 0-based in Python: b0 is the first estimate.
        # (R starts at b1; this is a deliberate language convention.)
        indices = [int(m.group()[1:]) for m in re.finditer(r"\bb\d+\b", expr)]
        bmax = max(indices)
        if bmax >= x.shape[0]:
            msg = (
                f"b{bmax} cannot be used in `hypothesis` because the call produced "
                f"just {x.shape[0]} estimate(s). Try executing the exact same "
                "command without the `hypothesis` argument to see which estimates "
                "are available for hypothesis testing."
            )
            raise ValueError(msg)

        for i in range(x.shape[0]):
            expr = re.sub(rf"\bb{i}\b", f"marginaleffects__{i}", expr)
        rowlabels = [f"marginaleffects__{i}" for i in range(x.shape[0])]
    else:
        terms = x["term"].to_list() if "term" in x.columns else []
        if not terms or len(set(terms)) < len(terms):
            msg = (
                "To use term names in a `hypothesis` string, the same function "
                "call without `hypothesis` argument must produce a `term` column "
                "with unique row identifiers. You can use `b0`, `b1`, etc. "
                "indices instead of term names in the `hypotheses` string Ex: "
                '"b0 + b1 = 0" Alternatively, you can use the `newdata`, '
                "`variables`, or `by` arguments."
            )
            raise ValueError(msg)
        rowlabels = terms

    def apply(est):
        est = np.asarray(est, dtype=float).reshape(-1)
        out = _eval_string_function(est, hypothesis=expr, rowlabels=rowlabels)
        return np.atleast_1d(np.asarray(out, dtype=float))

    compiled_affine = _linear_hypothesis_matrix(expr, rowlabels, x.height)
    if compiled_affine is not None:
        H, offset = compiled_affine
        estimate = x["estimate"].to_numpy()
        # Relative tolerance: with a large offset the values agree only to
        # their own floating-point spacing, which rtol tracks and a fixed
        # atol does not.
        if np.allclose(apply(estimate), estimate @ H + offset, rtol=1e-9, atol=1e-12):
            return Hyp(kind="matrix", apply=apply, H=H, offset=offset)
    return Hyp(kind="string", apply=apply)


def eval_string_hypothesis(x: pl.DataFrame, hypothesis: str, lab: str) -> pl.DataFrame:
    hyp = _compile_string_hypothesis(x, hypothesis, lab)
    out = hyp.apply(x["estimate"].to_numpy())
    return pl.DataFrame({"hypothesis": [re.sub(r"\s+", "", lab)], "estimate": out})


def lincom_multiply(x, lincom):
    estimates = x["estimate"].to_numpy()
    H = np.asarray(lincom, dtype=float)
    if H.ndim == 1:
        H = H.reshape(-1, 1)
    multiplied_results = np.asarray(estimates @ H, dtype=float).reshape(-1)
    return pl.DataFrame({"estimate": multiplied_results})


def _r_character(series: pl.Series) -> list[str]:
    """Format values the way R's ``as.character()`` does in ``paste()``."""

    def fmt(v):
        if v is None:
            return "NA"
        if isinstance(v, bool):
            return "TRUE" if v else "FALSE"
        if isinstance(v, float):
            if np.isnan(v):
                return "NaN"
            return format(v, ".15g")
        return str(v)

    return [fmt(v) for v in series.to_list()]


def get_hypothesis_row_labels(x, by=None, hypothesis_by=None):
    """Row labels for formula hypotheses, mirroring R's ``get_labels()``.

    Label columns are ``term``, ``group``, ``contrast``, ``contrast_*``,
    ``value`` and ``by``, plus the ``by`` columns, minus the ``| group``
    columns (``hypothesis_by``), keeping only columns that vary. Values are
    pasted with a space. Without such columns, or (ungrouped) when labels are
    duplicated, rows are labelled ``b0``, ``b1``, ...
    """
    pattern = re.compile(r"^(term|group|contrast|contrast_.*|value|by)$")
    cols = [col for col in x.columns if pattern.match(col)]
    if isinstance(by, str):
        by = [by]
    if isinstance(by, (list, tuple)):
        cols = list(dict.fromkeys(cols + [b for b in by if b in x.columns]))
    cols = [c for c in cols if c not in (hypothesis_by or [])]
    cols = [c for c in cols if x[c].n_unique() > 1]

    # b0, b1, ...: R uses b1, b2, ..., matching its 1-based positional indices;
    # Python's are 0-based, so the fallback labels follow them.
    fallback = [f"b{i}" for i in range(x.height)]
    if not cols:
        return fallback
    pieces = [_r_character(x[c]) for c in cols]
    labels = [" ".join(parts) for parts in zip(*pieces)]
    # As in R, the duplicate check only applies without `| group`: within a
    # grouped hypothesis, labels repeat across groups by design.
    if not hypothesis_by and len(set(labels)) < len(labels):
        return fallback
    return labels


def _label_columns(by, newdata, x):
    """The `by` columns used for labels; R falls back to datagrid() names."""
    if isinstance(by, (str, list, tuple)) and not isinstance(by, bool):
        return by
    names = getattr(newdata, "datagrid_explicit", None) or []
    return [n for n in names if n in x.columns] or None


def _compile_formula_hypothesis(x, hypothesis, lab) -> Hyp:
    template = x.clone()

    def apply(est):
        from .test.formula import eval_hypothesis_formula

        tmp = template.with_columns(pl.Series(np.asarray(est)).alias("estimate"))
        out = eval_hypothesis_formula(tmp, hypothesis, lab=lab)
        return out["estimate"].to_numpy()

    from .test.formula import parse_hypothesis_formula

    lhs, _, _ = parse_hypothesis_formula(hypothesis)
    # Differences and contrast-weight dot products are linear in the
    # estimates, so they can be compiled to an exact matrix stage.
    if lhs in ("difference", "dotproduct"):
        estimate = x["estimate"].to_numpy()
        base = apply(estimate)
        columns = []
        for row in np.eye(x.height):
            candidate = apply(estimate + row)
            if candidate.shape != base.shape:
                return Hyp(kind="formula", apply=apply)
            columns.append(candidate - base)
        H = np.column_stack(columns).T
        if np.allclose(apply(estimate), estimate @ H, rtol=1e-12, atol=1e-12):
            return Hyp(kind="matrix", apply=apply, H=H)
    return Hyp(kind="formula", apply=apply)


# Bare strings R rejects; the formula interface replaces them.
_DEPRECATED_STRINGS = {
    "pairwise",
    "revpairwise",
    "sequential",
    "revsequential",
    "reference",
    "meandev",
    "meanotherdev",
    "revreference",
}


def _combine(results) -> tuple[pl.DataFrame, Hyp]:
    """Stack several compiled hypotheses (a list, or an expanded ``b*``)."""
    frames = [out for out, _ in results]
    hyps = [hyp for _, hyp in results]

    def apply(est):
        return np.concatenate([hyp.apply(est) for hyp in hyps])

    out = pl.concat(frames, how="vertical")
    if all(hyp.kind == "matrix" for hyp in hyps):
        H = np.column_stack([hyp.H for hyp in hyps])
        offset = np.concatenate(
            [
                np.broadcast_to(np.atleast_1d(hyp.offset), (hyp.H.shape[1],))
                for hyp in hyps
            ]
        )
        return out, Hyp(kind="matrix", apply=apply, H=H, offset=offset)
    return out, Hyp(kind="list", apply=apply)


# `b*` as a standalone token: not the tail of a name (`carb*2`), not a power
# (`b**2`), and not followed by an operand (`b*2`, `b*x`, `b*(x)`), which
# would be a multiplication involving a term named `b`.
_WILDCARD = re.compile(r"\bb\*(?!\*)(?!\s*[\w.(])")


def _as_polars_frame(obj) -> pl.DataFrame:
    """A user-supplied data frame as polars, without a pandas index column."""
    if isinstance(obj, pl.DataFrame):
        return obj
    if type(obj).__module__.startswith("pandas"):
        return pl.from_pandas(obj, include_index=False)
    return pl.DataFrame(ingest(obj))


def _expand_wildcard(hypothesis: str, n: int) -> list[str] | None:
    """``"b* = 0"`` -> ``["b0 = 0", ..., "b{n-1} = 0"]``, as R's expand_wildcard().

    Indices are 0-based, like Python's positional indices.
    """
    count = len(_WILDCARD.findall(hypothesis))
    if count == 0:
        return None
    if count > 1:
        raise ValueError("More than one 'b*' substring found.")
    return [_WILDCARD.sub(f"b{i}", hypothesis) for i in range(n)]


def _weights_hypothesis(x: pl.DataFrame, hypothesis) -> tuple[pl.DataFrame, Hyp]:
    """Numeric vector, matrix or data frame of weights, as R's hypothesis_matrix().

    Each column is a vector of weights. Columns are labelled by their names
    when the weights come as a data frame, else ``"custom"`` as in R.
    """
    if isinstance(hypothesis, (np.ndarray, list, tuple)):
        H, labels = np.asarray(hypothesis, dtype=float), None
    else:
        frame = _as_polars_frame(hypothesis)
        H, labels = frame.to_numpy().astype(float), frame.columns
    if H.ndim == 1:
        H = H.reshape(-1, 1)
    if H.ndim != 2 or H.shape[0] != x.height:
        raise ValueError(
            f"The `hypothesis` weights must have {x.height} rows, one per "
            f"estimate. Got shape {H.shape}."
        )
    if labels is None:
        labels = ["custom"] * H.shape[1]

    def apply(est):
        return np.asarray(np.asarray(est, dtype=float).reshape(-1) @ H).reshape(-1)

    hyp = Hyp(kind="matrix", apply=apply, H=H)
    out = pl.DataFrame(
        {"term": labels, "estimate": hyp.apply(x["estimate"].to_numpy())}
    )
    return out, hyp


def _function_output(out, x: pl.DataFrame) -> pl.DataFrame:
    """Validate a hypothesis function's result, as R's hypothesis_function()."""
    msg = (
        "The `hypothesis` argument function must return a data frame with "
        "`term` (or `hypothesis`) and `estimate` columns."
    )
    if out is None or (hasattr(out, "__len__") and len(out) == 0):
        raise ValueError("The `hypothesis` function returned an empty result.")
    # Data frames have columns; Series and arrays are numeric results.
    if hasattr(out, "columns") and hasattr(out, "__arrow_c_stream__"):
        out = _as_polars_frame(out)
        if "estimate" not in out.columns or not (
            "term" in out.columns or "hypothesis" in out.columns
        ):
            raise ValueError(msg)
        return out
    try:
        est = np.asarray(out, dtype=float).reshape(-1)
    except (TypeError, ValueError) as exc:
        raise ValueError(msg) from exc
    if "term" in x.columns and x.height == est.size:
        return pl.DataFrame({"term": x["term"], "estimate": est})
    return pl.DataFrame({"term": list(range(1, est.size + 1)), "estimate": est})


def _function_hypothesis(x, fun, by, newdata) -> tuple[pl.DataFrame, Hyp]:
    """Callable hypothesis, as R's hypothesis_function().

    ``fun`` must accept ``x`` (the estimates as a data frame) and may accept
    ``newdata`` and ``by``. It returns a data frame with ``term`` (or
    ``hypothesis``) and ``estimate`` columns, or a numeric vector.
    """
    params = inspect.signature(fun).parameters
    if "x" not in params:
        raise ValueError("The `hypothesis` function must accept an `x` argument.")
    if not set(params) <= {"x", "newdata", "by"}:
        raise ValueError(
            "The allowable arguments for the `hypothesis` function are: `x`, "
            "`newdata`, and `by`."
        )
    template = x.clone()

    def run(est):
        tmp = template.with_columns(pl.Series("estimate", np.asarray(est)))
        kwargs = {"x": tmp, "newdata": newdata, "by": by}
        return _function_output(fun(**{k: kwargs[k] for k in params}), tmp)

    out = run(x["estimate"].to_numpy())
    n_out = out.height

    def apply(est):
        value = run(est)["estimate"].to_numpy()
        if value.size != n_out:
            raise ValueError(
                "The `hypothesis` function must return the same number of "
                "estimates every time it is called."
            )
        return value

    return out, Hyp(kind="function", apply=apply)


def hypothesis_compile(x: pl.DataFrame, hypothesis, by=None, newdata=None):
    """Apply ``hypothesis`` to the estimates in ``x``, as R's get_hypothesis().

    Returns the hypothesis frame and its compiled stage (``None`` when the
    hypothesis is absent or a scalar null value).
    """
    if hypothesis is None or isinstance(hypothesis, (int, float)):
        return x, None

    if isinstance(hypothesis, str) and hypothesis.strip() in _DEPRECATED_STRINGS:
        raise ValueError(
            "This string is deprecated for use in the `hypothesis` argument. "
            'Use the formula interface instead. Ex: `hypothesis="~reference"`'
        )

    if callable(hypothesis):
        return _function_hypothesis(x, hypothesis, by=by, newdata=newdata)

    if isinstance(hypothesis, np.ndarray) or hasattr(hypothesis, "__arrow_c_stream__"):
        return _weights_hypothesis(x, hypothesis)
    if isinstance(hypothesis, (list, tuple)) and all(
        isinstance(h, (int, float)) and not isinstance(h, bool) for h in hypothesis
    ):
        return _weights_hypothesis(x, hypothesis)

    if isinstance(hypothesis, str) and "~" in hypothesis:
        from .test.formula import eval_hypothesis_formula, parse_hypothesis_formula

        _, _, hypothesis_by = parse_hypothesis_formula(hypothesis)
        lab = get_hypothesis_row_labels(
            x, by=_label_columns(by, newdata, x), hypothesis_by=hypothesis_by
        )
        hyp = _compile_formula_hypothesis(x, hypothesis, lab)
        out = eval_hypothesis_formula(x, hypothesis, lab=lab)
        return out, hyp

    if isinstance(hypothesis, str):
        expanded = _expand_wildcard(hypothesis, x.height)
        if expanded is not None:
            return _combine([hypothesis_compile(x, h) for h in expanded])
        hyp = _compile_string_hypothesis(x, hypothesis, lab=hypothesis)
        out = pl.DataFrame(
            {
                "hypothesis": [re.sub(r"\s+", "", hypothesis)],
                "estimate": hyp.apply(x["estimate"].to_numpy()),
            }
        )
        return out, hyp

    if isinstance(hypothesis, (list, tuple)) and len(hypothesis) > 0:
        if not all(isinstance(item, str) for item in hypothesis):
            raise TypeError(
                "When `hypothesis` is a sequence, every element must be a string "
                "(or every element a number, for a vector of weights)."
            )
        return _combine(
            [hypothesis_compile(x, h, by=by, newdata=newdata) for h in hypothesis]
        )

    raise ValueError("Invalid `hypothesis` argument")
