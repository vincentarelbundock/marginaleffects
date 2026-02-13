import re
from itertools import compress
import numpy as np
import polars as pl
import numpy


def eval_string_hypothesis(x: pl.DataFrame, hypothesis: str, lab: str) -> pl.DataFrame:
    hypothesis = re.sub("=", "-(", hypothesis) + ")"
    if re.search(r"\bb\d+\b", hypothesis):
        bmax = max(
            [
                int(re.sub("b", "", match.group()))
                for match in re.finditer(r"\bb\d+\b", lab)
            ],
            default=0,
        )
        if bmax > x.shape[0]:
            msg = f"b{bmax} cannot be used in `hypothesis` because the call produced just {x.shape[0]} estimate(s). Try executing the exact same command without the `hypothesis` argument to see which estimates are available for hypothesis testing."
            raise ValueError(msg)

        for i in range(x.shape[0]):
            tmp = f"marginaleffects__{i}"
            hypothesis = re.sub(f"b{i}", tmp, hypothesis)

        rowlabels = [f"marginaleffects__{i}" for i in range(x.shape[0])]
    else:
        if "term" not in x.columns:
            msg = 'To use term names in a `hypothesis` string, the same function call without `hypothesis` argument must produce a `term` column with unique row identifiers. You can use `b0`, `b1`, etc. indices instead of term names in the `hypotheses` string Ex: "b0 + b1 = 0" Alternatively, you can use the `newdata`, `variables`, or `by` arguments:'
            raise ValueError(msg)

        if len(x["term"]) != len(set(x["term"])):
            # Collapse duplicate terms by averaging their estimates to create unique labels.
            x = x.group_by("term", maintain_order=True).agg(
                pl.col("estimate").mean().alias("estimate")
            )
            if len(x["term"]) != len(set(x["term"])):
                msg = 'To use term names in a `hypothesis` string, the same function call without `hypothesis` argument must produce a `term` column with unique row identifiers. You can use `b0`, `b1`, etc. indices instead of term names in the `hypotheses` string Ex: "b0 + b1 = 0" Alternatively, you can use the `newdata`, `variables`, or `by` arguments:'
                raise ValueError(msg)

        rowlabels = x["term"].to_list()

    def eval_string_function(vec, hypothesis, rowlabels):
        import scipy

        env = {rowlabel: vec[i] for i, rowlabel in enumerate(rowlabels)}
        env["numpy"] = numpy
        env["scipy"] = scipy
        env["np"] = numpy
        hypothesis = hypothesis.replace("=", "==")
        out = eval(hypothesis, env)
        return out

    out = eval_string_function(
        x["estimate"].to_numpy(), hypothesis=hypothesis, rowlabels=rowlabels
    )

    out = pl.DataFrame({"term": [re.sub(r"\s+", "", lab)], "estimate": [out]})

    return out


# function extracts the estimate column from a data frame and sets it to x. If `hypothesis` argument is a numpy array, it feeds it directly to lincome_multiply. If lincome is a string, it checks if the string is valid, and then calls the corresponding function.
def get_hypothesis(x, hypothesis, by=None):
    msg = "Invalid `hypothesis` argument"

    lab = get_hypothesis_row_labels(x, by=by)
    if hypothesis is None or isinstance(hypothesis, (int, float)):
        return x
    if isinstance(hypothesis, np.ndarray):
        out = lincom_multiply(x, hypothesis)
        lab = [f"H{i + 1}" for i in range(out.shape[0])]
        out = out.with_columns(pl.Series(lab).alias("term"))
    elif isinstance(hypothesis, str) and "~" in hypothesis:
        from .hypothesis_formula import eval_hypothesis_formula

        out = eval_hypothesis_formula(x, hypothesis, lab=lab)
    elif isinstance(hypothesis, str) and "=" in hypothesis:
        out = eval_string_hypothesis(x, hypothesis, lab=hypothesis)
    elif isinstance(hypothesis, (list, tuple)):
        if len(hypothesis) == 0:
            raise ValueError(msg)
        frames = []
        for hyp in hypothesis:
            if not isinstance(hyp, str):
                raise ValueError(
                    "When `hypothesis` is a sequence, every element must be a string."
                )
            if "~" in hyp:
                from .hypothesis_formula import eval_hypothesis_formula

                frames.append(eval_hypothesis_formula(x, hyp, lab=lab))
            elif "=" in hyp:
                frames.append(eval_string_hypothesis(x, hyp, lab=hyp))
            else:
                raise ValueError(msg)
        out = pl.concat(frames, how="vertical")
    else:
        raise ValueError(msg)
    return out


def lincom_multiply(x, lincom):
    estimates = x["estimate"]
    multiplied_results = np.dot(estimates, lincom)
    out = pl.DataFrame({"estimate": multiplied_results})
    return out


def get_hypothesis_row_labels(x, by=None):
    pattern = re.compile(r"^(term|by|group|value|contrast|contrast_)$")
    lab = [col for col in x.columns if pattern.match(col)]

    # Step 2: Filter columns with more than one unique value
    lab = [col for col in lab if len(x[col].unique()) > 1]

    # Step 3: Include additional columns from "by" if provided
    if by is not None and isinstance(by, bool) is False:
        if isinstance(by, str):
            by = [by]
        lab = [e for e in list(set(lab) | set(by)) if e != "group"]

    # Step 4: If no columns left, return default
    if len(lab) == 0:
        return [f"{i}" for i in range(len(x))]

    # Step 5: Create a sub-dataframe with selected columns
    lab_df = x[lab]
    idx = [x[col].n_unique() > 1 for col in lab_df.columns]

    # Step 6: Create row labels by concatenating values
    if any(idx):
        lab_df = lab_df.select(list(compress(lab_df.columns, idx)))
    lab = lab_df.select(
        pl.concat_str(lab_df.columns, separator=", ").alias("concatenated")
    )["concatenated"].to_list()

    # Step 7: Wrap labels containing "-" in parentheses
    lab = [f"({label})" if "-" in label else label for label in lab]

    return lab
