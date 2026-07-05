from marginaleffects.hypothesis_compile import (
    eval_string_hypothesis,
    get_hypothesis_row_labels,
    hypothesis_compile,
    lincom_multiply,
)

__all__ = [
    "eval_string_hypothesis",
    "get_hypothesis",
    "get_hypothesis_row_labels",
    "lincom_multiply",
]


def get_hypothesis(x, hypothesis, by=None):
    return hypothesis_compile(x, hypothesis, by=by)[0]
