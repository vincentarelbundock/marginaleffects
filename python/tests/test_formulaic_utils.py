import pytest
from marginaleffects.formulaic_utils import parse_variables_categorical


@pytest.mark.parametrize(
    "formula,expected",
    [
        ("y ~ C(x1) + x2", ["x1"]),
        ("y ~ C(x1) + C(x2)", ["x1", "x2"]),
        ("y ~ x1 + x2", []),
        ("y ~ C(x1, levels=[1,2]) + C(x2)", ["x1", "x2"]),
        ("y ~ C(x1) + C(x2) + C(x3)", ["x1", "x2", "x3"]),
        ("y ~ x1 + C(x2) + x3", ["x2"]),
        ("y ~ C(x1) * C(x2)", ["x1", "x2"]),
        ("y ~ C(x1) + C(x2) + x3 + C(x4)", ["x1", "x2", "x4"]),
        ("y ~ C(x1) + x2 + C(x3, Treatment(reference='base-level'))", ["x1", "x3"]),
    ],
)
def test_parse_variables_categorical(formula, expected):
    assert parse_variables_categorical(formula) == expected
