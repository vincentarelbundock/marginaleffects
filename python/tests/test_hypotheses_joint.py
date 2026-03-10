import polars as pl
import pytest
import statsmodels.formula.api as smf
from polars.testing import assert_frame_equal

from marginaleffects import *
from tests.helpers import mtcars


mod = smf.ols("am ~ hp + wt + disp", data=mtcars).fit()

mod_without_intercept = smf.ols("am ~ 0 + hp + wt + disp", data=mtcars).fit()


@pytest.mark.parametrize(
    "kwargs, expected_file",
    [
        ({"joint": ["hp", "wt"]}, "test_hypotheses_joint_01.csv"),
        (
            {"joint": ["hp", "disp"], "joint_test": "chisq"},
            "test_hypotheses_joint_02.csv",
        ),
        ({"joint": [1, 2]}, "test_hypotheses_joint_03.csv"),
        ({"joint": [0, 1, 2], "hypothesis": [1, 2, 3]}, "test_hypotheses_joint_04.csv"),
        (
            {"joint": ["Intercept", "disp", "wt"], "hypothesis": 4},
            "test_hypotheses_joint_05.csv",
        ),
    ],
)
def test_hypotheses_joint(kwargs, expected_file):
    hypo_py = hypotheses(mod, **kwargs)
    hypo_r = pl.read_csv(f"tests/r/{expected_file}").rename({"p.value": "p_value"})
    hypo_r = hypo_r.cast({"p_value": pl.Float64})
    assert_frame_equal(hypo_py.to_polars(), hypo_r)


@pytest.mark.parametrize(
    "kwargs, expected_file",
    [
        ({"joint": [0, 1, 2]}, "test_hypotheses_joint_06.csv"),
        ({"joint": ["hp", "wt"]}, "test_hypotheses_joint_07.csv"),
    ],
)
def test_hypotheses_joint_2(kwargs, expected_file):
    hypo_py = hypotheses(mod_without_intercept, **kwargs)
    hypo_r = pl.read_csv(f"tests/r/{expected_file}").rename({"p.value": "p_value"})
    assert_frame_equal(hypo_py.to_polars(), hypo_r)


@pytest.mark.parametrize(
    "kwargs, expected_file",
    [
        ({"joint": "hp|wt"}, "test_hypotheses_joint_01.csv"),
        ({"joint": "^..$"}, "test_hypotheses_joint_01.csv"),
        (
            {"joint": "p$", "joint_test": "chisq"},
            "test_hypotheses_joint_02.csv",
        ),  # matches hp and disp
    ],
)
def test_hypotheses_joint_regex(kwargs, expected_file):
    hypo_py = hypotheses(mod, **kwargs)
    hypo_r = pl.read_csv(f"tests/r/{expected_file}").rename({"p.value": "p_value"})
    assert_frame_equal(hypo_py.to_polars(), hypo_r)
