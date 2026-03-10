import pytest
from marginaleffects import *
from marginaleffects.plot.comparisons import *
from tests.utilities import *
from tests.helpers import *
import statsmodels.formula.api as smf

pytestmark = pytest.mark.skipif(True, reason="Plot tests skipped on all platforms")

FIGURES_FOLDER = "plot_comparisons"


@pytest.mark.plot
@pytest.mark.parametrize(
    ("by", "condition", "expected_file"),
    [
        pytest.param(
            "island",
            None,
            "continuous_01",
        ),
        pytest.param(
            False,
            ["flipper_length_mm", "species"],
            "continuous_02",
        ),
        pytest.param(
            False,
            "species",
            "continuous_03",
        ),
    ],
)
def test_continuous(by, condition, expected_file, penguins_mod_add):
    fig = plot_comparisons(
        penguins_mod_add,
        variables="bill_length_mm",
        by=by,
        condition=condition,
    )
    assert assert_image(fig, expected_file, FIGURES_FOLDER) is None


@pytest.mark.plot
@pytest.mark.parametrize(
    ("variables", "by", "condition", "expected_file"),
    [
        pytest.param(
            "species",
            False,
            ["bill_length_mm", "island"],
            "discrete_01",
        ),
        pytest.param(
            "species",
            "island",
            None,
            "discrete_02",
        ),
        pytest.param(
            "species",
            False,
            "bill_length_mm",
            "discrete_03",
        ),
    ],
)
def test_discrete(variables, by, condition, expected_file, penguins_mod_add):
    fig = plot_comparisons(
        penguins_mod_add,
        variables=variables,
        by=by,
        condition=condition,
    )
    assert assert_image(fig, expected_file, FIGURES_FOLDER) is None


@pytest.mark.plot
@pytest.mark.parametrize(
    ("condition", "expected_file"),
    [
        pytest.param(
            {
                "bill_length_mm": None,
                "island": None,
                "flipper_length_mm": "threenum",
            },
            "threenum_horiz_axis",
        ),
        pytest.param(
            {
                "bill_length_mm": None,
                "flipper_length_mm": "threenum",
                "island": None,
            },
            "threenum_color",
        ),
    ],
)
def test_threenum(condition, expected_file, penguins_mod_add):
    fig = plot_comparisons(penguins_mod_add, variables="species", condition=condition)
    assert assert_image(fig, expected_file, FIGURES_FOLDER) is None

    @pytest.mark.parametrize(
        "variables, condition, gray, expected_file",
        [
            (
                "species",
                {
                    "bill_length_mm": None,
                    "island": None,
                    "flipper_length_mm": "threenum",
                },
                True,
                "threenum_horiz_axis_gray",
            ),
            (
                "species",
                {
                    "bill_length_mm": None,
                    "flipper_length_mm": "threenum",
                    "island": None,
                },
                True,
                "threenum_color_gray",
            ),
            (
                "species",
                {
                    "bill_length_mm": None,
                    "island": None,
                    "flipper_length_mm": "threenum",
                },
                False,
                "threenum_horiz_axis",
            ),
            (
                "species",
                {
                    "bill_length_mm": None,
                    "flipper_length_mm": "threenum",
                    "island": None,
                },
                False,
                "threenum_color",
            ),
        ],
    )
    def test_threenum(
        self, variables, condition, gray, expected_file, penguins_mod_add
    ):
        fig = plot_comparisons(
            penguins_mod_add, variables=variables, condition=condition, gray=gray
        )
        assert assert_image(fig, expected_file, FIGURES_FOLDER) is None


def test_issue_171_02():
    penguins = pl.read_csv(
        "tests/data/penguins.csv",
        null_values="NA",
    ).drop_nulls()
    mod = smf.ols(
        "body_mass_g ~ flipper_length_mm * species * bill_length_mm * island",
        penguins.to_pandas(),
    ).fit()
    condition = {
        "bill_length_mm": None,
        "flipper_length_mm": 200,
        "island": None,
    }
    fig = plot_comparisons(mod, variables="species", condition=condition)
    assert assert_image(fig, "issue_171_02", FIGURES_FOLDER) is None
