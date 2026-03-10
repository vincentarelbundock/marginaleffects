import pytest
from marginaleffects import *
from marginaleffects.plot.slopes import *
from tests.utilities import *
from tests.helpers import *

pytestmark = pytest.mark.skipif(True, reason="Plot tests skipped on all platforms")

FIGURES_FOLDER = "plot_slopes"


@pytest.mark.plot
class TestPlotSlopes:
    def test_by(self, penguins_mod_add):
        fig = plot_slopes(penguins_mod_add, variables="species", by="island")
        assert assert_image(fig, "by_01", FIGURES_FOLDER) is None

        # fig = plot_slopes(mod, variables="bill_length_mm", by=["species", "island"])
        # assert assert_image(fig, "by_02", FIGURES_FOLDER) is None

    def test_condition(self, penguins_mod_add):
        fig = plot_slopes(
            penguins_mod_add,
            variables="bill_length_mm",
            condition=["flipper_length_mm", "species"],
            eps_vcov=1e-2,
        )
        assert assert_image(fig, "condition_01", FIGURES_FOLDER) is None

        fig = plot_slopes(
            penguins_mod_add, variables="species", condition="bill_length_mm"
        )
        assert assert_image(fig, "condition_02", FIGURES_FOLDER) is None

        fig = plot_slopes(
            penguins_mod_add, variables="island", condition="bill_length_mm", eps=1e-2
        )
        assert assert_image(fig, "condition_03", FIGURES_FOLDER) is None

        fig = plot_slopes(
            penguins_mod_add,
            variables="species",
            condition=["bill_length_mm", "species", "island"],
        )
        assert assert_image(fig, "condition_04", FIGURES_FOLDER) is None

    @pytest.mark.parametrize(
        "input_condition, input_variables, gray, expected_figure_filename",
        [
            (
                {"wt": None, "cyl": None, "disp": None, "qsec": None},
                "hp",
                False,
                "issue114_slopes_01",
            ),
            (
                {"wt": None, "cyl": None, "disp": None, "qsec": None},
                "cyl",
                False,
                "issue114_slopes_02",
            ),
            (
                {"cyl": None, "wt": "minmax", "disp": None, "qsec": None},
                "hp",
                False,
                "issue114_slopes_03",
            ),
            (
                {"wt": None, "cyl": None, "disp": None, "qsec": None},
                "hp",
                True,
                "issue114_slopes_01_gray",
            ),
            (
                {"wt": None, "cyl": None, "disp": None, "qsec": None},
                "cyl",
                True,
                "issue114_slopes_02_gray",
            ),
            # this can be commented out when the slopes issue is fixed, it will allow to generate the correct figures
            # (
            #     {"cyl": None, "wt": "minmax", "disp": None, "qsec": None},
            #     "hp",
            #     True,
            #     "issue114_slopes_03_gray",
            # ),
        ],
    )
    def test_issue114_slopes_and_gray(
        self,
        input_condition,
        input_variables,
        gray,
        expected_figure_filename,
        mtcars_mod,
    ):
        fig = plot_slopes(
            mtcars_mod, variables=input_variables, condition=input_condition, gray=gray
        )
        assert assert_image(fig, expected_figure_filename, FIGURES_FOLDER) is None

    @pytest.mark.parametrize(
        "input_condition, input_variables, gray, expected_figure_filename",
        [
            (
                {"flipper_length_mm": None, "species": ["Adelie", "Chinstrap"]},
                ["species"],
                False,
                "issue_114_03",
            ),
            (
                {"bill_length_mm": None, "flipper_length_mm": "minmax"},
                ["island"],
                False,
                "issue_114_04",
            ),
            (
                {"flipper_length_mm": None, "bill_length_mm": "fivenum"},
                ["island"],
                False,
                "issue_114_05",
            ),
            (
                {"flipper_length_mm": None, "bill_length_mm": "threenum"},
                ["island"],
                False,
                "issue_114_06",
            ),
            (
                {"flipper_length_mm": None, "bill_length_mm": "threenum"},
                ["island"],
                True,
                "issue_114_06_gray",
            ),
            (
                {
                    "flipper_length_mm": None,
                    "bill_length_mm": None,
                    "island": None,
                },
                ["island"],
                False,
                "issue_114_07",
            ),
            (
                {
                    "flipper_length_mm": None,
                    "bill_length_mm": None,
                    "island": None,
                },
                ["species"],
                False,
                "issue_114_08",
            ),
        ],
    )
    def test_issue_114(
        self,
        input_condition,
        input_variables,
        gray,
        expected_figure_filename,
        penguins_mod_add,
    ):
        fig = plot_slopes(
            penguins_mod_add,
            variables=input_variables,
            condition=input_condition,
            gray=gray,
        )
        assert assert_image(fig, expected_figure_filename, FIGURES_FOLDER) is None
