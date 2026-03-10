import statsmodels.formula.api as smf
from marginaleffects import *
from marginaleffects.plot.predictions import *
from tests.utilities import *
import pytest
from tests.helpers import *

pytestmark = pytest.mark.skipif(True, reason="Plot tests skipped on all platforms")

FIGURES_FOLDER = "plot_predictions"


@pytest.mark.plot
class TestPlotPredictions:
    def test_gray_discrete_interval_len3(self):
        data = (
            get_dataset("mtcars", "datasets")
            .sort("gear")
            .with_columns(pl.col("gear").cast(pl.String).cast(pl.Categorical))
            .to_pandas()
        )

        mod = smf.ols("mpg ~ wt + hp + C(gear)", data=data).fit()

        fig = plot_predictions(mod, condition=["gear", "wt", "hp"], gray=True)
        assert (
            assert_image(fig, "test_gray_discrete_interval_len3", FIGURES_FOLDER)
            is None
        )

    def test_gray_discrete_interval_len1(self):
        data = get_dataset("mtcars", "datasets")

        data = data.drop_nulls()
        mod = smf.glm("mpg ~ hp * wt * cyl * gear", data=data).fit()

        fig = plot_predictions(mod, condition=["gear"], gray=True)
        assert (
            assert_image(fig, "test_gray_discrete_interval_len1", FIGURES_FOLDER)
            is None
        )

    def test_gray_discrete_not_interval(self):
        data = (
            get_dataset("mtcars", "datasets")
            .sort("gear")
            .with_columns(pl.col("gear").cast(pl.String).cast(pl.Categorical))
            .to_pandas()
        )
        mod = smf.ols("mpg ~ wt + C(gear)", data=data).fit()

        fig = plot_predictions(mod, condition=["gear"], vcov=False, gray=True)
        assert (
            assert_image(fig, "test_gray_discrete_not_interval", FIGURES_FOLDER) is None
        )

    def test_gray_not_discrete_interval_len3(self):
        data = get_dataset("mtcars", "datasets")

        data = data.drop_nulls()
        mod = smf.glm("mpg ~ hp * wt * cyl * gear", data=data).fit()

        fig = plot_predictions(mod, condition=["gear", "hp", "cyl"], gray=True)
        assert (
            assert_image(fig, "test_gray_not_discrete_interval_len3", FIGURES_FOLDER)
            is None
        )

    def test_gray_not_discrete_interval_len1(self):
        data = get_dataset("mtcars", "datasets")

        data = data.drop_nulls()
        mod = smf.glm("mpg ~ hp * wt * cyl * gear", data=data).fit()

        fig = plot_predictions(mod, condition=["gear"], gray=True)
        assert (
            assert_image(fig, "test_gray_not_discrete_interval_len1", FIGURES_FOLDER)
            is None
        )

    @pytest.mark.parametrize(
        "by, expected_figure_filename",
        [
            ("species", "by_01"),
            (["species", "island"], "by_02"),
        ],
    )
    def test_by(self, by, expected_figure_filename, penguins_model):
        fig = plot_predictions(penguins_model, by=by)
        assert assert_image(fig, expected_figure_filename, FIGURES_FOLDER) is None

    @pytest.mark.parametrize(
        "input_condition, expected_figure_filename",
        [
            (
                {"flipper_length_mm": list(range(180, 220)), "species": None},
                "condition_01",
            ),
            (["bill_length_mm", "species", "island"], "condition_02"),
        ],
    )
    def test_condition(self, input_condition, expected_figure_filename, penguins_model):
        fig = plot_predictions(penguins_model, condition=input_condition)
        assert assert_image(fig, expected_figure_filename, FIGURES_FOLDER) is None

    @pytest.mark.parametrize(
        "input_condition, expected_figure_filename",
        [
            (["qsec", "am"], "issue_57_01"),
            (
                {
                    "am": None,
                    "qsec": [mtcars["qsec"].min(), mtcars["qsec"].max()],
                },
                "issue_57_02",
            ),
            ({"wt": None, "am": None}, "issue_57_03"),
        ],
    )
    def test_issue_57(self, input_condition, expected_figure_filename):
        mod = smf.ols("mpg ~ wt + am + qsec", mtcars.to_pandas()).fit()

        fig = plot_predictions(mod, condition=input_condition)
        assert assert_image(fig, expected_figure_filename, FIGURES_FOLDER) is None

    def issue_62(self):
        import types

        mod = smf.ols("mpg ~ hp * wt * am", data=mtcars).fit()
        cond = {
            "hp": None,
            "wt": [
                mtcars["wt"].mean() - mtcars["wt"].std(),
                mtcars["wt"].mean(),
                mtcars["wt"].mean() + mtcars["wt"].std(),
            ],
            "am": None,
        }
        p = plot_predictions(mod, condition=cond)
        assert isinstance(p, types.ModuleType)

    @pytest.mark.parametrize(
        "input_condition, expected_figure_filename",
        [
            (["flipper_length_mm", "species"], "issue_114_01"),
            (["flipper_length_mm", "bill_length_mm"], "issue_114_02"),
            (
                {"flipper_length_mm": None, "species": ["Adelie", "Chinstrap"]},
                "issue_114_03",
            ),
            ({"flipper_length_mm": None, "bill_length_mm": "threenum"}, "issue_114_04"),
            ({"flipper_length_mm": None, "bill_length_mm": "fivenum"}, "issue_114_05"),
            ({"flipper_length_mm": None, "bill_length_mm": "minmax"}, "issue_114_06"),
            (
                {
                    "flipper_length_mm": None,
                    "species": ["Adelie", "Chinstrap"],
                    "bill_length_mm": None,
                },
                "issue_114_07",
            ),
            (
                {
                    "flipper_length_mm": None,
                    "species": ["Adelie", "Chinstrap"],
                    "bill_length_mm": None,
                    "island": None,
                },
                "issue_114_08",
            ),
        ],
        ids=[
            "issue_114_01",
            "issue_114_02",
            "issue_114_03",
            "issue_114_04",
            "issue_114_05",
            "issue_114_06",
            "issue_114_07",
            "issue_114_08",
        ],
    )
    def test_issue_114(self, input_condition, expected_figure_filename, penguins_model):
        fig = plot_predictions(penguins_model, condition=input_condition)
        assert assert_image(fig, expected_figure_filename, FIGURES_FOLDER) is None


def test_issue_171():
    dat = get_dataset("thornton")
    mod = smf.logit(
        "outcome ~ incentive + agecat + distance", data=dat.to_pandas()
    ).fit()
    cond = {"distance": None, "agecat": ">35", "incentive": 0}
    fig = plot_predictions(mod, condition=cond)
    assert assert_image(fig, "issue_171", FIGURES_FOLDER) is None


def test_points_adds_raw_layer():
    mod = smf.ols("mpg ~ wt", data=mtcars.to_pandas()).fit()
    fig_points = plot_predictions(mod, condition=["wt"], points=0.4)
    assert assert_image(fig_points, "points_adds_raw_layer", FIGURES_FOLDER) is None


def test_points_out_of_bounds_raises():
    mod = smf.ols("mpg ~ wt", data=mtcars.to_pandas()).fit()
    with pytest.raises(ValueError):
        plot_predictions(mod, condition=["wt"], points=1.5)
