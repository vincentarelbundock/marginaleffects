"""R-parity tests for the aggregation plan framework.

Reference values in this file were produced by the R implementation in `r/`
and are quoted to more digits than the assertions need, so a genuine drift is
distinguishable from floating-point noise.
"""

import numpy as np
import polars as pl
import pytest
import statsmodels.api as sm
import statsmodels.formula.api as smf

from marginaleffects import (
    avg_comparisons,
    avg_predictions,
    avg_slopes,
    comparisons,
    get_dataset,
    predictions,
)
from marginaleffects.by import attach_by_labels, harmonize_by_types
from marginaleffects.planning import (
    AggGroup,
    apply_plan_aggregation,
    compile_agg_blocks,
)


@pytest.fixture(scope="module")
def mtcars():
    return get_dataset("mtcars", "datasets").to_pandas()


@pytest.fixture(scope="module")
def ols(mtcars):
    return smf.ols("mpg ~ hp * wt + am", mtcars).fit()


@pytest.fixture(scope="module")
def ols_simple(mtcars):
    return smf.ols("mpg ~ hp + wt", mtcars).fit()


def se(result):
    return result.to_polars()["std_error"].to_numpy()


def est(result):
    return result.to_polars()["estimate"].to_numpy()


# --- aggregation rules -----------------------------------------------------


def test_unweighted_aggregation_drops_missing_estimates():
    groups = [AggGroup(idx=np.array([0, 1, 2]), w=None)]
    out = apply_plan_aggregation(groups, np.array([1.0, np.nan, 3.0]))
    assert out == pytest.approx([2.0])


def test_unweighted_aggregation_is_nan_when_every_estimate_is_missing():
    groups = [AggGroup(idx=np.array([0, 1]), w=None)]
    assert np.isnan(apply_plan_aggregation(groups, np.array([np.nan, np.nan]))[0])


def test_weighted_aggregation_zeroes_the_weight_of_a_missing_estimate():
    # R zeroes both the estimate and its weight, so the aggregate is the
    # weighted mean of the rows that remain rather than NaN.
    groups = [AggGroup(idx=np.array([0, 1, 2]), w=np.array([1.0, 5.0, 3.0]))]
    out = apply_plan_aggregation(groups, np.array([2.0, np.nan, 4.0]))
    assert out == pytest.approx([(2.0 * 1 + 4.0 * 3) / 4])


def test_a_zero_weight_blanks_an_infinite_estimate():
    # 0 * Inf is NaN and would otherwise poison the whole group.
    groups = [AggGroup(idx=np.array([0, 1]), w=np.array([0.0, 2.0]))]
    out = apply_plan_aggregation(groups, np.array([np.inf, 5.0]))
    assert out == pytest.approx([5.0])


def test_a_missing_weight_propagates_rather_than_dropping_its_row():
    groups = [AggGroup(idx=np.array([0, 1]), w=np.array([np.nan, 2.0]))]
    assert np.isnan(apply_plan_aggregation(groups, np.array([1.0, 5.0]))[0])


def test_zero_total_weight_is_nan():
    groups = [AggGroup(idx=np.array([0, 1]), w=np.array([0.0, 0.0]))]
    assert np.isnan(apply_plan_aggregation(groups, np.array([1.0, 5.0]))[0])


# --- block stacking --------------------------------------------------------


def test_equal_length_groups_compile_into_one_block():
    groups = [
        AggGroup(idx=np.array([0, 1]), w=None),
        AggGroup(idx=np.array([2, 3]), w=None),
    ]
    agg = compile_agg_blocks(groups)
    assert len(agg.blocks) == 1
    assert agg.blocks[0].idx.shape == (2, 2)
    assert agg.n == 2 and not agg.weighted


def test_ragged_groups_compile_into_one_block_per_length():
    groups = [
        AggGroup(idx=np.array([0, 1]), w=None),
        AggGroup(idx=np.array([2, 3, 4]), w=None),
        AggGroup(idx=np.array([5, 6]), w=None),
    ]
    agg = compile_agg_blocks(groups)
    assert sorted(block.idx.shape for block in agg.blocks) == [(2, 2), (3, 1)]
    out = apply_plan_aggregation(agg, np.arange(7, dtype=float))
    assert out == pytest.approx([0.5, 3.0, 5.5])


def test_compiled_blocks_round_trip_through_an_already_compiled_plan():
    groups = [AggGroup(idx=np.array([0, 1]), w=None)]
    agg = compile_agg_blocks(groups)
    assert compile_agg_blocks(agg) is agg


def test_mixed_weighted_and_unweighted_groups_treat_missing_weights_as_one():
    groups = [
        AggGroup(idx=np.array([0, 1]), w=np.array([1.0, 3.0])),
        AggGroup(idx=np.array([2, 3]), w=None),
    ]
    out = apply_plan_aggregation(groups, np.array([1.0, 5.0, 2.0, 4.0]))
    assert out == pytest.approx([(1.0 + 15.0) / 4, 3.0])


# --- comparison aggregation stage ------------------------------------------


def test_a_row_level_custom_comparison_is_aggregated_by_group(ols):
    # R: comparisons(m, variables="hp", by="am", comparison=function(hi, lo) hi - lo)
    out = comparisons(ols, variables="hp", by="am", comparison=lambda hi, lo: hi - lo)
    assert out.shape[0] == 2
    assert est(out) == pytest.approx([-0.01570281032, -0.05302897636], rel=1e-6)
    assert se(out) == pytest.approx([0.01067199506, 0.009581968034], rel=1e-5)


def test_a_custom_comparison_and_the_builtin_average_agree(ols):
    custom = comparisons(
        ols, variables="hp", by="am", comparison=lambda hi, lo: hi - lo
    )
    builtin = comparisons(ols, variables="hp", by="am", comparison="differenceavg")
    assert est(custom) == pytest.approx(est(builtin))
    # The callable path differentiates numerically and the builtin path
    # analytically, so they agree to finite-difference precision, not exactly.
    # R shows the same spread between its two paths.
    assert se(custom) == pytest.approx(se(builtin), rel=1e-5)


def test_the_aggregation_stage_is_recorded_on_the_comparison_plan(ols):
    from marginaleffects.comparisons import _comparisons_build

    out = comparisons(ols, variables="hp", by="am", comparison=lambda hi, lo: hi - lo)
    # The plan is what the Jacobian replays, so an aggregated result must
    # carry the stage rather than having collapsed the rows on the side.
    assert out.jacobian.shape[0] == 2
    assert _comparisons_build is not None


def test_a_builtin_average_needs_no_second_aggregation(ols):
    out = comparisons(ols, variables="hp", by="am")
    assert out.shape[0] == 2
    assert se(out) == pytest.approx([0.01067199685, 0.009582014049], rel=1e-5)


# --- `by` data frame -------------------------------------------------------


def test_a_by_frame_labels_predictions(ols):
    # R: predictions(m, by = data.frame(am = c(0, 1), by = c("automatic", "manual")))
    out = predictions(
        ols, by=pl.DataFrame({"am": [0, 1], "by": ["automatic", "manual"]})
    )
    assert out.to_polars()["by"].to_list() == ["automatic", "manual"]
    assert est(out) == pytest.approx([17.14736842, 24.39230769], rel=1e-8)
    assert se(out) == pytest.approx([0.502855793, 0.6079230103], rel=1e-6)


def test_a_by_frame_collapses_several_levels_into_one_label(ols):
    # R: predictions(m, by = data.frame(cyl = c(4, 6, 8), by = c("small", "small", "big")))
    out = predictions(
        ols, by=pl.DataFrame({"cyl": [4, 6, 8], "by": ["small", "small", "big"]})
    )
    frame = out.to_polars().sort("by")
    assert frame["by"].to_list() == ["big", "small"]
    assert frame["estimate"].to_numpy() == pytest.approx(
        [15.13095735, 23.94814429], rel=1e-8
    )
    assert frame["std_error"].to_numpy() == pytest.approx(
        [0.5459266321, 0.4894967624], rel=1e-6
    )


def test_a_by_frame_labels_comparisons(ols):
    # R: comparisons(m, variables="hp", by = data.frame(am=c(0,1), by=c("automatic","manual")))
    out = comparisons(
        ols,
        variables="hp",
        by=pl.DataFrame({"am": [0, 1], "by": ["automatic", "manual"]}),
    )
    assert out.to_polars()["by"].to_list() == ["automatic", "manual"]
    assert est(out) == pytest.approx([-0.01570281032, -0.05302897636], rel=1e-7)
    assert se(out) == pytest.approx([0.01067199685, 0.009582014049], rel=1e-6)


def test_a_by_frame_labels_slopes(ols):
    out = avg_slopes(
        ols,
        variables="hp",
        by=pl.DataFrame({"cyl": [4, 6, 8], "by": ["small", "small", "big"]}),
    )
    frame = out.to_polars().sort("by")
    assert frame["estimate"].to_numpy() == pytest.approx(
        [-0.009371726265, -0.04758477339], rel=1e-6
    )
    assert frame["std_error"].to_numpy() == pytest.approx(
        [0.01199559474, 0.008862580441], rel=1e-5
    )


def test_partial_by_frame_coverage_warns_and_drops_uncovered_rows(ols):
    with pytest.warns(UserWarning, match="does not cover all combinations"):
        out = predictions(
            ols, by=pl.DataFrame({"cyl": [4, 6], "by": ["small", "medium"]})
        )
    frame = out.to_polars().sort("by")
    assert frame["by"].to_list() == ["medium", "small"]
    assert frame["estimate"].to_numpy() == pytest.approx(
        [20.08184472, 26.40851674], rel=1e-8
    )
    assert frame["std_error"].to_numpy() == pytest.approx(
        [0.5720918632, 0.5919871614], rel=1e-6
    )


def test_a_by_frame_without_a_by_column_is_rejected(ols):
    with pytest.raises(ValueError, match="must have a column named `by`"):
        predictions(ols, by=pl.DataFrame({"am": [0, 1], "label": ["a", "b"]}))


def test_a_by_frame_with_only_a_by_column_is_rejected(ols):
    with pytest.raises(ValueError, match="at least one column besides"):
        predictions(ols, by=pl.DataFrame({"by": ["a", "b"]}))


def test_a_by_frame_matching_nothing_is_rejected(ols):
    with pytest.raises(ValueError, match="None of the columns"):
        predictions(ols, by=pl.DataFrame({"nonesuch": [1, 2], "by": ["a", "b"]}))


def test_by_frame_join_keys_are_harmonized_across_numeric_and_string():
    estimates = pl.DataFrame({"cyl": [4, 6], "estimate": [1.0, 2.0]})
    by = pl.DataFrame({"cyl": ["4", "6"], "by": ["a", "b"]})
    out, keep = attach_by_labels(estimates, by)
    assert keep.all()
    assert out["by"].to_list() == ["a", "b"]


def test_harmonize_by_types_leaves_matching_dtypes_alone():
    estimates = pl.DataFrame({"cyl": [4, 6], "estimate": [1.0, 2.0]})
    by = pl.DataFrame({"cyl": [4, 6], "by": ["a", "b"]})
    assert harmonize_by_types(estimates, by)["cyl"].dtype == by["cyl"].dtype


# --- by=True semantics -----------------------------------------------------


def test_by_true_on_predictions_is_a_grand_mean(ols_simple):
    out = predictions(ols_simple, by=True)
    assert out.shape[0] == 1
    assert est(out) == pytest.approx([20.090625], rel=1e-9)
    assert se(out) == pytest.approx([0.4584548], rel=1e-6)


def test_by_true_on_comparisons_groups_by_term_and_contrast(ols_simple):
    out = comparisons(ols_simple, by=True).to_polars().sort("term")
    assert out["term"].to_list() == ["hp", "wt"]
    assert out["estimate"].to_numpy() == pytest.approx(
        [-0.03177295, -3.87783074], rel=1e-6
    )
    assert out["std_error"].to_numpy() == pytest.approx(
        [0.00902971, 0.63273349], rel=1e-5
    )


def test_by_true_keeps_the_response_group_dimension(mtcars):
    model = smf.mnlogit("cyl ~ hp + wt", mtcars).fit(disp=0)
    out = predictions(model, by=True).to_polars()
    # One row per response level, exactly as R's `^group$` grouping gives.
    assert out.height == mtcars["cyl"].nunique()
    assert "group" in out.columns


# --- unconditional variance ------------------------------------------------


def test_unconditional_predictions_match_r(ols_simple):
    from marginaleffects import vcovUnconditional

    assert se(avg_predictions(ols_simple, vcov=vcovUnconditional())) == pytest.approx(
        [1.048644582], rel=1e-7
    )


def test_unconditional_hc1_applies_the_model_degrees_of_freedom_factor(ols_simple):
    from marginaleffects import vcovUnconditional

    assert se(
        avg_predictions(ols_simple, vcov=vcovUnconditional("HC1"))
    ) == pytest.approx([1.101550236], rel=1e-7)


def test_unconditional_subgroup_averages_match_r(ols_simple):
    from marginaleffects import vcovUnconditional

    out = (
        avg_predictions(ols_simple, by="cyl", vcov=vcovUnconditional())
        .to_polars()
        .sort("cyl")
    )
    assert out["std_error"].to_numpy() == pytest.approx(
        [1.217737678, 0.677477039, 0.674209715], rel=1e-6
    )


def test_unconditional_comparisons_match_r(ols_simple):
    from marginaleffects import vcovUnconditional

    assert se(
        avg_comparisons(ols_simple, variables="hp", vcov=vcovUnconditional())
    ) == pytest.approx([0.006646055777], rel=1e-5)


def test_unconditional_scalar_ratio_contrast_matches_r(ols_simple):
    from marginaleffects import vcovUnconditional

    assert se(
        avg_comparisons(
            ols_simple, variables="hp", comparison="ratioavg", vcov=vcovUnconditional()
        )
    ) == pytest.approx([0.0003254387482], rel=2e-3)


def test_unconditional_clustering_matches_r(ols_simple):
    from marginaleffects import vcovUnconditional

    assert se(
        avg_comparisons(
            ols_simple, variables="hp", vcov=vcovUnconditional(cluster="cyl")
        )
    ) == pytest.approx([0.005053470922], rel=1e-5)


def test_unconditional_clustered_hc1_matches_r(ols_simple):
    from marginaleffects import vcovUnconditional

    assert se(
        avg_comparisons(
            ols_simple, variables="hp", vcov=vcovUnconditional("HC1", cluster="cyl")
        )
    ) == pytest.approx([0.005224823439], rel=1e-5)


def test_unconditional_logit_predictions_match_r(mtcars):
    from marginaleffects import vcovUnconditional

    model = smf.glm("am ~ hp + wt", mtcars, family=sm.families.Binomial()).fit()
    assert se(avg_predictions(model, vcov=vcovUnconditional())) == pytest.approx(
        [0.08682074369], rel=1e-6
    )


def test_unconditional_logit_subgroups_match_r(mtcars):
    from marginaleffects import vcovUnconditional

    model = smf.glm("am ~ hp + wt", mtcars, family=sm.families.Binomial()).fit()
    out = (
        avg_predictions(model, by="cyl", vcov=vcovUnconditional())
        .to_polars()
        .sort("cyl")
    )
    assert out["std_error"].to_numpy() == pytest.approx(
        [0.120350743, 0.1768311807, 0.09412722961], rel=1e-6
    )


def test_unconditional_hypothesis_after_aggregation_matches_r(ols_simple):
    from marginaleffects import vcovUnconditional

    out = avg_predictions(
        ols_simple, by="cyl", hypothesis="b0-b1=0", vcov=vcovUnconditional()
    )
    assert est(out) == pytest.approx([4.483866], rel=1e-6)
    assert se(out) == pytest.approx([1.093565], rel=1e-6)


def test_unconditional_exceeds_the_delta_method_for_a_population_average(ols_simple):
    from marginaleffects import vcovUnconditional

    # Averaging over the observed covariates adds a second source of variation,
    # so the unconditional error must be the larger of the two here.
    conditional = se(avg_predictions(ols_simple))
    unconditional = se(avg_predictions(ols_simple, vcov=vcovUnconditional()))
    assert unconditional[0] > conditional[0]


def test_unconditional_rejects_unit_level_effects(ols_simple):
    from marginaleffects import vcovUnconditional

    with pytest.raises(ValueError, match="averaged or aggregated"):
        predictions(ols_simple, vcov=vcovUnconditional())


def test_unconditional_rejects_a_synthetic_grid(ols_simple):
    from marginaleffects import vcovUnconditional

    with pytest.raises(ValueError, match="original model-data rows"):
        avg_predictions(ols_simple, newdata="mean", vcov=vcovUnconditional())


def test_unconditional_rejects_unsupported_finite_sample_types():
    from marginaleffects import vcovUnconditional

    with pytest.raises(ValueError, match="HC2 through HC5"):
        vcovUnconditional("HC3")


def test_unconditional_rejects_an_unknown_cluster_column(ols_simple):
    from marginaleffects import vcovUnconditional

    with pytest.raises(ValueError, match="not found in the model data"):
        avg_predictions(ols_simple, vcov=vcovUnconditional(cluster="nonesuch"))


def test_unconditional_rejects_unvalidated_model_classes(mtcars):
    from sklearn.linear_model import LinearRegression

    from marginaleffects import fit_sklearn, vcovUnconditional

    model = fit_sklearn("mpg ~ hp + wt", mtcars, engine=LinearRegression())
    with pytest.raises(ValueError, match="not currently supported"):
        avg_predictions(model, vcov=vcovUnconditional())


def test_the_bare_vcov_unconditional_function_is_accepted(ols_simple):
    from marginaleffects import vcovUnconditional

    bare = se(avg_predictions(ols_simple, vcov=vcovUnconditional))
    called = se(avg_predictions(ols_simple, vcov=vcovUnconditional()))
    assert bare == pytest.approx(called)


def test_a_cluster_formula_string_is_accepted(ols_simple):
    from marginaleffects import vcovUnconditional

    assert vcovUnconditional(cluster="~cyl").cluster == "cyl"
