import numpy as np

from marginaleffects.estimands import estimands
from marginaleffects.plan import (
    AggGroup,
    CompGroup,
    ComparisonPlan,
    Hyp,
    PredictionPlan,
    comparison_plan_apply,
    prediction_plan_apply,
)


def test_prediction_plan_apply_aligns_aggregates_and_applies_hypothesis():
    plan = PredictionPlan(
        n_pred=4,
        exog=np.eye(4),
        align=np.array([2, 0, -1, 1]),
        has_na=True,
        agg=[
            AggGroup(idx=np.array([0, 1]), w=np.array([1.0, 3.0])),
            AggGroup(idx=np.array([2, 3]), w=None),
        ],
        hyp=Hyp(
            kind="matrix",
            apply=lambda est: est @ np.array([[1.0], [-1.0]]),
            H=np.array([[1.0], [-1.0]]),
        ),
        n_out=1,
    )

    out = prediction_plan_apply(plan, np.array([10.0, 20.0, 30.0, 40.0]))

    np.testing.assert_allclose(out, [-5.0], equal_nan=True)


def test_comparison_plan_apply_replays_group_outputs_in_order():
    groups = [
        CompGroup(
            idx=np.array([0, 2]),
            out_idx=np.array([0, 1]),
            scalar=False,
            fun=estimands["difference"],
            fun_key="difference",
            x=np.array([0.0, 2.0]),
            w=None,
        ),
        CompGroup(
            idx=np.array([1, 3]),
            out_idx=np.array([2]),
            scalar=True,
            fun=estimands["ratioavgwts"],
            fun_key="ratioavgwts",
            x=np.array([1.0, 3.0]),
            w=np.array([1.0, 3.0]),
        ),
    ]
    plan = ComparisonPlan(
        n_pred=4,
        exog_hi=np.eye(4),
        exog_lo=np.eye(4),
        exog_nd=None,
        need_y=False,
        align=None,
        eps=1e-4,
        groups=groups,
        n_comp=3,
        hyp=None,
        has_na=False,
    )

    out = comparison_plan_apply(
        plan,
        hi=np.array([3.0, 6.0, 8.0, 12.0]),
        lo=np.array([1.0, 3.0, 2.0, 4.0]),
    )

    expected_avg = np.average([6.0, 12.0], weights=[1.0, 3.0]) / np.average(
        [3.0, 4.0], weights=[1.0, 3.0]
    )
    np.testing.assert_allclose(out, [2.0, 6.0, expected_avg])
