from marginaleffects import get_dataset, avg_comparisons, avg_predictions
from statsmodels.formula.api import logit
import pytest

dat = get_dataset("thornton")
mod = logit("outcome ~ incentive", dat.to_pandas()).fit()


def test_no_missing_value_warning():
    with pytest.warns(UserWarning, match="Dropping rows with missing observations."):
        avg_comparisons(mod, variables="incentive")


def test_polars_dataframe_with_one_row():
    df = dat.select(["outcome", "incentive"]).drop_nulls()
    ate = avg_comparisons(mod, variables="incentive", newdata=df)
    assert ate.shape[0] == 1, "The DataFrame should have exactly one row"


def test_newdata_lwd():
    hiv = get_dataset("thornton").to_pandas()
    mod = logit("outcome ~ agecat + incentive", hiv).fit()
    p = avg_predictions(mod, by="agecat", newdata=hiv)
    assert p.shape[0] == 3
