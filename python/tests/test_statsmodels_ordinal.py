import numpy as np
import polars as pl
import pandas as pd
from marginaleffects import *
from polars.testing import assert_series_equal
from statsmodels.miscmodels.ordinal_model import OrderedModel

dat = get_dataset("affairs").to_pandas()
dat["affairs"] = pd.Categorical(dat["affairs"], ordered=True)
dat["children"] = pd.Categorical(dat["children"])
dat["gender"] = pd.Categorical(dat["gender"])

mod = OrderedModel.from_formula(
    "affairs ~ children + yearsmarried + gender", data=dat, distr="logit"
).fit(method="bfgs", disp=False)

# Python groups are 0-5 indices; R uses category labels
group_map = {"0": "0", "1": "1", "2": "2", "3": "3", "4": "4-10", "5": ">10"}


def test_predictions_01():
    known = pl.read_csv("tests/r/test_statsmodels_ordinal_predictions_01.csv")
    nd = datagrid(children="yes", yearsmarried=10, gender="woman", model=mod)
    unknown = predictions(mod, newdata=nd).with_columns(
        pl.col("group").replace_strict(group_map)
    )
    known = known.sort("group")
    unknown = unknown.sort("group")
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-3,
    )
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        rtol=1e-2,
    )


def test_avg_predictions_01():
    known = pl.read_csv("tests/r/test_statsmodels_ordinal_avg_predictions_01.csv")
    unknown = avg_predictions(mod).with_columns(
        pl.col("group").replace_strict(group_map)
    )
    known = known.sort("group")
    unknown = unknown.sort("group")
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-3,
    )
    np.testing.assert_allclose(
        known["std.error"].to_numpy(),
        unknown["std_error"].to_numpy(),
        rtol=1e-2,
    )


def test_slopes_01():
    known = pl.read_csv("tests/r/test_statsmodels_ordinal_slopes_01.csv")
    nd = datagrid(children="yes", yearsmarried=10, gender="woman", model=mod)
    unknown = slopes(mod, newdata=nd).with_columns(
        pl.col("group").replace_strict(group_map)
    )
    known = known.sort(["term", "group"])
    unknown = unknown.sort(["term", "group"])
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-3,
    )
    # Split SE checks: yearsmarried SEs use atol because R (polr) and Python
    # (OrderedModel) compute Hessians differently, causing up to ~1e-4
    # absolute divergence on the smallest SEs (~4e-4).
    mask_ym = known["term"] == "yearsmarried"
    mask_other = ~mask_ym
    np.testing.assert_allclose(
        known.filter(mask_other)["std.error"].to_numpy(),
        unknown.filter(mask_other)["std_error"].to_numpy(),
        rtol=1e-2,
    )
    np.testing.assert_allclose(
        known.filter(mask_ym)["std.error"].to_numpy(),
        unknown.filter(mask_ym)["std_error"].to_numpy(),
        atol=1.1e-4,
    )


def test_avg_slopes_01():
    known = pl.read_csv("tests/r/test_statsmodels_ordinal_avg_slopes_01.csv")
    unknown = avg_slopes(mod).with_columns(
        pl.col("group").replace_strict(group_map)
    )
    known = known.sort(["term", "group"])
    unknown = unknown.sort(["term", "group"])
    np.testing.assert_allclose(
        known["estimate"].to_numpy(),
        unknown["estimate"].to_numpy(),
        rtol=1e-3,
    )
    mask_ym = known["term"] == "yearsmarried"
    mask_other = ~mask_ym
    np.testing.assert_allclose(
        known.filter(mask_other)["std.error"].to_numpy(),
        unknown.filter(mask_other)["std_error"].to_numpy(),
        rtol=1e-2,
    )
    np.testing.assert_allclose(
        known.filter(mask_ym)["std.error"].to_numpy(),
        unknown.filter(mask_ym)["std_error"].to_numpy(),
        atol=1.1e-4,
    )
