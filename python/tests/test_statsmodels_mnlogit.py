import polars as pl
import statsmodels.formula.api as smf
from marginaleffects import *
from polars.testing import assert_series_equal

dat = (
    get_dataset("penguins", "palmerpenguins")
    .drop_nulls(["species", "island", "bill_length_mm", "flipper_length_mm"])
    .with_columns(
        pl.col("island").replace_strict({"Biscoe": 1, "Dream": 2, "Torgersen": 3})
    )
)
mod = smf.mnlogit("island ~ bill_length_mm + flipper_length_mm", dat.to_pandas()).fit()


def test_avg_predictions_01():
    known = pl.read_csv("tests/r/test_statsmodels_mnlogit_avg_predictions_01.csv")
    dict = {"0": "Biscoe", "1": "Dream", "2": "Torgersen"}
    unknown = avg_predictions(mod)
    unknown = unknown.with_columns(pl.col("group").replace_strict(dict))
    assert_series_equal(
        known["estimate"], unknown["estimate"], rel_tol=1e-2, check_names=False
    )
    assert_series_equal(
        known["std.error"], unknown["std_error"], rel_tol=1e-2, check_names=False
    )


# def test_avg_comparisons_01():
#     r_code = """
#     library(marginaleffects)
#     library(nnet)
#     dat = get_dataset("penguins", "palmerpenguins")
#     dat = dat[complete.cases(dat[c("species", "island", "bill_length_mm", "flipper_length_mm")]), ]
#     dat$island = factor(dat$island, levels = c("Biscoe", "Dream", "Torgersen"))
#     mod = multinom(island ~ bill_length_mm + flipper_length_mm, data = dat, trace=FALSE)
#     pred = avg_comparisons(mod)
#     data.frame(pred)
#     """
#     r = r2pl(r_code)
#     dict = {"0": "Biscoe", "1": "Dream", "2": "Torgersen"}
#     py = avg_comparisons(mod)#.with_columns(pl.col("group").replace_strict(dict))
#     assert_series_equal(r["estimate"], py["estimate"], rel_tol=1e-2, check_names=False)

# pytest.skip(reason="Skipping tests in this file", allow_module_level=True)


# def test_predictions_01():
#     unknown = (
#         predictions(mod)
#         .with_columns(
#             pl.col("group").replace_strict(r), (pl.col("rowid") + 1).alias("rowid")
#         )
#         .rename({"estimate": "estimate_py"})
#     )

#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_predictions_01.csv")
#     compare = known.join(unknown, on=["rowid", "group"], how="inner")
#     compare.select("std.error", "std_error").head()
#     compare.select("estimate", "estimate_py").tail()
#     assert_series_equal(
#         compare["estimate"], compare["estimate_py"], rel_tol=1e-2, check_names=False
#     )
#     assert_series_equal(
#         compare["std.error"], compare["std_error"], rel_tol=1e-1, check_names=False
#     )


# def test_predictions_02():
#     unknown = predictions(mod, by="species")
#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_predictions_02.csv")
#     assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)


# def test_comparisons_01():
#     unknown = (
#         comparisons(mod)
#         .with_columns(pl.col("group").replace_strict(r))
#         .sort(["term", "group"])
#     )
#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_comparisons_01.csv").sort(
#         ["term", "group"]
#     )
#     assert_series_equal(known["estimate"].head(), unknown["estimate"].head(), rel_tol=1e-1)

#     unknown = comparisons(mod)
#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_comparisons_01.csv")
#     assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)


# def test_comparisons_02():
#     unknown = (
#         comparisons(mod, by=["group", "species"])
#         .with_columns(pl.col("group").replace_strict(r))
#         .sort(["term", "group", "species"])
#     )
#     known = pl.read_csv("tests/r/test_statsmodels_mnlogit_comparisons_02.csv").sort(
#         ["term", "group", "species"]
#     )
#     assert_series_equal(known["estimate"], unknown["estimate"], rel_tol=1e-2)
