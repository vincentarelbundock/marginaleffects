testthat::skip_if_not_installed("truncreg")
testthat::skip_if_not_installed("margins")
requiet("truncreg")
requiet("margins")

# Basic expectation tests
data("tobin", package = "survival")
mod_simple <- truncreg::truncreg(durable ~ age, data = tobin, subset = durable > 0)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# truncreg: no validity check
data("tobin", package = "survival")
model <- truncreg(durable ~ age + quant, data = tobin, subset = durable > 0)
tid <- avg_slopes(model)
expect_s3_class(tid, "data.frame")
expect_equal(nrow(tid), 2, ignore_attr = TRUE)
expect_false(any(tid$estimate == 0))
expect_false(anyNA(tid$estimate))
expect_false(any(tid$std.error == 0))
expect_false(anyNA(tid$std.error))


# truncreg vs. Stata
# numeric differences could be resolved with different tolerance, but
# finding the correct threshold by trial and error is difficult on CRAN
stata <- readRDS(testing_path("stata/stata.rds"))$truncreg_truncreg_01
data("tobin", package = "survival")
model <- truncreg::truncreg(durable ~ age + quant, data = tobin, subset = durable > 0)
mfx <- merge(avg_slopes(model), stata)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001, ignore_attr = TRUE)
# margins
mar <- margins(model, unit_ses = TRUE)
mfx <- slopes(model)
expect_margins2(mfx, mar, tolerance = .001)
