source("helpers.R")
using("marginaleffects")

requiet("truncreg")
requiet("margins")


# truncreg: no validity check
data("tobin", package = "survival")
model <- truncreg(durable ~ age + quant, data = tobin, subset = durable > 0)
mfx <- slopes(model)
tid <- tidy(mfx)
expect_inherits(tid, "data.frame")
expect_equivalent(nrow(tid), 2)
expect_false(any(tid$estimate == 0))
expect_false(anyNA(tid$estimate))
expect_false(any(tid$std.error == 0))
expect_false(anyNA(tid$std.error))



# truncreg vs. Stata
# numeric differences could be resolved with different tolerance, but
# finding the correct threshold by trial and error is difficult on CRAN
stata <- readRDS(testing_path("stata/stata.rds"))$truncreg_truncreg_01
data("tobin", package = "survival")
model <- truncreg::truncreg(durable ~ age + quant, 
                            data = tobin, subset = durable > 0)
mfx <- merge(tidy(slopes(model)), stata)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .0001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .001)
# margins
mar <- margins(model, unit_ses = TRUE)
mfx <- slopes(model)
expect_true(expect_margins(mfx, mar, tolerance = .0001))




rm(list = ls())