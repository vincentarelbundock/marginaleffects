exit_file("tobit1 is not on CRAN")
source("helpers.R")
using("marginaleffects")

if (!require("tobit1")) exit_file("tobit1")
requiet("tobit1")
requiet("broom")
tol <- 0.001
tol_se <- 0.001

charitable$logdon <- log(charitable$donation) - log(25)
data("feesadm", package = "tobit1")
mod <- tobit1(fees ~ expense + I(expense ^ 2) + region, feesadm)

# marginaleffects vs. margins (custom method shipped by tobit1)
mfx1 <- slopes(mod, type = "linpred")
mfx1 <- tidy(mfx1)
mar1 <- margins(mod, what = "linpred")
mar1 <- summary(mar1)
expect_equivalent(mfx1$estimate, mar1$AME, tolerance = tol)
expect_equivalent(mfx1$std.error, mar1$SE, tolerance = tol)

mfx2 <- slopes(mod, type = "prob")
mfx2 <- tidy(mfx2)
mar2 <- margins(mod, what = "prob")
mar2 <- summary(mar2)
expect_equivalent(mfx2$estimate, mar2$AME, tolerance = tol_se)
expect_equivalent(mfx2$std.error, mar2$SE, tolerance = tol_se)


# predictions vs. built-in
mar <- prediction(mod, what = "expvalue")
mfx <- predictions(mod, type = "expvalue")
expect_equivalent(mar$fitted, mfx$estimate)




rm(list = ls())