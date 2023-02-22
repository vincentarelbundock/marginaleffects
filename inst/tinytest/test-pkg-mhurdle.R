source("helpers.R")
using("marginaleffects")

requiet("mhurdle")

tol <- 0.001
tol_se <- 0.001

data("Interview", package = "mhurdle")
m1 <- mhurdle(shows ~ 0 | linc + smsa + age + educ + size, data = Interview, h2 = TRUE, dist = "n", method = "bhhh")
m2 <- mhurdle(shows ~ educ + size | linc | smsa + age, data = Interview,
h2 = FALSE, method = "bhhh", corr = TRUE, finalHessian = TRUE)

# marginaleffects vs. margins (unit-level SEs)
set.seed(1024)
nd <- Interview[sample(seq_len(nrow(Interview)), 10),]
mfx <- slopes(m2, newdata = nd, type = "E")
mar <- margins(m2, type = "response", data = nd, unit_ses = TRUE)

expect_equivalent(mfx[mfx$term == "linc", "estimate"], as.numeric(mar$dydx_linc), tolerance = tol)
expect_equivalent(mfx[mfx$term == "educ", "estimate"], as.numeric(mar$dydx_educ), tolerance = tol)
expect_equivalent(mfx[mfx$term == "age", "estimate"], as.numeric(mar$dydx_age), tolerance = tol)

expect_equivalent(mfx[mfx$term == "linc", "std.error"], mar$SE_dydx_linc, tolerance = tol_se)
expect_equivalent(mfx[mfx$term == "educ", "std.error"], mar$SE_dydx_educ, tolerance = tol_se)
expect_equivalent(mfx[mfx$term == "age", "std.error"], mar$SE_dydx_age, tolerance = tol_se)

# marginaleffects vs. margins: AME 
mfx <- slopes(m2, type = "E")
mfx <- tidy(mfx)
mfx <- mfx[match(c("age", "educ", "linc", "size", "smsa"), mfx$term),]
mar <- margins(m2)
mar <- summary(mar)
expect_equivalent(mfx$estimate, mar$AME, tolerance = tol)
expect_equivalent(mfx$std.error, mar$SE, tolerance = tol_se)



rm(list = ls())