requiet("mhurdle")

tol <- 0.0001
tol_se <- 0.001

data("Interview", package = "mhurdle")
m1 <- mhurdle(shows ~ 0 | linc + smsa + age + educ + size, data = Interview, h2 = TRUE, dist = "n", method = "bhhh")
m2 <- mhurdle(shows ~ educ + size | linc | smsa + age, data = Interview,
    h2 = FALSE, method = "bhhh", corr = TRUE, finalHessian = TRUE)


test_that("marginaleffects vs. margins (unit-level SEs)", {
    set.seed(1024)
    nd <- Interview[sample(seq_len(nrow(Interview)), 10),]
    mfx <- marginaleffects(m2, newdata = nd, type = "E")
    mar <- margins(m2, type = "response", data = nd, unit_ses = TRUE)

    expect_equal(mfx[mfx$term == "linc", "dydx"], mar$dydx_linc, tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx[mfx$term == "educ", "dydx"], mar$dydx_educ, tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx[mfx$term == "age", "dydx"], mar$dydx_age, tolerance = tol, ignore_attr = TRUE)

    expect_equal(mfx[mfx$term == "linc", "std.error"], mar$SE_dydx_linc, tolerance = tol_se, ignore_attr = TRUE)
    expect_equal(mfx[mfx$term == "educ", "std.error"], mar$SE_dydx_educ, tolerance = tol_se, ignore_attr = TRUE)
    expect_equal(mfx[mfx$term == "age", "std.error"], mar$SE_dydx_age, tolerance = tol_se, ignore_attr = TRUE)
})


test_that("marginaleffects vs. margins: AME ", {
    mfx <- marginaleffects(m2, type = "E")
    mfx <- tidy(mfx)
    mfx <- mfx[match(c("age", "educ", "linc", "size", "smsa"), mfx$term),]
    mar <- margins(m2)
    mar <- summary(mar)
    expect_equal(mfx$estimate, mar$AME, ignore_attr = TRUE, tolerance = tol)
    expect_equal(mfx$std.error, mar$SE, ignore_attr = TRUE, tolerance = tol_se)
})
