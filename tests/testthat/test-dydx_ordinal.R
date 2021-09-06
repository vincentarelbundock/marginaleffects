skip_if_not_installed("ordinal")

test_that("ordinal: vs `margins`", {
    data("wine", package = "ordinal")
    tmp <- wine
    tmp$warm <- as.numeric(tmp$temp == "warm")
    mod <- ordinal::clm(rating ~ warm * contact, data = tmp)
    res <- mfx(mod, variables = "warm", variance = NULL)
    mar <- suppressWarnings(data.frame(margins(mod)))
    expect_true(cor(res$dydx_warm, mar$dydx_warm) > 0.999)
    expect_error(mfx(mod, variables = "warm"), regexp = "variance")
    expect_error(mfx(mod, variance = NULL), regexp = "numeric")
})
