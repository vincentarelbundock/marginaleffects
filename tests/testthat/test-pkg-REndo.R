test_that("REndo package works", {
    skip_if_not_installed("REndo")

    withr_library("REndo")

    data("dataLatentIV", package = "REndo")
    mod <- latentIV(y ~ P, data = dataLatentIV, verbose = FALSE)
    s <- avg_slopes(mod, type = "response")
    expect_s3_class(s, "slopes")

    data("dataCopCont", package = "REndo")
    set.seed(1002)
    mod <- copulaCorrection(
        formula = y ~ X1 + X2 + P | continuous(P),
        data = dataCopCont,
        num.boots = 50,
        verbose = FALSE
    ) |>
        suppressWarnings()
    s <- avg_slopes(mod)
    expect_s3_class(s, "slopes")
})
