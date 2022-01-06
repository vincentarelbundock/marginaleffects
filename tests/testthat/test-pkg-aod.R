requiet("aod")

test_that("betabin: no validity", {
    data("orob2", package = "aod")
    mod <- betabin(cbind(y, n - y) ~ seed, ~ 1, data = orob2)
    expect_marginaleffects(mod, n_unique = 1)
    expect_predictions(predictions(mod))
})
