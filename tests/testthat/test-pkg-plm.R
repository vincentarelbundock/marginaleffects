skip_if_not_installed("plm")
requiet("plm")

data("Grunfeld", package = "plm")


### marginaleffects

test_that("pooling vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$plm_pooling
    pool <- plm(inv ~ value * capital, data = Grunfeld, model = "pooling")
    mfx <- merge(tidy(marginaleffects(pool)), stata)
    expect_marginaleffects(pool, n_unique = 1)
    expect_equal(mfx$estimate, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})


test_that("Swamy-Arora vs. Stata", {
    # numeric differences could be resolved with different tolerance, but
    # finding the correct threshold by trial and error is difficult on CRAN
    skip_on_cran()
    stata <- readRDS(test_path("stata/stata.rds"))$plm_sa
    mod <- plm(inv ~ value * capital, data = Grunfeld,
               model = "random", effect = "individual")
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_marginaleffects(mod)
    expect_equal(mfx$estimate, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})


test_that("no validity checks", {
    amemiya <- plm(inv ~ value * capital,
                   data = Grunfeld, model = "random", random.method = "amemiya",
                   effect = "twoways")
    expect_marginaleffects(amemiya)
    walhus <- plm(inv ~ value * capital,
                  data = Grunfeld, model = "random", random.method = "walhus",
                  effect = "twoways")
    expect_marginaleffects(walhus)
})


test_that("within error", {
    # within model are not supported by `predict.plm`
    stata <- readRDS(test_path("stata/stata.rds"))$plm_within
    mod <- plm(inv ~ value * capital, data = Grunfeld, model = "within", effect = "twoways")
    expect_error(marginaleffects(mod), regexp = "appear.*support")
})


### predictions

test_that("predictions: pooling no validity", {
    pool <- plm(inv ~ value * capital, data = Grunfeld, model = "pooling")
    pred1 <- predictions(pool)
    pred2 <- predictions(pool, newdata = head(Grunfeld))
    expect_predictions(pred1, n_row = 1, se = FALSE)
    expect_predictions(pred2, n_row = 6, se = FALSE)
})
