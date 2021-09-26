skip_if_not_installed("plm")
requiet("plm")

data("Grunfeld", package = "plm")

test_that("pooling vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$plm_pooling
    pool <- plm(inv ~ value * capital, data = Grunfeld, model = "pooling")
    mfx <- merge(tidy(marginaleffects(pool)), stata)
    expect_mfx(pool, n_unique = 1)
    expect_equal(mfx$estimate, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})


test_that("Swamy-Arora vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$plm_sa
    mod <- plm(inv ~ value * capital, data = Grunfeld,
               model = "random", effect = "individual")
    mfx <- merge(tidy(marginaleffects(mod)), stata)
    expect_mfx(mod)
    expect_equal(mfx$estimate, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001)
})


test_that("no validity checks", {
    amemiya <- plm(inv ~ value * capital,
                   data = Grunfeld, model = "random", random.method = "amemiya",
                   effect = "twoways")
    expect_mfx(amemiya)
    walhus <- plm(inv ~ value * capital,
                  data = Grunfeld, model = "random", random.method = "walhus",
                  effect = "twoways")
    expect_mfx(walhus)
})


test_that("within error", {
    # within model are not supported by `predict.plm`
    stata <- readRDS(test_path("stata/stata.rds"))$plm_within
    mod <- plm(inv ~ value * capital, data = Grunfeld, model = "within", effect = "twoways")
    expect_error(marginaleffects(mod), regexp = "appear.*support")
})
