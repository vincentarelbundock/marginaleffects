# comparisons fail on github but work interactively
skip_on_ci() 
# ripley emergency email about tiny numerical differences
skip_on_cran() 
requiet("plm")
requiet("margins")
requiet("broom")
tol <- .0001
tol_se <- .001

data("Grunfeld", package = "plm")

### marginaleffects

test_that("pooling vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$plm_pooling
    pool <- plm(inv ~ value * capital, data = Grunfeld, model = "pooling")
    mfx <- merge(tidy(marginaleffects(pool)), stata)
    expect_marginaleffects(pool, n_unique = 1)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = tol)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se)
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
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = tol)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se)

    # margins
    mfx <- tidy(marginaleffects(mod))
    mar <- tidy(margins(mod))
    mfx <- mfx[order(mfx$term),]
    expect_equal(mfx$estimate, mar$estimate, ignore_attr = TRUE, tolerance = tol)
    expect_equal(mfx$std.error, mar$std.error, ignore_attr = TRUE, tolerance = tol_se)
})


test_that("no validity checks", {
    amemiya <- plm(inv ~ value * capital,
                   data = Grunfeld, model = "random", random.method = "amemiya",
                   effect = "twoways")
    expect_marginaleffects(amemiya)

    # margins
    mfx <- tidy(marginaleffects(amemiya))
    mar <- tidy(margins(amemiya))
    mfx <- mfx[order(mfx$term),]
    expect_equal(mfx$estimate, mar$estimate, ignore_attr = TRUE, tolerance = tol)
    expect_equal(mfx$std.error, mar$std.error, ignore_attr = TRUE, tolerance = tol_se)


    walhus <- plm(inv ~ value * capital,
                  data = Grunfeld, model = "random", random.method = "walhus",
                  effect = "twoways")
    expect_marginaleffects(walhus)

    # margins
    mfx <- tidy(marginaleffects(walhus))
    mar <- tidy(margins(walhus))
    mfx <- mfx[order(mfx$term),]
    expect_equal(mfx$estimate, mar$estimate, ignore_attr = TRUE, tolerance = tol)
    expect_equal(mfx$std.error, mar$std.error, ignore_attr = TRUE, tolerance = tol_se)
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
    expect_predictions(pred1, n_row = nrow(Grunfeld))
    expect_predictions(pred2, n_row = 6)
})
