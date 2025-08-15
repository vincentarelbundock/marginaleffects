test_that("plm package works", {
    skip_if_not_installed("margins")
    skip_if_not_installed("broom")
    skip_if_not_installed("plm")

    withr_library("margins")
    withr_library("broom")
    withr_library("plm")

    tol <- .001
    tol_se <- .01 # BDR emergency email about tiny numerical differences

    dat <- get_dataset("Grunfeld", "plm")
    dat$rownames <- NULL
    dat <- pdata.frame(dat)
    pool <- plm(inv ~ value * capital, data = dat, model = "pooling")
    swamy <- plm(
        inv ~ value * capital,
        data = dat,
        model = "random",
        variables = "individual"
    )
    amemiya <- plm(
        inv ~ value * capital,
        data = dat,
        model = "random",
        random.method = "amemiya",
        variables = "twoways"
    )
    walhus <- plm(
        inv ~ value * capital,
        data = dat,
        model = "random",
        random.method = "walhus",
        variables = "twoways"
    )

    ### marginaleffects

    # pooling vs. Stata
    stata <- readRDS(test_path("stata/stata.rds"))$plm_pooling
    mfx <- merge(avg_slopes(pool), stata)
    slo <- slopes(pool)
    expect_s3_class(slo, "slopes")
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se, ignore_attr = TRUE)

    # Swamy-Arora vs. Stata
    stata <- readRDS(test_path("stata/stata.rds"))$plm_sa
    mfx <- merge(avg_slopes(swamy), stata)
    slo <- slopes(swamy)
    expect_s3_class(slo, "slopes")
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = tol_se, ignore_attr = TRUE)

    # margins
    mfx <- avg_slopes(swamy)
    mar <- tidy(margins(swamy))
    mfx <- mfx[order(mfx$term), ]
    expect_equal(mfx$estimate, mar$estimate, tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mar$std.error, tolerance = tol_se, ignore_attr = TRUE)

    # no validity checks
    slo <- slopes(amemiya)
    expect_s3_class(slo, "slopes")
    # margins
    avg_slopes(amemiya, type = "link")
    avg_slopes(amemiya, type = "response")
    mfx <- avg_slopes(amemiya)
    mar <- tidy(margins(amemiya))
    mfx <- mfx[order(mfx$term), ]
    expect_equal(mfx$estimate, mar$estimate, tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mar$std.error, tolerance = tol_se, ignore_attr = TRUE)

    slo <- slopes(walhus)
    expect_s3_class(slo, "slopes")

    # margins
    mfx <- avg_slopes(walhus)
    mar <- tidy(margins(walhus))
    mfx <- mfx[order(mfx$term), ]
    expect_equal(mfx$estimate, mar$estimate, tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mar$std.error, tolerance = tol_se, ignore_attr = TRUE)

    # # commented out because the dev version of {plm} now has a fully-working predict method
    # # within error
    # # within model are not supported by `predict.plm`
    # stata <- readRDS(test_path("stata/stata.rds"))$plm_within
    # mod <- plm(inv ~ value * capital, data = dat, model = "within", variables = "twoways")
    # expect_error(slopes(mod), pattern = "Unable")

    ### predictions

    # predictions: pooling no validity
    pred1 <- predictions(pool)
    pred2 <- predictions(pool, newdata = head(dat))
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), nrow(dat), ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)
})
