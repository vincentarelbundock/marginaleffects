test_that("brms-average package works", {
    skip_on_cran()
    skip_if_not_installed("brms")
    skip_if_not_installed("insight")

    withr_library("brms")
    withr_library("insight")

    void <- capture.output({
        m1 <- brm(mpg ~ hp, data = mtcars, silent = 2)
        m2 <- brm(mpg ~ hp + drat, data = mtcars, silent = 2)
        m3 <- brm(mpg ~ hp + drat + mo(cyl), data = mtcars, silent = 2)
    })

    # pp_average() vs. predictions()
    set.seed(1024)
    p1 <- suppressWarnings(pp_average(
        m1,
        m2 = m2,
        m3 = m3,
        robust = TRUE,
        newdata = head(mtcars)
    ))
    set.seed(1024)
    p2 <- suppressWarnings(predictions(
        m1,
        m2 = m2,
        m3 = m3,
        type = "average",
        newdata = head(mtcars)
    ))
    expect_equal(p2$estimate, p1[, "Estimate"], ignore_attr = TRUE)
    expect_equal(p2$conf.low, p1[, "Q2.5"], ignore_attr = TRUE)
    expect_equal(p2$conf.high, p1[, "Q97.5"], ignore_attr = TRUE)

    # manual vs. comparisons()
    set.seed(1024)
    dat_lo <- dat_hi <- head(mtcars)
    dat_hi$hp <- dat_hi$hp + 10
    dat_lo$hp <- dat_lo$hp - 10
    avg1 <- suppressWarnings(pp_average(
        m1,
        m2 = m2,
        m3 = m3,
        summary = FALSE,
        newdata = dat_lo
    ))
    avg2 <- suppressWarnings(pp_average(
        m1,
        m2 = m2,
        m3 = m3,
        summary = FALSE,
        newdata = dat_hi
    ))
    contr <- avg2 - avg1
    cmp1 <- t(apply(contr, 2, quantile, c(.5, .025, .975)))

    set.seed(1024)
    cmp2 <- suppressWarnings(comparisons(
        m1,
        m2 = m2,
        m3 = m3,
        type = "average",
        variables = list(hp = 20),
        newdata = head(mtcars)
    ))

    expect_equal(cmp2$estimate, cmp1[, "50%"], tolerance = .2, ignore_attr = TRUE)
    expect_equal(cmp2$conf.low, cmp1[, "2.5%"], tolerance = .2, ignore_attr = TRUE)
    expect_equal(cmp2$conf.high, cmp1[, "97.5%"], tolerance = .2, ignore_attr = TRUE)

    # method argument
    set.seed(1024)
    cmp1 <- suppressWarnings(comparisons(
        m1,
        m2 = m2,
        m3 = m3,
        type = "average",
        method = "posterior_epred",
        variables = list(hp = 20),
        newdata = head(mtcars)
    ))
    set.seed(1024)

    cmp2 <- suppressWarnings(comparisons(
        m1,
        m2 = m2,
        m3 = m3,
        type = "average",
        variables = list(hp = 20),
        newdata = head(mtcars)
    ))
    expect_true(all(cmp1$estimate != cmp2$estimate))
})
