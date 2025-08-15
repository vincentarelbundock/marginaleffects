test_that("blme package works", {
    skip_if_not_installed("blme")

    withr_library("blme")

    # exit_file("TODO: does not error consistently")

    dat <- get_dataset("sleepstudy", "lme4")
    penaltyFn <- function(sigma) dcauchy(sigma, 0, 10, log = TRUE)
    fm5 <- blmer(
        Reaction ~ Days + (0 + Days | Subject),
        data = dat,
        cov.prior = custom(penaltyFn, chol = TRUE, scale = "log")
    )
    fm6 <- blmer(
        Reaction ~ Days + (1 + Days | Subject),
        data = dat,
        cov.prior = NULL,
        fixef.prior = normal
    )
    mod <- bglmer(vs ~ mpg + (1 | gear), data = mtcars, family = binomial)

    slo1 <- slopes(fm5)
    expect_s3_class(slo1, "slopes")
    slo2 <- slopes(fm6)
    expect_s3_class(slo2, "slopes")
    slo3 <- slopes(mod)
    expect_s3_class(slo3, "slopes")

    # suppressWarnings about standard errors not account for uncertainty in random effects
    pre1 <- suppressWarnings(predictions(fm5))
    expect_s3_class(pre1, "predictions")
    pre2 <- suppressWarnings(predictions(fm6))
    expect_s3_class(pre2, "predictions")
    pre3 <- suppressWarnings(predictions(mod))
    expect_s3_class(pre3, "predictions")
})
