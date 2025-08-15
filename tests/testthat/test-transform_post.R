test_that("transform_post works correctly", {
    # exponentiate
    acs12 <- get_dataset("acs12", "openintro")
    acs12$disability <- as.numeric(acs12$disability == "yes")
    mod <- glm(disability ~ gender + race + married + age, data = acs12, family = binomial)

    cmp1 <- comparisons(
        mod,
        variables = "gender",
        comparison = "lnratioavg"
    )
    cmp2 <- comparisons(
        mod,
        variables = "gender",
        comparison = "lnratioavg",
        transform = exp
    )
    expect_equal(exp(cmp1$estimate), cmp2$estimate, ignore_attr = TRUE)
    expect_equal(exp(cmp1$conf.low), cmp2$conf.low, ignore_attr = TRUE)
    expect_equal(exp(cmp1$conf.high), cmp2$conf.high, ignore_attr = TRUE)

    # # argument name deprecation
    # # aggregate refactor makes thsi possible again
    # expect_warning(tidy(cmp2, transform = exp))
    # expect_warning(summary(cmp2, transform = exp))

    # # aggregate refactor deprecates trasnsform_avg
    # tid1 <- tidy(cmp1)
    # tid2 <- tidy(cmp1, transform = exp)
    # expect_equal(exp(tid1$estimate), tid2$estimate, ignore_attr = TRUE)
    # expect_equal(exp(tid1$conf.low), tid2$conf.low, ignore_attr = TRUE)
    # expect_equal(exp(tid1$conf.high), tid2$conf.high, ignore_attr = TRUE)

    # issue #1115
    set.seed(0)
    n <- 500
    trt <- rep(0:1, each = 500)
    x <- rnorm(n)
    y <- 2 * x + 1 + 0.5 * trt + rnorm(n, 0, 0.3)
    d <- data.frame(x, trt, y)
    fit <- lm(y ~ trt + x, data = d)
    cmp <- avg_comparisons(
        fit,
        variables = "trt",
        comparison = "lnratioavg",
        transform = "exp"
    ) |>
        inferences(method = "boot", R = 100)
    expect_s3_class(cmp, "comparisons")
})
