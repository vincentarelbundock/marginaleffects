skip_on_cran()

test_that("padding with interactions works correctly", {
    withr_library("marginaleffects")
    dat <- get_dataset("movies", "ggplot2movies")
    dat$style <- ifelse(dat$Action == 1, "Action", "Other")
    dat$style <- ifelse(dat$Comedy == 1, "Comedy", dat$style)
    dat$style <- ifelse(dat$Drama == 1, "Drama", dat$style)
    dat$style <- factor(dat$style)
    dat$certified_fresh <- dat$rating >= 8
    dat <- dat[dat$length < 240, ]
    mod <- glm(certified_fresh ~ length * style, data = dat, family = binomial)
    res <- predictions(mod, type = "response")
    expect_s3_class(res, "predictions")
    expect_equal(nrow(res), nrow(dat), ignore_attr = TRUE)
    expect_true("std.error" %in% colnames(res))
})
