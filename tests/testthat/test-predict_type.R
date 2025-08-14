test_that("sanity gives informative error for all the functions", {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    mod <- lm(mpg ~ hp + cyl, data = dat)
    expect_error(comparisons(mod, type = "junk"), regexp = "Must be element")
    expect_error(predictions(mod, type = "junk"), regexp = "Must be element")
    expect_error(slopes(mod, type = "junk"), regexp = "Must be element")
})


test_that("multivariate models work with type = prob", {
    skip_if_not_installed("pscl")
    withr_library("pscl")
    dat2 <- get_dataset("bioChemists", "pscl")
    model <- hurdle(art ~ phd + fem | ment, data = dat2, dist = "negbin")
    mfx <- slopes(model, type = "prob")
    expect_true(all(as.character(0:19) %in% mfx$group))
})


test_that("Issue #1123: invlink(link) not default for avg_predictions()", {
    mod <- glm(am ~ hp, data = mtcars, family = binomial)
    p1 <- avg_predictions(mod)
    p2 <- avg_predictions(mod, type = "response")
    p3 <- avg_predictions(mod, type = "invlink(link)")
    expect_equal(p1$estimate, p2$estimate, ignore_attr = TRUE)
    expect_false(isTRUE(all.equal(p1$estimate, p3$estimate)))
})
