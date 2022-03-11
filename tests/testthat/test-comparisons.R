
test_that("print.comparisons.summary", {
    dat <- mtcars
    dat$gear <- factor(dat$gear)
    mod <- lm(mpg ~ gear, data = dat)
    expect_snapshot(summary(comparisons(mod)))
})

