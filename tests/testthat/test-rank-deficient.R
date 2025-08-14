test_that("rank deficient models produce warnings and work", {
    # rank deficient
    dat <- mtcars
    dat$gear <- as.factor(dat$gear)
    dat$cyl <- as.factor(dat$cyl)
    m <- glm(am ~ gear * cyl, data = dat, family = binomial())
    expect_warning(comparisons(m), regexp = "rank deficient")
    expect_s3_class(suppressWarnings(comparisons(m)), "comparisons")
})
