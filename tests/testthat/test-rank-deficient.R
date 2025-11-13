
# rank deficient
dat <- mtcars
dat$gear <- as.factor(dat$gear)
dat$cyl <- as.factor(dat$cyl)
dat <- dat
m <- glm(am ~ gear * cyl, data = dat, family = binomial())
expect_warning(comparisons(m), regexp = "rank deficient")
expect_s3_class(suppressWarnings(comparisons(m)), "comparisons")
