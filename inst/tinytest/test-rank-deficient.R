source("helpers.R", local = TRUE)

# rank deficient
dat <<- mtcars
dat$gear <- as.factor(dat$gear)
dat$cyl <- as.factor(dat$cyl)
m <- glm(am ~ gear * cyl, data = dat, family = binomial())
expect_warning(comparisons(m), pattern = "rank deficient")
expect_inherits(suppressWarnings(comparisons(m)), "comparisons")
