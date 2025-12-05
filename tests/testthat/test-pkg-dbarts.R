skip("TODO")
testthat::skip_if_not_installed("dbarts")
testthat::skip_if_not_installed("modeldata")

requiet("dbarts")
requiet("modeldata")

# Basic expectation tests
mod_simple <- dbarts::bart2(
    mpg ~ .,
    data = mtcars,
    keepTrees = TRUE,
    verbose = FALSE
)
expect_slopes2(mod_simple, se = FALSE)
expect_predictions2(mod_simple, se = FALSE)
expect_comparisons2(mod_simple, se = FALSE)
expect_error(hypotheses(mod_simple), "does not support hypothesis tests")

dat_penguins <- get_dataset("penguins", "palmerpenguins")
dat_penguins <- na.omit(dat_penguins)

# matrix interface not supported
y <- as.vector(dat_penguins$bill_length_mm)
X <- model.matrix(~., dat_penguins[, 3:ncol(dat_penguins)])
mod <- dbarts::bart(
    X,
    y,
    verbose = FALSE
) |>
    suppressWarnings()
expect_error(comparisons(mod, newdata = dat_penguins), "bart2") |> suppressWarnings()


# formula interface supported
mod <- dbarts::bart2(
    bill_length_mm ~ .,
    data = dat_penguins,
    keepTrees = TRUE,
    verbose = FALSE
)

p <- predictions(mod, by = "species", newdata = dat_penguins)
expect_s3_class(p, "predictions")
c <- avg_comparisons(mod, newdata = dat_penguins)
expect_s3_class(c, "comparisons")


# Issue 940: Indexing hell
testthat::skip_if_not_installed("MatchIt")
options(marginaleffects_posterior_center = mean)
dat <- get_dataset("lalonde", "MatchIt")

fit <- dbarts::bart2(
    re78 ~ treat + age + educ + race + married + nodegree + re74 + re75,
    data = dat,
    keepTrees = TRUE,
    verbose = FALSE
)

p0 <- predict(fit, newdata = transform(subset(dat, treat == 1), treat = 0))
p1 <- predict(fit, newdata = transform(subset(dat, treat == 1), treat = 1))
p <- avg_comparisons(fit, variables = "treat", newdata = subset(dat, treat == 1))
expect_equal(p$estimate, mean(p1 - p0))

p0 <- predict(fit, newdata = transform(subset(dat, treat == 0), treat = 0))
p1 <- predict(fit, newdata = transform(subset(dat, treat == 0), treat = 1))
p <- avg_comparisons(fit, variables = "treat", newdata = subset(dat, treat == 0))
expect_equal(p$estimate, mean(p1 - p0))

p0 <- avg_comparisons(fit, variables = "treat", newdata = subset(dat, treat == 0))
p1 <- avg_comparisons(fit, variables = "treat", newdata = subset(dat, treat == 1))
p <- avg_comparisons(fit, variables = "treat", by = "treat", newdata = dat)
expect_equal(sort(c(p0$estimate, p1$estimate)), sort(p$estimate))

p0 <- avg_predictions(fit, newdata = subset(dat, treat == 0))
p1 <- avg_predictions(fit, newdata = subset(dat, treat == 1))
p <- avg_predictions(fit, by = "treat", newdata = dat)
expect_equal(sort(c(p0$estimate, p1$estimate)), sort(p$estimate))

options(marginaleffects_posterior_center = NULL)
