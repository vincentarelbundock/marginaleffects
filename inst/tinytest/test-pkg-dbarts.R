source("helpers.R")
requiet("dbarts")
requiet("modeldata")
requiet("marginaleffects")

dat <- get_dataset("penguins", "palmerpenguins")
dat <- na.omit(dat)

# matrix interface not supported
y <- as.vector(dat$bill_length_mm)
X <- model.matrix(~., dat[, 3:ncol(dat)])
mod <- dbarts::bart(
    X,
    y,
    verbose = FALSE
) |>
    suppressWarnings()
expect_error(comparisons(mod, newdata = dat), "bart2") |> suppressWarnings()


# formula interface supported
mod <- dbarts::bart2(
    bill_length_mm ~ .,
    data = dat,
    keepTrees = TRUE,
    verbose = FALSE
)

p <- predictions(mod, by = "species", newdata = dat)
expect_inherits(p, "predictions")
p <- avg_comparisons(mod, variables = colnames(dat), newdata = dat)
expect_inherits(p, "comparisons")


# Issue 940: Indexing hell
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
