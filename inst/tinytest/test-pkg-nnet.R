source("helpers.R")
using("marginaleffects")

requiet("nnet")

# multinom group estimates
TitanicSurvival <- "https://vincentarelbundock.github.io/Rdatasets/csv/carData/TitanicSurvival.csv"
TitanicSurvival <- read.csv(TitanicSurvival)
TitanicSurvival$age3 <- cut(
    TitanicSurvival$age,
    include.lowest = TRUE,
    right = FALSE,
    dig.lab = 4,
    breaks = c(0, 25, 50, 80))
m1 <- nnet::multinom(passengerClass ~ sex * age3, data = TitanicSurvival, trace = FALSE)
mfx <- slopes(
    m1,
    type = "probs",
    variables = "sex",
    by = "age3",
    newdata = datagridcf(age3 = c("[0,25)","[25,50)","[50,80]")))
expect_equivalent(nrow(mfx), 9)


# multinom basic
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
void <- capture.output(
    mod <- nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true)
)
expect_slopes(mod, type = "probs")


# marginaleffects summary
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
void <- capture.output(
    mod <- nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true)
)
mfx <- slopes(mod, type = "probs")
s <- tidy(mfx)
expect_false(anyNA(s$estimate))
expect_false(anyNA(s$std.error))


# multinom vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$nnet_multinom_01
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
dat$y <- as.factor(dat$y)
void <- capture.output(
    mod <- nnet::multinom(y ~ x1 + x2, data = dat, quiet = true)
)
mfx <- slopes(mod, type = "probs")
mfx <- merge(tidy(mfx), stata, all = TRUE)
mfx <- na.omit(mfx)
expect_true(nrow(mfx) == 6) # na.omit doesn't trash everything
# standard errors match now!!
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .001)


# set_coef
tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
void <- capture.output(
    old <- nnet::multinom(cyl ~ hp + am + mpg, data = tmp, quiet = true)
)
b <- rep(0, length(coef(old)))
new <- set_coef(old, b)
expect_true(all(coef(new) == 0))
b <- rep(1, length(coef(new)))
new <- set_coef(old, b)
expect_true(all(coef(new) == 1))


# bugfix: nnet single row predictions
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
void <- capture.output(
    mod <- nnet::multinom(factor(y) ~ x1 + x2, data = dat, quiet = true)
)
mfx <- slopes(mod, variables = "x1", newdata = datagrid(), type = "probs")
expect_inherits(mfx, "data.frame")
expect_equivalent(nrow(mfx), 4)
mfx <- slopes(mod, newdata = datagrid(), type = "probs")
expect_inherits(mfx, "data.frame")
expect_equivalent(nrow(mfx), 8)


# predictions with multinomial outcome
set.seed(1839)
n <- 1200
x <- factor(sample(letters[1:3], n, TRUE))
y <- vector(length = n)
y[x == "a"] <- sample(letters[4:6], sum(x == "a"), TRUE)
y[x == "b"] <- sample(letters[4:6], sum(x == "b"), TRUE, c(1 / 4, 2 / 4, 1 / 4))
y[x == "c"] <- sample(letters[4:6], sum(x == "c"), TRUE, c(1 / 5, 3 / 5, 2 / 5))
dat <- data.frame(x = x, y = factor(y))
tmp <- as.data.frame(replicate(20, factor(sample(letters[7:9], n, TRUE))))
dat <- cbind(dat, tmp)
void <- capture.output({
    m1 <- nnet::multinom(y ~ x, dat)
    m2 <- nnet::multinom(y ~ ., dat)
})

# class outcome not supported
expect_error(predictions(m1, type = "class"), pattern = "type")
expect_error(marginal_means(m1, type = "class"), pattern = "type")
expect_error(slopes(m1, type = "class"), pattern = "type")

# small predictions
pred1 <- predictions(m1, type = "probs")
pred2 <- predictions(m1, type = "probs", newdata = "marginalmeans")
expect_predictions(pred1, n_row = nrow(dat) * 3)
expect_predictions(pred2, n_row = 9)

# large predictions
idx <- 3:5
n_row <- sapply(dat[, idx], function(x) length(unique(x)))
n_row <- prod(n_row) * length(unique(dat$y))
expect_error(predictions(m2, type = "probs", newdata = "mean"), pattern = "Cross product")

# massive prediction raises error
expect_error(predictions(m2, type = "probs"), pattern = "")


# bugs stay dead #218
set.seed(42)
dat <- data.frame(
    y = factor(sample(c(rep(4, 29), rep(3, 15), rep(2, 4), rep(1, 2)))),
    x = factor(sample(c(rep(1, 17), rep(2, 12), rep(2, 12), rep(1, 9)))),
    z1 = sample(1:2, 50, replace=TRUE), z2=runif(50, 16, 18))
void <- capture.output(
    model <- nnet::multinom(y ~ x + z1 + z2, data = dat, verbose = FALSE, hessian = TRUE)
)
mfx <- slopes(model, type = "probs")
expect_inherits(mfx, "marginaleffects")


# bug: single row newdata produces vector
mod <- nnet::multinom(factor(gear) ~ mpg, data = mtcars, trace = FALSE)
p <- predictions(mod, newdata = head(mtcars, 1), type = "latent")
expect_equivalent(nrow(p), 3)


# Issue #476: binary dependent variable
x <- 1:1000
n <- length(x)
y1 <- rbinom(n, 10, prob = plogis(-10 + 0.02 * x))
y2 <- 10 - y1
dat <- data.frame(x, y1, y2)
dat_long <- tidyr::pivot_longer(dat, !x, names_to = "y", values_to = "count")
dat_long <- transform(dat_long, y = factor(y, levels = c("y2", "y1")))
fit_multinom <- nnet::multinom(y ~ x, weights = count, data = dat_long, trace = FALSE)
p <- predictions(fit_multinom,
    newdata = datagrid(x = unique),
    type = "latent")
expect_inherits(p, "predictions")


# Issue #482: sum of predicted probabilities
mod <- nnet::multinom(factor(cyl) ~ mpg + am, data = mtcars, trace = FALSE)
by <- data.frame(
    by = c("4,6", "4,6", "8"),
    group = as.character(c(4, 6, 8)))
p1 <- predictions(mod, newdata = "mean")
p2 <- predictions(mod, newdata = "mean", byfun = sum, by = by)
p3 <- predictions(mod, newdata = "mean", byfun = mean, by = by)
expect_equivalent(nrow(p1), 3)
expect_equivalent(nrow(p2), 2)
expect_equivalent(nrow(p3), 2)
expect_equivalent(sum(p1$estimate[1:2]), p2$estimate[1])
expect_equivalent(mean(p1$estimate[1:2]), p3$estimate[1])



rm(list = ls())