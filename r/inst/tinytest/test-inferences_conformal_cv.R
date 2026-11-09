source("helpers.R")
using("marginaleffects")

###### conformal_cv+: CV+ of Barber, Candès, Ramdas & Tibshirani (2021). Issue #1761.
# default `R` uses 10 folds instead of erroring on the 25-fold cap
mod <- lm(mpg ~ hp + wt, mtcars)
p <- predictions(mod) |>
    inferences(method = "conformal_cv+", data_train = mtcars)
expect_inherits(p, "predictions")
expect_true(all(p$pred.low < p$estimate & p$estimate < p$pred.high))

# matches a manual implementation of eq. 11: fold models predict the test points
set.seed(1024)
idx_train <- sample(nrow(mtcars), 24)
train <- mtcars[idx_train, ]
test <- mtcars[-idx_train, ]
mod <- lm(mpg ~ hp + wt, train)
set.seed(48103)
p <- predictions(mod, newdata = test, conf_level = 0.8) |>
    inferences(method = "conformal_cv+", R = 4, data_train = train)
set.seed(48103)
n <- nrow(train)
idx <- sample.int(n, n)
folds <- split(idx, ceiling(seq_along(idx) / (n / 4)))
lo <- hi <- NULL
for (i in folds) {
    m <- lm(mpg ~ hp + wt, train[-i, ])
    r <- abs(train$mpg[i] - predict(m, train[i, ]))
    mu <- predict(m, test)
    lo <- rbind(lo, outer(r, mu, function(a, b) b - a))
    hi <- rbind(hi, outer(r, mu, function(a, b) b + a))
}
alpha <- 0.2
k_lo <- floor(alpha * (n + 1))
k_hi <- ceiling((1 - alpha) * (n + 1))
expect_equivalent(p$pred.low, apply(lo, 2, function(v) sort(v)[k_lo]))
expect_equivalent(p$pred.high, apply(hi, 2, function(v) sort(v)[k_hi]))
# intervals are not forced to be symmetric around the full-data prediction
expect_true(any(abs((p$estimate - p$pred.low) - (p$pred.high - p$estimate)) > 1e-6))

# approximate coverage
set.seed(48103)
dat <- get_dataset("military")
dat <- dat[sample(nrow(dat), 2000), ]
idx <- sample(c("train", "test"), nrow(dat), replace = TRUE)
dat <- split(dat, idx)
mod <- lm(rank ~ grade + branch + gender + race, data = dat$train)
p <- predictions(mod, newdata = dat$test, conf_level = 0.9) |>
    inferences(method = "conformal_cv+", R = 5, data_train = dat$train)
coverage <- mean(p$rank >= p$pred.low & p$rank <= p$pred.high)
expect_equivalent(coverage, 0.9, tolerance = 0.03)

# classification: prediction sets from fold models with softmax scores
requiet("nnet")
set.seed(48103)
idx <- sample(c("train", "test"), nrow(iris), replace = TRUE)
dat <- split(iris, idx)
mod <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width, data = dat$train, trace = FALSE)
p <- predictions(mod, newdata = dat$test, conf_level = 0.9) |>
    inferences(method = "conformal_cv+", R = 5, data_train = dat$train, conformal_score = "softmax")
expect_true("pred.set" %in% colnames(p))
expect_equal(nrow(p), nrow(dat$test))
coverage <- mean(mapply(function(y, s) y %in% s, as.character(p$Species), p$pred.set))
expect_true(coverage >= 0.8)

# classification: a fold that loses an outcome class. multinom becomes binary
# and `predictions()` returns only the non-reference class; the reference class
# keeps the remaining probability, and the absent class gets 0.
d2 <- iris[iris$Species != "virginica", ]
m2 <- nnet::multinom(Species ~ Sepal.Length, data = d2, trace = FALSE) |> suppressWarnings()
p2 <- predictions(m2, newdata = iris[c(1, 51, 101), ], vcov = FALSE)
cc <- marginaleffects:::conformal_complete_classes(p2, levels(iris$Species), d2$Species)
cc <- cc[order(cc$rowid, cc$group), ]
expect_equal(nrow(cc), 9)
expect_equivalent(cc$estimate[cc$group == "setosa"], 1 - p2$estimate)
expect_equivalent(cc$estimate[cc$group == "virginica"], c(0, 0, 0))
expect_equivalent(as.numeric(tapply(cc$estimate, cc$rowid, sum)), c(1, 1, 1))

# end to end: a single virginica observation, so one fold trains without it
set.seed(48103)
train <- rbind(iris[iris$Species != "virginica", ], iris[101, ])
test <- iris[c(1:5, 51:55), ]
mod <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width, data = train, trace = FALSE)
p <- suppressWarnings(
    predictions(mod, newdata = test, conf_level = 0.9) |>
        inferences(method = "conformal_cv+", R = 5, data_train = train, conformal_score = "softmax"))
expect_equal(nrow(p), nrow(test))
coverage <- mean(mapply(function(y, s) y %in% s, as.character(p$Species), p$pred.set))
expect_true(coverage >= 0.8)

# a test row with a missing predictor keeps NA bounds; other rows are computed
mod <- lm(mpg ~ hp + wt, mtcars)
test <- mtcars[1:4, ]
test$hp[2] <- NA
set.seed(48103)
p <- predictions(mod, newdata = test) |>
    inferences(method = "conformal_cv+", R = 5, data_train = mtcars)
expect_equal(nrow(p), 4)
expect_true(is.na(p$pred.low[2]) && is.na(p$pred.high[2]))
expect_false(anyNA(p$pred.low[-2]))
expect_false(anyNA(p$pred.high[-2]))

# classification: the unestimable row gets an NA set and keeps its place
mod <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width, data = iris, trace = FALSE)
test <- iris[c(1, 51, 101), ]
test$Sepal.Width[2] <- NA
set.seed(48103)
p <- predictions(mod, newdata = test) |>
    inferences(method = "conformal_cv+", R = 5, data_train = iris, conformal_score = "softmax")
expect_equal(p$rowid, 1:3)
expect_true(identical(p$pred.set[[2]], NA_character_))
expect_true(length(p$pred.set[[1]]) > 0 && !anyNA(p$pred.set[[1]]))

# an entirely unestimable test batch returns NA bounds instead of erroring
mod <- lm(mpg ~ hp + wt, mtcars)
test <- mtcars[1, ]
test$hp <- NA_real_
set.seed(48103)
p <- predictions(mod, newdata = test) |>
    inferences(method = "conformal_cv+", R = 5, data_train = mtcars)
expect_equal(nrow(p), 1)
expect_true(is.na(p$pred.low) && is.na(p$pred.high))

# classification: missing training outcomes are not treated as a class, so a
# binary fold's reference class keeps all the remaining probability
d2 <- iris[iris$Species != "virginica", ]
m2 <- nnet::multinom(Species ~ Sepal.Length, data = d2, trace = FALSE) |> suppressWarnings()
p2 <- predictions(m2, newdata = iris[c(1, 51), ], vcov = FALSE)
cc <- marginaleffects:::conformal_complete_classes(
    p2, c(levels(iris$Species), NA), c(as.character(d2$Species), NA))
expect_false(anyNA(cc$group))
expect_equivalent(cc$estimate[cc$group == "setosa"], 1 - p2$estimate)

# end to end: missing outcomes and a fold that loses a class
set.seed(48103)
train <- rbind(iris[iris$Species != "virginica", ], iris[101, ])
train$Species[c(3, 60)] <- NA
mod <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width, data = train, trace = FALSE)
test <- iris[c(1:5, 51:55), ]
p <- suppressWarnings(
    predictions(mod, newdata = test) |>
        inferences(method = "conformal_cv+", R = 5, data_train = train, conformal_score = "softmax"))
expect_equal(nrow(p), nrow(test))
expect_false(anyNA(unlist(p$pred.set)))
expect_true(mean(mapply(function(y, s) y %in% s, as.character(p$Species), p$pred.set)) >= 0.8)
