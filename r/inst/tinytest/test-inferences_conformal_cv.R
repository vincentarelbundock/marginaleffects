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
