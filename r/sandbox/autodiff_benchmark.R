########## benchmarking ##########
library(microbenchmark)
pkgload::load_all("~/repos/marginaleffects/r")

# Download data and fit a large model
dat <- get_dataset("airbnb")
dat$groupid <- sample(letters, nrow(dat), replace = TRUE)
mod <- glm(TV ~ ., data = dat, family = binomial)

# Put this at the top of a script; first-call timings below exclude this startup cost.
autodiff(TRUE)

fd <- \() {autodiff(FALSE);p <- predictions(mod)}
ad <- \() {autodiff(TRUE);p <- predictions(mod)}
microbenchmark(fd(), ad(), times = 5)

fd <- \() {autodiff(FALSE);p <- avg_predictions(mod)}
ad <- \() {autodiff(TRUE);p <- avg_predictions(mod)}
microbenchmark(fd(), ad(), times = 5)

fd <- \() {autodiff(FALSE);p <- avg_predictions(mod, by = "groupid")}
ad <- \() {autodiff(TRUE);p <- avg_predictions(mod, by = "groupid")}
microbenchmark(fd(), ad(), times = 5)

fd <- \() {autodiff(FALSE);p <- comparisons(mod, variables = "Dryer")}
ad <- \() {autodiff(TRUE);p <- comparisons(mod, variables = "Dryer")}
microbenchmark(fd(), ad(), times = 5)

fd <- \() {autodiff(FALSE);p <- avg_comparisons(mod, variables = "Dryer")}
ad <- \() {autodiff(TRUE);p <- avg_comparisons(mod, variables = "Dryer")}
microbenchmark(fd(), ad(), times = 5)

fd <- \() {autodiff(FALSE);p <- avg_comparisons(mod, variables = "Dryer", by = "groupid")}
ad <- \() {autodiff(TRUE);p <- avg_comparisons(mod, variables = "Dryer", by = "groupid")}
microbenchmark(fd(), ad(), times = 5)

