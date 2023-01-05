# simple benchmarks for use in development
# clone the master branch to ~/Downloads/master
# do NOT call `library(marginaleffects)`

# many factor levels
N <- 1e3
dat <- data.frame(matrix(rnorm(N * 10), ncol = 10))
dat$c1 <- factor(sample(letters, N, replace = TRUE))
dat$c2 <- factor(sample(letters, N, replace = TRUE))
mod <- lm(X1 ~ ., dat)

pkgload::load_all("~/Downloads/master")
system.time(slopes(mod))

pkgload::load_all("~/repos/marginaleffects")
system.time(slopes(mod))


library(marginaleffects)
library(profvis)
profvis(slopes(mod))

# many variables
N <- 1e4
dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
mod <- lm(X1 ~ ., dat)

pkgload::load_all("~/Downloads/master")
system.time(slopes(mod))

pkgload::load_all("~/repos/marginaleffects")
system.time(slopes(mod))


# arguments
pkgload::load_all("~/Downloads/master")
results <- bench::mark(
    # marginal effects at the mean; no standard error
    comparisons(mod, vcov = FALSE, newdata = datagrid()),
    # marginal effects at the mean
    comparisons(mod, newdata = datagrid()),
    # 1 variable; no standard error
    comparisons(mod, vcov = FALSE, variables = "X3"),
    # 1 variable
    comparisons(mod, variables = "X3"),
    # 26 variables; no standard error
    comparisons(mod, vcov = FALSE),
    # 26 variables
    # comparisons(mod),
    iterations = 1, check = FALSE)
results[, c(1, 3, 5)]

pkgload::load_all("~/repos/marginaleffects")
results <- bench::mark(
    # marginal effects at the mean; no standard error
    comparisons(mod, vcov = FALSE, newdata = datagrid()),
    # marginal effects at the mean
    comparisons(mod, newdata = datagrid()),
    # 1 variable; no standard error
    comparisons(mod, vcov = FALSE, variables = "X3"),
    # 1 variable
    comparisons(mod, variables = "X3"),
    # 26 variables; no standard error
    comparisons(mod, vcov = FALSE),
    # 26 variables
    # comparisons(mod),
    iterations = 1, check = FALSE)
results[, c(1, 3, 5)]
