N <- 1e4
dat <- data.frame(matrix(rnorm(N * 10), ncol = 10))
dat$c1 <- factor(sample(letters, N, replace = TRUE))
dat$c2 <- factor(sample(letters, N, replace = TRUE))
mod <- lm(X1 ~ ., dat)

library(marginaleffects)
system.time(comparisons(mod))

# NEW:
  #  user  system elapsed
  # 9.239   0.001   9.123

# OLD: 34.6s



remotes::install_github("vincentarelbundock/marginaleffects")
library(marginaleffects)
system.time({
mfx <- comparisons(mod)
})

 #   user  system elapsed
 # 36.737   0.116  36.801

remotes::install_github("vincentarelbundock/marginaleffects@allin1")
library(marginaleffects)


system.time({
k = comparisons(mod)
})

library(profvis)
profvis({
    comparisons(mod)
})

###### OLD: 135s
###### NEW: 159s

library(marginaleffects)

# simulate data and fit a large model
N <- 1e5
dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
mod <- lm(X1 ~ ., dat)

system.time({
    vars <- colnames(dat)[2:26]
    tmp <- lapply(vars, function(x) comparisons(mod, variables = x))
    tmp <- rbindlist(tmp)
})

system.time({
    comparisons(mod)
})



library(marginaleffects)
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


# OLD


# NEW

mat = matrix(NA_real_, nrow = 1e6, ncol = 2)
mat_dt = data.table()
x <- rnorm(1e6)
bench::mark(
mat[, 1]  <- x,
mat_dt[, V1 := x],
iterations = 1,
check = FALSE)
