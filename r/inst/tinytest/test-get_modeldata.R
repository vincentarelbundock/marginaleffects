source("helpers.R")
using("marginaleffects")

# set_modeldata() attaches data and get_modeldata() retrieves it
mod <- lm(mpg ~ hp, data = mtcars)
mod <- set_modeldata(mod, mtcars)
md <- get_modeldata(mod)
expect_inherits(md, "data.frame")
expect_equivalent(nrow(md), nrow(mtcars))
expect_equivalent(ncol(md), ncol(mtcars))

# set_modeldata() data is used by predictions()
mod <- lm(mpg ~ hp + wt, data = mtcars)
mod <- set_modeldata(mod, mtcars)
p <- predictions(mod)
expect_inherits(p, "predictions")
expect_equivalent(nrow(p), nrow(mtcars))

# get_modeldata() warns when insight::get_data() raises warnings
# Fit without data= arg using global env variables, then remove them
# so insight cannot recover from the environment
# Reset the warn_once flag so the warning fires in this test session
op <- options(
    marginaleffects_safe = TRUE,
    marginaleffects_modeldata_from_environment = TRUE
)
y_var <- rnorm(20)
x_var <- rnorm(20)
mod <- lm(y_var ~ x_var)
rm(y_var, x_var)
expect_warning(
    get_modeldata(mod),
    pattern = "set_modeldata"
)
options(op)

# set_modeldata() makes a deep copy: modifying original does not affect stored data
dat <- data.frame(y = rnorm(20), x = rnorm(20))
mod <- lm(y ~ x, data = dat)
mod <- set_modeldata(mod, dat)
dat$x <- 999
md <- get_modeldata(mod)
expect_true(all(md$x != 999))

# same with data.table
requiet("data.table")
dt <- data.table(y = rnorm(20), x = rnorm(20))
mod <- lm(y ~ x, data = dt)
mod <- set_modeldata(mod, dt)
dt[, x := 999]
md <- get_modeldata(mod)
expect_true(all(md$x != 999))

# No warning when set_modeldata() was used
options(marginaleffects_modeldata_from_environment = TRUE)
dat <- data.frame(y = rnorm(20), x = rnorm(20))
mod <- lm(y ~ x, data = dat)
mod <- set_modeldata(mod, dat)
# This should NOT warn because set_modeldata was used
md <- get_modeldata(mod)
expect_inherits(md, "data.frame")
expect_equivalent(nrow(md), 20L)
