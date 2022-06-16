source("helpers.R")
if(!isTRUE(requiet("car"))) exit_file("car not installed")

# When `FUN` and `hypothesis` are `NULL`, `deltamethod()` returns a data.frame of parameters
mod <- lm(mpg ~ hp + wt + factor(cyl), data = mtcars)
dmm <- deltamethod(mod)
expect_inherits(dmm, "data.frame")

# Test of equality between coefficients
dmm <- deltamethod(mod, "hp = wt")
dmc <- car::linearHypothesis(mod, hypothesis = "hp = wt")
expect_equivalent(dmm$estimate, attr(dmc, "value")[[1]])
expect_equivalent(dmm$std.error, sqrt(attr(dmc, "vcov")[[1]]))

# Non-linear function
dmm <- deltamethod(mod, "exp(hp + wt) = 0.1")
expect_inherits(dmm, "data.frame")

# Robust standard errors
dmm <- deltamethod(mod, "hp = wt", vcov = "HC3")
expect_inherits(dmm, "data.frame")

# r1, r2, ... shortcuts can be used to identify rows in the output of FUN
dmm <- deltamethod(mod, "r2 = r3")
expect_inherits(dmm, "data.frame")

# term names with special characters have to be enclosed in backticks
dmm <- deltamethod(mod, "`factor(cyl)6` = `factor(cyl)8`")
expect_inherits(dmm, "data.frame")

# The `FUN` argument can be used to compute standard errors for fitted values
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

f <- function(x) predict(x, type = "link", newdata = mtcars)
p <- deltamethod(mod, FUN = f)
expect_inherits(p, "data.frame")

f <- function(x) predict(x, type = "response", newdata = mtcars)
p <- deltamethod(mod, FUN = f)
expect_inherits(p, "data.frame")
