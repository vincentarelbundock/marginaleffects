source("helpers.R")
using("marginaleffects")
exit_if_not(requiet("car"))

# When `FUN` and `hypotheses` are `NULL`, `hypotheses()` returns a data.frame of parameters
dat <- mtcars
mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)
dmm <- hypotheses(mod)
expect_inherits(dmm, "data.frame")

# Test of equality between coefficients
dmm <- hypotheses(mod, "hp = wt")
dmc <- car::linearHypothesis(mod, hypothesis = "hp = wt")
expect_equivalent(dmm$estimate, attr(dmc, "value")[[1]])
expect_equivalent(dmm$std.error, sqrt(attr(dmc, "vcov")[[1]]))

# Non-linear function
dmm <- hypotheses(mod, "exp(hp + wt) = 0.1")
expect_inherits(dmm, "data.frame")

# Robust standard errors
dmm <- hypotheses(mod, "hp = wt", vcov = "HC3")
expect_inherits(dmm, "data.frame")

# b1, b2, ... shortcuts can be used to identify rows in the output of FUN
dmm <- hypotheses(mod, "b2 = b3")
expect_inherits(dmm, "data.frame")

# term names with special characters have to be enclosed in backticks
dmm <- hypotheses(mod, "`factor(cyl)6` = `factor(cyl)8`")
expect_inherits(dmm, "data.frame")

# The `FUN` argument can be used to compute standard errors for fitted values
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

f <- function(x) predict(x, type = "link", newdata = mtcars)
p <- hypotheses(mod, FUN = f)
expect_inherits(p, "data.frame")
expect_true(all(p$std.error > 0))

f <- function(x) predict(x, type = "response", newdata = mtcars)
p <- hypotheses(mod, FUN = f)
expect_inherits(p, "data.frame")
expect_true(all(p$std.error > 0))

# equality between predictions: 1 and 2 equal, 2 and 3 different
f <- function(x) predict(x, type = "link", newdata = mtcars)
dmm <- hypotheses(mod, FUN = f, hypothesis = "b1 = b2")
expect_equivalent(dmm$estimate, 0)
dmm <- hypotheses(mod, FUN = f, hypothesis = "b3 = b2")
expect_equivalent(dmm$estimate, 1.33154848763268)

# named matrix
mod <- lm(mpg ~ factor(cyl), data = mtcars)
hyp <- matrix(
    c(0, -1, 1, 1/3, 1/3, 1/3),
    ncol = 2,
    dimnames = list(NULL, c("H1", "H2")))
del <- hypotheses(mod, hypothesis = hyp)
expect_equivalent(del$term, c("H1", "H2"))


# two-step to check code where `hypotheses(model)` model is an object not a call
mod <- lm(mpg ~ factor(cyl), data = mtcars)
cmp <- avg_comparisons(mod)
hyp <- hypotheses(cmp, equivalence = c(-10, -5))
expect_inherits(hyp, "hypotheses")




# hypotheses() applied to {marginaleffects} package objects
# commented out because doesn't work in environments because of match.call()
# mod <- glm(vs ~ hp + am, data = mtcars, family = binomial)

# cmp <- comparisons(mod, by = "am")
# dm <- hypotheses(cmp, hypothesis = "b1 = b3")
# expect_true("b1=b3" %in% dm$term)
# expect_equivalent(nrow(dm), 1)

# mfx <- slopes(mod)
# dm <- hypotheses(mfx, hypothesis = "b1 = b3")
# expect_true("b1=b3" %in% dm$term)
# expect_equivalent(nrow(dm), 1)

# pre <- predictions(mod, newdata = datagrid())
# dm <- hypotheses(pre, hypothesis = "b1 = 0.05")
# expect_true("b1=0.05" %in% dm$term)
# expect_equivalent(nrow(dm), 1)

# mod <- glm(vs ~ hp + factor(am), data = mtcars, family = binomial)
# mm <- marginal_means(mod, "am")
# dm <- hypotheses(mm, hypothesis = "b1 = b2")
# expect_true("b1=b2" %in% dm$term)
# expect_equivalent(nrow(dm), 1)