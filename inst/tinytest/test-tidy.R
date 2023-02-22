source("helpers.R")
using("marginaleffects")



mod <- glm(vs ~ hp * mpg, data = mtcars, family = binomial)
mfx <- slopes(mod)
pred <- predictions(mod)


# tidy.predictions
requiet("MASS")
mod1 <- glm(vs ~ hp * mpg, data = mtcars, family = binomial)
# there used to be an interaction in this polr model, but it produced
# negative variances and NaN standard errors
mod2 <- polr(factor(gear) ~ hp + mpg, data = mtcars, Hess = TRUE)
pred1 <- predictions(mod1)
pred2 <- suppressMessages(predictions(mod2, type = "probs"))
ti1 <- tidy(pred1)
ti2 <- tidy(pred2)
expect_false(all(ti2$std.error == 0))
expect_inherits(ti1, "data.frame")
expect_inherits(ti2, "data.frame")
expect_equivalent(nrow(ti1), 1)
expect_equivalent(nrow(ti2), 3)

# Stata comparisons (manually collected)
mod <- lm(mpg ~ hp + wt, data = mtcars)
pred <- predictions(mod)
ti <- tidy(pred)
expect_equivalent(ti$estimate, 20.09062, tolerance = .0001)
# not supported yet
# expect_equivalent(ti$std.error, 45.84548, tolerance = .0001)
# expect_equivalent(ti$conf.low, 19.15298, tolerance = .0001)
# expect_equivalent(ti$conf.high, 21.02827, tolerance = .0001)



# tidy: minimal
ti <- tidy(mfx)
expect_equivalent(nrow(ti), 2)
expect_true(ncol(ti) > 6)
ti1 <- tidy(mfx, conf.level = .90)
ti2 <- tidy(mfx, conf.level = .99)
expect_true(all(ti1$conf.low > ti2$conf.low))
expect_true(all(ti1$conf.high < ti2$conf.high))



# glance: with modelsummary
gl <- glance(mfx)
expect_equivalent(nrow(gl), 1)
expect_true(ncol(gl) > 5)



# bug: emmeans contrast rename in binomial
x <- glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial)
x <- slopes(x)
x <- tidy(x)
expect_inherits(x, "data.frame") 
expect_equivalent(nrow(x), 3)



# tidy: with and without contrasts
tmp <- mtcars
tmp$am <- as.logical(tmp$am)

# numeric only
x <- tidy(slopes(lm(mpg ~ hp, tmp)))
expect_true(nrow(x) == 1)
expect_true(ncol(x) > 7)

# logical only
model <- lm(mpg ~ am, tmp)
x <- tidy(slopes(model))
expect_true(nrow(x) == 1)
expect_true(ncol(x) > 7)

# factor only
model <- lm(mpg ~ factor(gear), tmp)
x <- tidy(slopes(model))
expect_true(nrow(x) == 2)
expect_true(ncol(x) > 7)

# combinations
x <- tidy(slopes(lm(mpg ~ hp + am, tmp)))
expect_true(nrow(x) == 2)
expect_true(ncol(x) > 7)

x <- tidy(slopes(lm(mpg ~ hp + factor(gear), tmp)))
expect_true(nrow(x) == 3)
expect_true(ncol(x) > 7)

x <- tidy(slopes(lm(mpg ~ am + factor(gear), tmp)))
expect_true(nrow(x) == 3)
expect_true(ncol(x) > 7)

x <- tidy(slopes(lm(mpg ~ hp + am + factor(gear), tmp)))
expect_true(nrow(x) == 4)
expect_true(ncol(x) > 7)


# deprecated argument
mod <- lm(mpg ~ hp, mtcars)
cmp <- comparisons(mod)
expect_error(tidy(cmp, transform_avg = exp), pattern = "deprecated")



rm(list = ls())