source("helpers.R")
requiet("marginaleffects")

op <- getOption("marginaleffects_safe", default = TRUE)
options(marginaleffects_safe = TRUE)

# factor in formula
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
expect_warning(slopes(mod))


# Issue #1447: invlink(link) hypothesis scale
dat <- transform(mtcars, cyl = factor(cyl))
mod <- glm(am ~ hp + cyl, data = dat, family = binomial)
expect_warning(predictions(mod, hypothesis = 3), pattern = "invlink")

options(marginaleffects_safe = op)
