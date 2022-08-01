exit_file("tinyviztest")
source("helpers.R")
using("tinyviztest")

# basic plot.marginaleffects()
mod <- glm(am ~ hp + wt, data = mtcars)
mfx <- marginaleffects(mod)
p <- plot(mfx)
expect_vdiff(p, "plot marginaleffects")


# plot(mfx): no CI
mod <- glm(am ~ hp + wt, data = mtcars)
mfx <- marginaleffects(mod, vcov = FALSE)
p <- plot(mfx)
expect_vdiff(p, "plot marginaleffects no CI")


# bugfix: contrasts overlap
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ hp + cyl, data = dat)
mfx <- marginaleffects(mod)
p <- plot(mfx)
expect_vdiff(p, "plot contrast overlap bug fix")
