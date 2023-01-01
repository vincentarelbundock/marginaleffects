source("helpers.R")
using("marginaleffects")
exit_if_not(!ON_OSX)

library("tinyviztest")
using("tinyviztest")

# basic plot.marginaleffects()
mod <- glm(am ~ hp + wt, data = mtcars)
mfx <- marginaleffects(mod)
p <- plot(mfx)
expect_vdiff(p, "plot_marginaleffects")


# plot(mfx): no CI
mod <- glm(am ~ hp + wt, data = mtcars)
mfx <- marginaleffects(mod, vcov = FALSE)
p <- plot(mfx)
expect_vdiff(p, "plot_marginaleffects_no_CI")


# bugfix: contrasts overlap
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat <<- dat
mod <- lm(mpg ~ hp + cyl, data = dat)
mfx <- marginaleffects(mod)
p <- plot(mfx)
expect_vdiff(p, "plot_contrast_overlap_bug_fix")
