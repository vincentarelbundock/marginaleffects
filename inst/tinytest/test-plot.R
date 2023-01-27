source("helpers.R")
using("marginaleffects")
exit_if_not(!ON_OSX)
exit_if_not(require("tinyviztest"))

library("tinyviztest")
using("tinyviztest")

# from marginaleffects objects
mod <- glm(am ~ hp + wt, data = mtcars)

p <- plot_comparisons(comparisons(mod))
expect_inherits(p, "gg")
p <- plot(comparisons(mod))
expect_inherits(p, "gg")

p <- plot_slopes(slopes(mod))
expect_inherits(p, "gg")
p <- plot(slopes(mod))
expect_inherits(p, "gg")


# from models
p <- plot_slopes(mod)
expect_snapshot_plot(p, "plot_marginaleffects")


# plot(mfx): no CI
mod <- glm(am ~ hp + wt, data = mtcars)
p <- plot_slopes(mod, vcov = FALSE)
expect_snapshot_plot(p, "plot_marginaleffects_no_CI")


# bugfix: contrasts overlap
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat <- dat
mod <- lm(mpg ~ hp + cyl, data = dat)
p <- plot_slopes(mod)
expect_snapshot_plot(p, "plot_contrast_overlap_bug_fix")
