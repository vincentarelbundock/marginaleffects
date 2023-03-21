source("helpers.R")
using("marginaleffects")
if (!requiet("tinysnapshot")) exit_file("tinysnapshot")
# if (ON_WINDOWS || ON_OSX) exit_file("linux only")
using("tinysnapshot")

# character predictors
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(
    large_penguin ~ bill_length_mm * flipper_length_mm + species,
    family = binomial, data = dat)
p <- plot_slopes(mod, variables = "bill_length_mm", condition = "flipper_length_mm", draw = FALSE)
expect_inherits(p, "data.frame")
expect_equivalent(nrow(p), 50)
expect_false(anyNA(p$estimate))


# custom values
p <- plot_slopes(mod, variables = "bill_length_mm", condition = list("flipper_length_mm" = 10), draw = FALSE)
expect_true(p$flipper_length_mm == 10)


# vcov
mod <- lm(mpg ~ hp * wt, data = mtcars)
mfx1 <- plot_slopes(mod, variables = "hp", condition = "wt", draw = FALSE)
mfx2 <- plot_slopes(mod, variables = "hp", condition = "wt", vcov = "HC3", draw = FALSE)
mfx3 <- plot_slopes(mod, variables = "hp", condition = "wt", vcov = ~cyl, draw = FALSE)
expect_true(all(mfx1$std.error != mfx2$std.error))
expect_true(all(mfx1$std.error != mfx3$std.error))
expect_true(all(mfx2$std.error != mfx3$std.error))
expect_true(all(mfx1$conf.low != mfx2$conf.low))
expect_true(all(mfx1$conf.low != mfx3$conf.low))
expect_true(all(mfx2$conf.low != mfx3$conf.low))


# factor effects are plotted in different facets
dat <- mtcars
dat$gear_fct <- factor(dat$gear)
dat$am_log <- as.logical(dat$am)
dat <- dat
mod <- lm(cyl ~ mpg * gear_fct + am_log, data = dat)
p <- plot_slopes(mod, variables = "gear_fct", condition = "mpg")
expect_snapshot_plot(p, "plot_slopes_factor_facets")
p <- plot_slopes(mod, variables = "am_log", condition = "mpg")
expect_inherits(p, "gg")

# continuous vs. categorical x-axis
mod <- lm(mpg ~ hp * wt * factor(cyl), mtcars)
p <- plot_slopes(mod, variables = "hp", condition = "cyl")
expect_snapshot_plot(p, "plot_slopes_categorical")
p <- plot_slopes(mod, variables = "hp", condition = "wt")
expect_snapshot_plot(p, "plot_slopes_continuous")

# two conditions
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p <- plot_slopes(mod, variables = "hp", condition = c("wt", "am"))
expect_snapshot_plot(p, "plot_slopes_two_conditions", tol = 500)


# Issue #725: `newdata` argument in plotting functions
mod <- glm(vs ~ hp + am, mtcars, family = binomial)
p1 <- plot_slopes(mod, variables = "hp", by = "am", newdata = datagridcf(am = 0:1), draw = FALSE)
p2 <- avg_slopes(mod, variables = "hp", by = "am", newdata = datagridcf(am = 0:1), draw = FALSE)
expect_equivalent(p1$estimate, p2$estimate)
expect_equivalent(p1$conf.low, p2$conf.low, tolerance = 1e-6)
p3 <- plot_slopes(mod, variables = "hp", by = "am", draw = FALSE)
p4 <- avg_slopes(mod, variables = "hp", by = "am", draw = FALSE)
expect_equivalent(p3$estimate, p4$estimate)
expect_equivalent(p3$conf.low, p4$conf.low)
expect_true(all(p1$conf.low != p3$conf.low))
p5 <- plot_slopes(mod, variables = "hp", condition = "am", draw = FALSE)
p6 <- slopes(mod, variables = "hp", newdata = datagrid(am = 0:1))
expect_equivalent(p5$estimate, p6$estimate)
expect_equivalent(p5$conf.low, p6$conf.low)
expect_true(all(p1$conf.low != p5$conf.low))
expect_true(all(p3$conf.low != p5$conf.low))
expect_error(plot_slopes(mod, variables = "hp", condition = "am", by = "am"))
expect_error(plot_slopes(mod, variables = "hp", newdata = mtcars))




rm(list = ls())