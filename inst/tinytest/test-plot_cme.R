exit_file("tinyviztest")
source("helpers.R")
using("tinyviztest")

# character predictors
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(
    large_penguin ~ bill_length_mm * flipper_length_mm + species,
    family = binomial, data = dat)
p <- plot_cme(mod, effect = "bill_length_mm", condition = "flipper_length_mm", draw = FALSE)
expect_inherits(p, "data.frame")
expect_equivalent(nrow(p), 100)
expect_false(anyNA(p$dydx))


# vcov
mod <- lm(mpg ~ hp * wt, data = mtcars)
mfx1 <- plot_cme(mod, effect = "hp", condition = "wt", draw = FALSE)
mfx2 <- plot_cme(mod, effect = "hp", condition = "wt", vcov = "HC3", draw = FALSE)
mfx3 <- plot_cme(mod, effect = "hp", condition = "wt", vcov = ~cyl, draw = FALSE)
expect_true(all(mfx1$std.error != mfx2$std.error))
expect_true(all(mfx1$std.error != mfx3$std.error))
expect_true(all(mfx2$std.error != mfx3$std.error))
expect_true(all(mfx1$conf.low != mfx2$conf.low))
expect_true(all(mfx1$conf.low != mfx3$conf.low))
expect_true(all(mfx2$conf.low != mfx3$conf.low))


# CI breaks here

# factor effects are plotted in different facets
dat <- mtcars
dat$gear_fct <- factor(dat$gear)
dat$am_log <- as.logical(dat$am)
mod <- lm(cyl ~ mpg * gear_fct + am_log, data = dat)
p <- plot_cme(mod, effect = "gear_fct", condition = "mpg")
expect_vdiff(p, "plot_cme_factor_facets")
p <- plot_cme(mod, effect = "am_log", condition = "mpg")
expect_inherits(p, "gg")

# continuous vs. categorical x-axis
mod <- lm(mpg ~ hp * wt * factor(cyl), mtcars)
p <- plot_cme(mod, effect = "hp", condition = "cyl")
expect_vdiff(p, "plot_cme_categorical")
p <- plot_cme(mod, effect = "hp", condition = "wt")
expect_vdiff(p, "plot_cme_continuous")

# two conditions
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p <- plot_cme(mod, effect = "hp", condition = c("wt", "am"))
expect_vdiff(p, "plot_cme_two_conditions")
