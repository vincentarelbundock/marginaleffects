testthat::skip_if_not_installed("vdiffr")
requiet("vdiffr")

# character predictors
dat_plot_slopes <- get_dataset("penguins", "palmerpenguins")
dat_plot_slopes$large_penguin <- ifelse(dat_plot_slopes$body_mass_g > median(dat_plot_slopes$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(
    large_penguin ~ bill_length_mm * flipper_length_mm + species,
    family = binomial,
    data = dat_plot_slopes
)
p <- plot_slopes(mod, variables = "bill_length_mm", condition = "flipper_length_mm", draw = FALSE)
expect_s3_class(p, "data.frame")
expect_equal(nrow(p), 50, ignore_attr = TRUE)
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
dat_plot_slopes2 <- mtcars
dat_plot_slopes2$gear_fct <- factor(dat_plot_slopes2$gear)
dat_plot_slopes2$am_log <- as.logical(dat_plot_slopes2$am)
dat_plot_slopes2 <- dat_plot_slopes2
mod <- lm(cyl ~ mpg * gear_fct + am_log, data = dat_plot_slopes2)
p <- plot_slopes(mod, variables = "gear_fct", condition = "mpg")
vdiffr::expect_doppelganger("plot_slopes_factor_facets", p)
p <- plot_slopes(mod, variables = "am_log", condition = "mpg")
expect_s3_class(p, "gg")

# continuous vs. categorical x-axis
mod <- lm(mpg ~ hp * wt * factor(cyl), mtcars)
p <- plot_slopes(mod, variables = "hp", condition = "cyl")
vdiffr::expect_doppelganger("plot_slopes_categorical", p)
p <- plot_slopes(mod, variables = "hp", condition = "wt")
vdiffr::expect_doppelganger("plot_slopes_continuous", p)

# two conditions
mod <- lm(mpg ~ hp * wt * am, data = mtcars)
p <- plot_slopes(mod, variables = "hp", condition = c("wt", "am"))
vdiffr::expect_doppelganger("plot_slopes_two_conditions", p)


# Issue #725: `newdata` argument in plotting functions
mod <- glm(vs ~ hp + am, mtcars, family = binomial)
p1 <- plot_slopes(
    mod,
    variables = "hp",
    by = "am",
    draw = FALSE,
    newdata = datagrid(am = 0:1, grid_type = "counterfactual")
)
p2 <- avg_slopes(mod, variables = "hp", by = "am", newdata = datagrid(am = 0:1, grid_type = "counterfactual"))
expect_equal(p1$estimate, p2$estimate, ignore_attr = TRUE)
expect_equal(p1$conf.low, p2$conf.low, tolerance = 1e-6, ignore_attr = TRUE)
p3 <- plot_slopes(mod, variables = "hp", by = "am", draw = FALSE)
p4 <- avg_slopes(mod, variables = "hp", by = "am", draw = FALSE)
expect_equal(p3$estimate, p4$estimate, ignore_attr = TRUE)
expect_equal(p3$conf.low, p4$conf.low, ignore_attr = TRUE)
expect_true(all(p1$conf.low != p3$conf.low))
p5 <- plot_slopes(mod, variables = "hp", condition = "am", draw = FALSE)
p6 <- slopes(mod, variables = "hp", newdata = datagrid(am = 0:1))
expect_equal(p5$estimate, p6$estimate, ignore_attr = TRUE)
expect_equal(p5$conf.low, p6$conf.low, ignore_attr = TRUE)
expect_true(all(p1$conf.low != p5$conf.low))
expect_true(all(p3$conf.low != p5$conf.low))
expect_error(plot_slopes(mod, variables = "hp", condition = "am", by = "am"))
expect_error(plot_slopes(mod, variables = "hp", newdata = mtcars))

# Plot 4 variables in condition using facet_grid
mod <- lm(mpg ~ hp * drat * factor(am) * carb, data = mtcars)
p <- plot_slopes(
    mod,
    variables = c("hp", "drat"),
    condition = list("am", "drat" = 3:5, "hp" = c(10, 15), "carb" = c(2, 3))
)
expect_s3_class(p, "gg")
