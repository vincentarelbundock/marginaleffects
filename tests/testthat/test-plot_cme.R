skip_if(getRversion() < "4.1.0") # different graphics engines
skip_on_cran()

test_that("character predictors", {
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm * flipper_length_mm + species, 
               family = binomial, data = dat)
    p <- plot_cme(mod, effect = "bill_length_mm", condition = "flipper_length_mm", draw = FALSE)
    expect_s3_class(p, "data.frame")
    expect_equal(nrow(p), 25)
    expect_false(anyNA(p$dydx))
})


test_that("continuous vs. categorical x-axis", {
    mod <- lm(mpg ~ hp * wt * factor(cyl), mtcars)
    p <- plot_cme(mod, effect = "hp", condition = "cyl")
    vdiffr::expect_doppelganger("cme categorical x-axis", p)
    p <- plot_cme(mod, effect = "hp", condition = "wt")
    vdiffr::expect_doppelganger("cme continuous x-axis", p)
})


test_that("two conditions", {
    mod <- lm(mpg ~ hp * wt * am, data = mtcars)
    vdiffr::expect_doppelganger("cme plot with 2 conditions",
                                plot_cme(mod, effect = "hp", condition = c("wt", "am")))
})


test_that("vcov", {
    skip_if_not_installed("insight", minimum_version = "0.17.1")
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
})


test_that("factor effects are plotted in different facets", {
    dat <- mtcars
    dat$gear_fct <- factor(dat$gear)
    mod <- lm(cyl ~ mpg * gear_fct, data = dat)
    p <- plot_cme(mod, effect = "gear_fct", condition = "mpg")
    vdiffr::expect_doppelganger("factor effects in facets", p)
})
