skip_on_ci() # different graphics engine produce different snapshots

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
