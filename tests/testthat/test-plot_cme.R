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
