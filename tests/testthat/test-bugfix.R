test_that("Bug stay dead: Issue 55", {
    # Error: Argument 1 must have names.
    # vab: possibly caused by a version of `emmeans` < 1.6.3
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, 
               data = dat, family = binomial)
    mfx <- marginaleffects(mod, variables = "species")
    expect_s3_class(mfx, "data.frame")
    expect_true(nrow(mfx) > 0)
    expect_true(ncol(mfx) > 0)
})
