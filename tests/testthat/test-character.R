test_that("character variable", {
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)
    mfx <- marginaleffects(mod)
    tid <- tidy(mfx)
    expect_true(all(c("bill_length_mm", "flipper_length_mm", "speciesChinstrap", "speciesGentoo") %in% mfx$term))
})


test_that("missing character levels", {
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)
    mfx <- predictions(mod, newdata = typical(species = c("Chinstrap")))
    expect_s3_class(mfx, "data.frame")
    expect_equal(1, nrow(mfx))
})
