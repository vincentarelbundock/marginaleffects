test_that("character variable works correctly", {
    dat <- get_dataset("penguins", "palmerpenguins")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)
    mfx <- slopes(mod)
    tid <- tidy(mfx)
    expect_true(all(c("bill_length_mm", "flipper_length_mm", "species") %in% mfx$term))
    expect_true(all(c("Chinstrap - Adelie", "Gentoo - Adelie") %in% mfx$contrast))
})


test_that("predictions work with missing character levels", {
    dat <- get_dataset("penguins", "palmerpenguins")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)

    # case 1
    pred <- predictions(mod, newdata = datagrid(species = "Chinstrap"))
    expect_s3_class(pred, "data.frame")
    expect_equal(1, nrow(pred))

    # case 2
    pred <- predictions(mod, newdata = datagrid(species = c("Chinstrap", "Gentoo")))
    expect_equal(nrow(pred), 2)
})
