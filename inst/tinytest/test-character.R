source("helpers.R")
using("marginaleffects")

# character variable
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
dat <- dat
mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)
mfx <- slopes(mod)
tid <- tidy(mfx)
expect_true(all(c("bill_length_mm", "flipper_length_mm", "species") %in% mfx$term))
expect_true(all(c("Chinstrap - Adelie", "Gentoo - Adelie") %in% mfx$contrast))


# predictions: missing character levels
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)
# case 1
pred <- predictions(mod, newdata = datagrid(species = "Chinstrap"))
expect_inherits(pred, "data.frame")
expect_equivalent(1, nrow(pred))
# case 2
pred <- predictions(mod, newdata = datagrid(species = c("Chinstrap", "Gentoo")))
expect_equivalent(nrow(pred), 2)


rm(list = ls())