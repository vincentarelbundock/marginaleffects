source("helpers.R", local = TRUE)

# character variable
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)
mfx <- marginaleffects(mod)
tid <- tidy(mfx)
expect_true(all(c("bill_length_mm", "flipper_length_mm", "species") %in% mfx$term))
expect_true(all(c("Chinstrap - Adelie", "Gentoo - Adelie") %in% mfx$contrast))


# predictions: missing character levels
# get_predicted does not work here, but we successfully fall back on get_predict
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
