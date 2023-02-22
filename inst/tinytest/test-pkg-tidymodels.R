source("helpers.R")
using("marginaleffects")
requiet("tidymodels")

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(
  dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), "yes", "no")
dat$large_penguin <- factor(dat$large_penguin, levels = c("yes", "no"))

mod <- set_engine(logistic_reg(), "glm")
mod <- fit(
    mod,
    large_penguin ~ bill_length_mm + flipper_length_mm + species,
    data = dat)

mfx <- slopes(mod)
expect_inherits(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)


rm(list = ls())