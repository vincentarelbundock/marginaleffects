
source("helpers.R")
using("marginaleffects")
requiet("tidymodels")

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(
  dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), "yes", "no")
dat$large_penguin <- factor(dat$large_penguin, levels = c("yes", "no"))


# class
mod <- set_engine(logistic_reg(), "glm") |>
  fit(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat)

p <- predictions(mod, newdata = dat, type = "prob")
expect_inherits(p, "predictions")
expect_true("std.error" %in% colnames(p))

p <- predictions(mod, newdata = dat, type = "class")
expect_inherits(p, "predictions")
expect_false("std.error" %in% colnames(p))

mfx <- avg_slopes(mod, newdata = dat, type = "prob")
expect_inherits(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)



# workflow: engine supported
data("bikes", package = "fmeffects")

mod <- workflow(count ~ ., linear_reg()) |>
  fit(data = bikes) |>
  suppressWarnings()

p <- predictions(mod, newdata = bikes, type = "numeric") |>
  suppressWarnings()
expect_inherits(p, "predictions")
expect_true("std.error" %in% colnames(p))

mfx <- avg_slopes(mod, newdata = bikes, type = "numeric") |>
  suppressWarnings()
expect_inherits(mfx, "marginaleffects")
expect_true(nrow(mfx) > 0)


# workflow: engine not supported
mod <- workflow(count ~ ., rand_forest(mode = "regression")) |>
  fit(data = bikes) |>
  suppressWarnings()

p <- predictions(mod, newdata = bikes, type = "numeric")
expect_inherits(p, "predictions")
expect_false("std.error" %in% colnames(p))

m <- slopes(mod, newdata = bikes, type = "numeric")
expect_inherits(m, "slopes")
expect_false("std.error" %in% colnames(m))




rm(list = ls())