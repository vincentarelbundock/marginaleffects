# source("../inst/tinytest/helpers.R")

# # TODO: sanitize_variables: eyex only supported for numeric predictors
# mod <- lm(mpg ~ hp, data = mtcars)
# comparisons(mod, variables = list(hp = "eyex")) |> summary()
# comparisons(mod, variables = list(hp = "dydx")) |> summary()
# comparisons(mod, variables = list(hp = "eydx")) |> summary()

# comparisons(mod, variables = list(hp = 2))
# comparisons(mod, variables = list(hp = "2sd"), transform_pre = "ratio")

# comparisons(mod, variables = list(hp = "dydx")) |> summary()
# comparisons(mod, variables = list(hp = "eydx")) |> summary()
# comparisons(mod, variables = list(hp = "eyex")) |> summary()
# marginaleffects(mod, variables = "hp") |> head()

# marginaleffects(mod) |> summary()


# # TODO: check label ratios for categorical
# mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
# comparisons(mod, transform_pre = "difference") |> summary()

# marginaleffects(mod, variables = list(hp = "eydx", cyl = "reference")) |> summary()
# marginaleffects(mod, variables = list(hp = "dyex")) |> summary()
# marginaleffects(mod, variables = list(hp = "eyex")) |> summary()
# comparisons(mod, variables = list(hp = "eydx", cyl = "reference")) |> summary()

# mod <- download_model("brms_numeric")

# marginaleffects(mod, variables = list(hp = "eyex", hp = "dydx"))

# comparisons(mod, variables = list(hp = "eyex")) |> summary()

