source("helpers.R")

# TODO: sanitize_variables: eyex only supported for numeric predictors
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)

# errors and warnings
expect_error(comparisons(mod, variables = list(hp = "eyex")), pattern = "iqr.*minmax")
expect_error(marginaleffects(mod, transform_pre = "eyex"), pattern = "supported")

marginaleffects(mod, slope = "dydx") |> summary()
marginaleffects(mod, slope = "eyex") |> summary()
marginaleffects(mod, slope = "eydx") |> summary()
marginaleffects(mod, slope = "dyex") |> summary()

comparisons(mod, transform_pre = "eyex") |> summary()
comparisons(mod, transform_pre = "dyex") |> summary()
comparisons(mod, transform_pre = "eydx") |> summary()
comparisons(mod, transform_pre = "dyex") |> summary()



# TODO: check label ratios for categorical
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
cmp <- comparisons(mod, transform_pre = "ratio")

