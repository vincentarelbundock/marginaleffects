source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")

# marginal effects at the mean
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
mfx1 <- marginaleffects(mod, newdata = datagrid())
mfx2 <- marginaleffects(mod, newdata = "mean")
expect_equivalent(mfx1, mfx2)



# unsupported arguments
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
expect_error(marginaleffects(mod, contrast_numeric = "sd"), pattern = "supported")
expect_error(marginaleffects(mod, contrast_factor = "pairwise"), pattern = "supported")
expect_error(marginaleffects(mod, transform_pre = mean), pattern = "supported")
expect_error(marginaleffects(mod, transform_post = exp), pattern = "supported")
expect_error(marginaleffects(mod, cross = TRUE), pattern = "supported")

