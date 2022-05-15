requiet("lme4")
requiet("fixest")

# informative errors
expect_error(datagrid(Petal.Length = 4.6), pattern = "inside")

# numeric clusters warning
mod <- lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)
expect_warning(datagrid(model = mod), pattern = "cluster")

mod <- feols(mpg ~ hp | cyl, data = mtcars)
expect_warning(datagrid(model = mod), pattern = "cluster")

