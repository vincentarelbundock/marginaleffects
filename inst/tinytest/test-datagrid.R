source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("lme4")
requiet("fixest")

# informative errors
expect_error(datagrid(Petal.Length = 4.6), pattern = "inside")

# numeric clusters no longer produce a warning; selects mode
mod <- lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)
expect_false(expect_warning(datagrid(model = mod)))
expect_true(datagrid(model = mod)$cyl == 8)
