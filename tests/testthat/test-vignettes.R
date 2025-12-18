testthat::skip_if_not_installed("tidyverse")
testthat::skip_if_not_installed("survey")
testthat::skip_if_not_installed("rstan")
requiet("tidyverse")
requiet("survey")
requiet("rstan")

# Stata comparisons produces identical results
mod <- lm(mpg ~ cyl + hp + wt, data = mtcars)
mfx <- avg_slopes(mod)
expect_equal(mfx$estimate, c(-0.941616811900303, -0.0180381021785969, -3.16697311076308), ignore_attr = TRUE)
expect_equal(mfx$std.error, c(0.550916180593437, 0.0118762627948526, 0.740576187204658), tolerance = 1e-3, ignore_attr = TRUE)

mod <- lm(mpg ~ as.factor(cyl) * hp + wt, data = mtcars)
mfx <- avg_slopes(
    mod,
    by = "cyl",
    variables = "hp",
    newdata = datagrid(cyl = c(4, 6, 8), grid_type = "counterfactual")
)
expect_equal(mfx$estimate, c(-0.0994659820285903, -0.0213767880896665, -0.0134410254648554), ignore_attr = TRUE)
expect_equal(mfx$std.error, c(0.0348665154637227, 0.0388220444007849, 0.0125138337466129), tolerance = 1e-4, ignore_attr = TRUE)
