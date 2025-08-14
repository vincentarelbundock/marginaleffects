test_that("Stata comparisons produce identical results for simple model", {
    mod <- lm(mpg ~ cyl + hp + wt, data = mtcars)
    mfx <- avg_slopes(mod)
    expect_equal(mfx$estimate, c(-0.941616811900303, -0.0180381021785969, -3.16697311076308))
    expect_equal(mfx$std.error, c(0.550916180593437, 0.0118762627948526, 0.740576187204658), tolerance = 1e-3)
})


test_that("Stata comparisons produce identical results for interaction model", {
    mod <- lm(mpg ~ as.factor(cyl) * hp + wt, data = mtcars)
    mfx <- avg_slopes(
        mod,
        by = "cyl",
        variables = "hp",
        newdata = datagrid(cyl = c(4, 6, 8), grid_type = "counterfactual")
    )
    expect_equal(mfx$estimate, c(-0.0994659820285903, -0.0213767880896665, -0.0134410254648554))
    expect_equal(mfx$std.error, c(0.0348665154637227, 0.0388220444007849, 0.0125138337466129), tolerance = 1e-4)
})
