test_that("visualisation_matrix() without `x` variable", {
    requiet("modelbased")
    mod <- lm(mpg ~ hp + factor(cyl), mtcars)

    p1 <- predictions(mod, newdata = datagrid(cyl = mtcars$cyl))
    p2 <- predictions(mod, newdata = visualisation_matrix(at = "cyl"))
    expect_equal(nrow(p1), nrow(p2))
    expect_true(all(c("newdata_adjusted_for", "newdata_at_specs") %in% names(attributes(p2))))

    m1 <- marginaleffects(mod, newdata = datagrid(cyl = mtcars$cyl))
    m2 <- marginaleffects(mod, newdata = visualisation_matrix(at = "cyl"))
    expect_equal(nrow(m1), nrow(m2))
    expect_true(all(c("newdata_adjusted_for", "newdata_at_specs") %in% names(attributes(m2))))
})
