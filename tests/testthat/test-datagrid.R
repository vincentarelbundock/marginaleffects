test_that("informative errors", {
    expect_error(datagrid(Petal.Length = 4.6), regexp = "inside")
})

test_that("numeric clusters warning", {
    requiet("lme4")
    requiet("fixest")

    mod <- lmer(mpg ~ hp + (1 + drat | cyl), data = mtcars)
    expect_warning(datagrid(model = mod), regex = "cluster")

    mod <- feols(mpg ~ hp | cyl, data = mtcars)
    expect_warning(datagrid(model = mod), regex = "cluster")
})
