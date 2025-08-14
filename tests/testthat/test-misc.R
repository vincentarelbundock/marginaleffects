test_that("model attributes are preserved for modelsummary glance", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    mod <- lm(mpg ~ am + factor(cyl), tmp)
    expect_s3_class(attr(predictions(mod), "model"), "lm")
    expect_s3_class(attr(comparisons(mod), "model"), "lm")
    expect_s3_class(attr(avg_slopes(mod), "model"), "lm")
})


test_that("Issue #1089: white space in variable name works", {
    tmp <- mtcars
    colnames(tmp)[1] <- "Miles per gallon"
    mod <- lm(hp ~ wt * `Miles per gallon`, tmp)
    s <- avg_slopes(mod) |> suppressWarnings()
    expect_s3_class(s, "slopes")
    expect_equal(nrow(s), 2)
    s <- avg_slopes(mod, variables = "Miles per gallon") |> suppressWarnings()
    expect_s3_class(s, "slopes")
    expect_equal(nrow(s), 1)
})


test_that("scale() returns a 1-column matrix works", {
    dat <- transform(mtcars, hp = scale(hp))
    mod <- lm(mpg ~ hp, data = dat)
    p <- predictions(mod)
    expect_s3_class(p, "predictions")
    expect_false(anyNA(p$estimate))
    expect_false(anyNA(p$std.error))
})


test_that("Issue #6: marginaleffectsJAX missing model matrix attribute", {
    mod_factor <- lm(mpg ~ hp + factor(cyl), data = mtcars)
    p <- predictions(mod_factor, by = "cyl")
    M <- attr(attr(p, "newdata"), "marginaleffects_model_matrix")
    expect_true(inherits(M, "matrix"))
})
