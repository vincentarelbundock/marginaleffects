
# important for modelsummary glance
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
expect_s3_class(components(predictions(mod), "model"), "lm")
expect_s3_class(components(comparisons(mod), "model"), "lm")
expect_s3_class(components(avg_slopes(mod), "model"), "lm")

# Issue #1089 white space in variable name
tmp <- mtcars
colnames(tmp)[1] <- "Miles per gallon"
mod <- lm(hp ~ wt * `Miles per gallon`, tmp)
s <- avg_slopes(mod) |> suppressWarnings()
expect_s3_class(s, "slopes")
expect_equal(nrow(s), 2, ignore_attr = TRUE)
s <- avg_slopes(mod, variables = "Miles per gallon") |> suppressWarnings()
expect_s3_class(s, "slopes")
expect_equal(nrow(s), 1, ignore_attr = TRUE)


# scale() returns a 1-column matrix
dat <- transform(mtcars, hp = scale(hp))
mod <- lm(mpg ~ hp, data = dat)
p <- predictions(mod)
expect_s3_class(p, "predictions")
expect_false(anyNA(p$estimate))
expect_false(anyNA(p$std.error))


# Issue #6 marginaleffectsJAX: missing model matrix attribute
mod_factor <- lm(mpg ~ hp + factor(cyl), data = mtcars)
p <- predictions(mod_factor, by = "cyl")
M <- attr(components(p, "newdata"), "marginaleffects_model_matrix")
expect_true(is.matrix(M))


# # Issue #1357
# testthat::skip("insight::get_datagrid() no longer returns proper e42dep")
# m <- insight::download_model("brms_linear_1")
# p <- avg_predictions(
#     m,
#     by = "e42dep",
#     newdata = insight::get_datagrid(m, by = "e42dep")
# )
# expect_s3_class(p, "predictions")
