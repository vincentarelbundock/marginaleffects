source("helpers.R")
using("marginaleffects")
set.seed(1024)

# Unsupported classes fail closed instead of guessing through insight.
unsupported <- structure(list(), class = "unsupported_model_matrix")
expect_null(marginaleffects:::get_model_matrix(unsupported, data.frame(x = 1)))

mod <- lm(mpg ~ hp + factor(cyl), mtcars)
p <- avg_predictions(mod, newdata = "balanced")
M <- attr(components(p, "newdata"), "marginaleffects_model_matrix")
expect_true(is.matrix(M))

# Cached matrices discard observation labels but preserve every part used for
# prediction and analytic differentiation.
mod <- lm(mpg ~ hp + factor(cyl) + splines::ns(wt, 3), data = mtcars)
newdata <- mtcars[1:12, , drop = FALSE]
rownames(newdata) <- paste0("observation_", seq_len(nrow(newdata)))
standard <- marginaleffects:::get_model_matrix(mod, newdata)
p <- predictions(mod, newdata = newdata)
cached <- attr(components(p, "newdata"), "marginaleffects_model_matrix")
expect_identical(dim(cached), dim(standard))
expect_identical(colnames(cached), colnames(standard))
expect_identical(unname(cached), unname(standard))
expect_null(rownames(cached))

mod <- lm(mpg ~ factor(cyl), data = mtcars)
p <- predictions(mod, by = "cyl") 
M <- attr(components(p, "newdata"), "marginaleffects_model_matrix")
expect_true(is.matrix(M))
