source("helpers.R")
using("marginaleffects")

if (!AUTODIFF) exit_file("autodiff-pipeline")

tol_b <- 1e-5
tol_se <- 1e-4

dat <- transform(mtcars, cyl = factor(cyl))
mod <- lm(mpg ~ hp + wt + cyl, data = dat)

autodiff(FALSE)
pre1 <- avg_predictions(mod, by = "cyl", wts = "gear")
autodiff(TRUE)
expect_message(pre2 <- avg_predictions(mod, by = "cyl", wts = "gear"))
expect_equal(pre1$estimate, pre2$estimate, tol = tol_b)
expect_equal(pre1$std.error, pre2$std.error, tol = tol_se)

H <- matrix(c(1, -1, 0), nrow = 3)
autodiff(FALSE)
pre1 <- avg_predictions(mod, by = "cyl", hypothesis = H)
autodiff(TRUE)
expect_message(pre2 <- avg_predictions(mod, by = "cyl", hypothesis = H))
expect_equal(pre1$estimate, pre2$estimate, tol = tol_b)
expect_equal(pre1$std.error, pre2$std.error, tol = tol_se)

autodiff(FALSE)
pre1 <- avg_predictions(mod, by = "cyl", hypothesis = c(1, -1, 0))
autodiff(TRUE)
expect_message(pre2 <- avg_predictions(mod, by = "cyl", hypothesis = c(1, -1, 0)))
expect_equal(pre1$estimate, pre2$estimate, tol = tol_b)
expect_equal(pre1$std.error, pre2$std.error, tol = tol_se)

bydf <- data.frame(
    cyl = factor(c(4, 6, 8), levels = levels(dat$cyl)),
    by = c("four", "six", "eight")
)
autodiff(FALSE)
pre1 <- avg_predictions(mod, by = bydf)
autodiff(TRUE)
expect_message(pre2 <- avg_predictions(mod, by = bydf))
expect_equal(pre1$estimate, pre2$estimate, tol = tol_b)
expect_equal(pre1$std.error, pre2$std.error, tol = tol_se)

autodiff(FALSE)
cmp1 <- avg_comparisons(mod, variables = "hp", by = "cyl", comparison = "ratio")
autodiff(TRUE)
expect_message(cmp2 <- avg_comparisons(mod, variables = "hp", by = "cyl", comparison = "ratio"))
expect_equal(cmp1$estimate, cmp2$estimate, tol = tol_b)
expect_equal(cmp1$std.error, cmp2$std.error, tol = tol_se)

autodiff(FALSE)
pre1 <- predictions(mod, newdata = datagrid(cyl = factor(c(4, 6), levels = levels(dat$cyl))))
autodiff(TRUE)
expect_message(pre2 <- predictions(mod, newdata = datagrid(cyl = factor(c(4, 6), levels = levels(dat$cyl)))))
expect_equal(pre1$estimate, pre2$estimate, tol = tol_b)
expect_equal(pre1$std.error, pre2$std.error, tol = tol_se)

autodiff(FALSE)
cmp1 <- avg_comparisons(mod, variables = "hp", comparison = function(hi, lo) hi - lo)
autodiff(TRUE)
expect_warning(cmp2 <- avg_comparisons(mod, variables = "hp", comparison = function(hi, lo) hi - lo))
expect_equal(cmp1$estimate, cmp2$estimate, tol = tol_b)
expect_equal(cmp1$std.error, cmp2$std.error, tol = tol_se)
