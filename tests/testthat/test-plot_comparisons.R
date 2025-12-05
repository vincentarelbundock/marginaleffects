testthat::skip_if_not_installed("vdiffr")
testthat::skip_if_not_installed("ggplot2")
requiet("vdiffr")
requiet("ggplot2")


mod <- lm(mpg ~ wt * hp, data = mtcars)

p <- plot_comparisons(mod, variables = list(hp = "minmax"), condition = "wt", draw = FALSE)
expect_equal(length(unique(p$estimate)), 50, ignore_attr = TRUE)
p <- plot_comparisons(mod, variables = list(hp = "minmax"), condition = "wt")
expect_s3_class(p, "gg")
p <- plot_comparisons(mod, variables = list(hp = "iqr"), condition = "wt")
p <- plot_comparisons(mod, variables = list("hp" = c(100, 130)), condition = "wt")
expect_s3_class(p, "gg")


# representative values
p <- plot_comparisons(mod, variables = list(hp = "minmax"), condition = list("wt" = "threenum"))
vdiffr::expect_doppelganger("plot_comparisons-minmax_x", p)

# two effects
p <- plot_comparisons(mod, variables = c("hp", "wt"), condition = "wt")
vdiffr::expect_doppelganger("plot_comparisons-2effects", p)

# bug from examples (revise_get_data() no longer returns a factor attribute)
mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
p <- plot_comparisons(mod, variables = "hp", condition = list("am", "drat" = 3:5), draw = FALSE)
expect_s3_class(p, "data.frame")


# Issue #592
mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
p <- plot_comparisons(mod, variables = "hp", condition = list("am", "drat" = 3:5))
expect_s3_class(p, "gg")


# Issue #545: blank graph
dat_titanic <- get_dataset("Titanic", "Stat2Data")
mod2 <- glm(Survived ~ Age, data = dat_titanic, family = binomial)
p <- plot_comparisons(
    mod2,
    variables = list("Age" = 10),
    condition = "Age",
    comparison = "ratio"
) +
    ylab("Adjusted Risk Ratio\nP(Survived = 1 | Age + 10) / P(Survived = 1 | Age)")
vdiffr::expect_doppelganger("plot_comparisons-rr_titanic", p)


# Issue #725: `newdata` argument in plotting functions
mod <- glm(vs ~ hp + am, mtcars, family = binomial)
p1 <- plot_comparisons(
    mod,
    variables = "hp",
    by = "am",
    newdata = datagrid(am = 0:1, grid_type = "counterfactual"),
    draw = FALSE
)
p2 <- avg_comparisons(
    mod,
    variables = "hp",
    by = "am",
    draw = FALSE,
    newdata = datagrid(am = 0:1, grid_type = "counterfactual")
)
expect_equal(p1$estimate, p2$estimate, ignore_attr = TRUE)
expect_equal(p1$conf.low, p2$conf.low, tolerance = 1e-6, ignore_attr = TRUE)
p3 <- plot_comparisons(mod, variables = "hp", by = "am", draw = FALSE)
p4 <- avg_comparisons(mod, variables = "hp", by = "am", draw = FALSE)
expect_equal(p3$estimate, p4$estimate, ignore_attr = TRUE)
expect_equal(p3$conf.low, p4$conf.low, ignore_attr = TRUE)
expect_true(all(p1$conf.low != p3$conf.low))
p5 <- plot_comparisons(mod, variables = "hp", condition = "am", draw = FALSE)
p6 <- comparisons(mod, variables = "hp", newdata = datagrid(am = 0:1))
expect_equal(p5$estimate, p6$estimate, ignore_attr = TRUE)
expect_equal(p5$conf.low, p6$conf.low, ignore_attr = TRUE)
expect_true(all(p1$conf.low != p5$conf.low))
expect_true(all(p3$conf.low != p5$conf.low))
expect_error(plot_comparisons(mod, variables = "hp", condition = "am", by = "am"))
expect_error(plot_comparisons(mod, variables = "hp", newdata = mtcars))
