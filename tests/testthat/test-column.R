mod <- lm(mpg ~ hp * am * factor(cyl), data = mtcars)

# no aggregation -> can keep everything in `newdata`
pre <- predictions(mod)
cmp <- comparisons(mod)
expect_true(all(c("cyl", "hp", "mpg", "am") %in% colnames(pre)))
expect_true(all(c("cyl", "hp", "mpg", "am") %in% colnames(cmp)))

# by=TRUE -> no covariate
pre <- avg_predictions(mod)
cmp <- avg_comparisons(mod)
expect_true(!any(c("cyl", "hp") %in% colnames(pre)))
expect_true(!any(c("cyl", "hp") %in% colnames(cmp)))

# by="cyl" -> only cyl
pre <- avg_predictions(mod, by = "cyl")
cmp <- avg_comparisons(mod, by = "cyl")
expect_true("cyl" %in% colnames(pre))
expect_true("cyl" %in% colnames(cmp))
expect_false("hp" %in% colnames(pre))
expect_false("hp" %in% colnames(cmp))
expect_false("mpg" %in% colnames(pre))
expect_false("mpg" %in% colnames(cmp))

# by data.frame -> "by"
bydf <- data.frame(cyl = c(4, 6, 8), by = c("small", "small", "large"))
pre <- avg_predictions(mod, by = bydf)
expect_true("by" %in% colnames(pre))
expect_false("am" %in% colnames(pre))
expect_false("cyl" %in% colnames(pre))

# hypothesis= x ~ y | groupid -> only groupid
pre <- avg_predictions(mod,
    by = c("cyl", "am"),
    hypothesis = difference ~ reference | cyl)
cmp <- avg_comparisons(mod,
    variables = "hp",
    by = c("cyl", "am"),
    hypothesis = difference ~ reference | cyl)
expect_true("cyl" %in% colnames(pre))
expect_true("cyl" %in% colnames(cmp))
expect_false("am" %in% colnames(pre))
expect_false("am" %in% colnames(cmp))
expect_false("hp" %in% colnames(pre))
expect_false("hp" %in% colnames(cmp))

# bayesian by=TRUE -> no covariates except for `by`
if (file.exists(test_path("../../inst/tinytest/modelarchive/data/brms_interaction.rds"))) {
    m <- readRDS(test_path("../../inst/tinytest/modelarchive/data/brms_interaction.rds"))
    slo <- avg_slopes(m, variables = "mpg")
    expect_false("mpg" %in% colnames(slo))
    expect_false("vs" %in% colnames(slo))
    slo <- avg_slopes(m, variables = "mpg", by = "vs")
    expect_false("mpg" %in% colnames(slo))
    expect_true("vs" %in% colnames(slo))

    pre <- avg_predictions(m)
    expect_equal(nrow(pre), 1)
    expect_false("mpg" %in% colnames(pre))
    expect_false("vs" %in% colnames(pre))
}


# categorical outcome model always has the "group" column
if (requireNamespace("nnet", quietly = TRUE)) {
    mod <- nnet::multinom(factor(cyl) ~ mpg + hp + am, data = mtcars, trace = FALSE)
    bydf <- data.frame(group = c(4, 6, 8), by = c("small", "small", "large"))
    p1 <- predictions(mod)
    p2 <- avg_predictions(mod, by = TRUE)
    p3 <- avg_predictions(mod, by = "am")
    p4 <- avg_predictions(mod, by = bydf)
    expect_true("group" %in% colnames(p1))
    expect_true("group" %in% colnames(p2))
    expect_true("group" %in% colnames(p3))
    expect_false("group" %in% colnames(p4))
    expect_true("by" %in% colnames(p4))
}


# magic 1-length output functions should exclude covariates
if (requireNamespace("mgcv", quietly = TRUE)) {
    model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)
    slo <- avg_slopes(model, variables = "Petal.Length")
    expect_false("Petal.Length" %in% colnames(slo))
}
