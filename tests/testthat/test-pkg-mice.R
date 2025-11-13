testthat::skip_if_not_installed("mice")
requiet("mice")

# Basic expectation tests
mod_simple <- lm(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

dat_mice1 <- iris
dat_mice1$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice1$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice1$Species[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice <- mice::mice(dat_mice1, m = 20, printFlag = FALSE, .Random.seed = 1024)
mir <- with(dat_mice, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))
mod <- lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species, data = dat_mice1)

mfx1 <- suppressWarnings(avg_slopes(mir, by = "Species"))
mfx2 <- avg_slopes(mod, by = "Species")
expect_s3_class(mfx1, "slopes")
expect_equal(nrow(mfx1), nrow(mfx2), ignore_attr = TRUE)


# Issue #711
data <- structure(
    list(
        id = 1:37,
        trt = c(
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm"
        ),
        endp = structure(
            c(
                1L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                2L,
                2L,
                2L,
                2L,
                2L,
                2L,
                1L,
                1L,
                1L,
                1L,
                1L,
                1L,
                1L,
                2L,
                2L,
                2L,
                2L,
                2L,
                1L,
                1L,
                1L,
                1L
            ),
            levels = c("TRUE", "FALSE"),
            class = "factor"
        )
    ),
    row.names = c(
        NA,
        -37L
    ),
    class = "data.frame"
)
data$endp <- factor(data$endp, levels = c("TRUE", "FALSE"))
data_miss <- data
data_miss[c(1, 5, 7, 30), c("endp")] <- NA
imp <- suppressWarnings(mice::mice(data_miss, m = 20, method = "pmm", maxit = 50, seed = 1000, print = FALSE))
dat_mice_complete <- complete(imp, "all")
fit_logistic <- function(dat) {
    mod <- glm(endp ~ trt, family = binomial(link = "logit"), data = dat)
    out <- avg_slopes(mod, newdata = dat)
    return(out)
}
mod_imputation <- suppressWarnings(lapply(dat_mice_complete, fit_logistic))
manu <- suppressWarnings(summary(pool(mod_imputation), conf.int = TRUE))
fit <- with(imp, glm(endp ~ trt, family = binomial(link = "logit")))
auto <- suppressWarnings(avg_slopes(fit))
expect_equal(auto$estimate, manu$estimate, ignore_attr = TRUE)
expect_equal(auto$std.error, manu$std.error, tolerance = 1e-6, ignore_attr = TRUE)


# Issue #1420
set.seed(1024)
dat_mice2 <- iris
dat_mice2$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice2$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice2$Species[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice_imp2 <- mice(dat_mice2, m = 20, printFlag = FALSE, .Random.seed = 1024)
mod_mice <- with(dat_mice_imp2, lm(Petal.Width ~ Sepal.Length))
h <- hypotheses(mod_mice, hypothesis = "Sepal.Length = 0", equivalence = c(-1, 1))
expect_false(anyNA(h$std.error))
expect_false(anyNA(h$estimate))

testthat::skip("scoping")
# Issue 1269: transforms must be apply after pooling
data("lalonde_mis", package = "cobalt")
dat_mice3 <- lalonde_mis
imp2 <- mice(dat_mice3, print = FALSE, seed = 48103)
fits <- with(imp2, glm(treat ~ age + married, family = binomial))
cmp1 <- suppressWarnings(avg_comparisons(fits, variables = "married", comparison = "lnratioavg", transform = "exp"))
expect_equal(cmp1$estimate, 0.3380001, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(cmp1$conf.low, 0.2386019, tolerance = 1e-3, ignore_attr = TRUE)
cmp2 <- suppressWarnings(avg_comparisons(fits, variables = "married", comparison = "lnratioavg"))
expect_equal(cmp2$estimate, -1.084709, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(cmp2$conf.low, -1.432959, tolerance = 1e-3, ignore_attr = TRUE)


# Issue #1117
imp3 <- mice(dat_mice3, print = FALSE)
est <- with(imp3, lm(re78 ~ treat * (age + educ + re74)))
pre <- avg_predictions(est, variables = "treat", newdata = subset(treat == 1))
expect_s3_class(pre, "predictions")
cmp <- avg_comparisons(est, variables = "treat", newdata = subset(treat == 1))
expect_s3_class(cmp, "comparisons")
