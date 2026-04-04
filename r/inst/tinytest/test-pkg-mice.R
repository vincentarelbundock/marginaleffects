source("helpers.R")
using("marginaleffects")
requiet("mice")

# Basic expectation tests
mod_simple <- lm(mpg ~ wt + am, data = mtcars)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

dat <- iris
dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice <- mice::mice(dat, m = 20, printFlag = FALSE, .Random.seed = 1024)
mir <- with(dat_mice, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))
mod <- lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species, data = dat)

mfx1 <- suppressWarnings(avg_slopes(mir, by = "Species"))
mfx2 <- avg_slopes(mod, by = "Species")
expect_inherits(mfx1, "slopes")
expect_equivalent(nrow(mfx1), nrow(mfx2))


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
dat_mice <- complete(imp, "all")
fit_logistic <- function(dat) {
    mod <- glm(endp ~ trt, family = binomial(link = "logit"), data = dat)
    out <- avg_slopes(mod, newdata = dat)
    return(out)
}
mod_imputation <- suppressWarnings(lapply(dat_mice, fit_logistic))
manu <- suppressWarnings(summary(pool(mod_imputation), conf.int = TRUE))
fit <- with(imp, glm(endp ~ trt, family = binomial(link = "logit")))
auto <- suppressWarnings(avg_slopes(fit))
expect_equivalent(auto$estimate, manu$estimate)
expect_equivalent(auto$std.error, manu$std.error, tolerance = 1e-6)


# Issue #1420
set.seed(1024)
dat <- iris
dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice <- mice(dat, m = 20, printFlag = FALSE, .Random.seed = 1024)
mod_mice <- with(dat_mice, lm(Petal.Width ~ Sepal.Length))
h <- hypotheses(mod_mice, hypothesis = "Sepal.Length = 0", equivalence = c(-1, 1))
expect_false(anyNA(h$std.error))
expect_false(anyNA(h$estimate))

# Issue #1682: avg_comparisons() with mira + bs() + subset= gives wrong results
requiet("splines")
set.seed(42)
n <- 400
dat1682 <- data.frame(
    age  = runif(n, 50, 90),
    x1   = rbinom(n, 1, 0.5),
    x2   = rnorm(n),
    year = sample(2019:2024, n, replace = TRUE)
)
dat1682$y <- rgamma(n,
    shape = 4,
    rate  = 4 / exp(1.2 - 0.3 * dat1682$x1 + 0.005 * (dat1682$age - 70)^2 / 100 + 0.1 * dat1682$x2)
)
dat1682$x2[sample(n, 60)] <- NA
dat1682$age[sample(n, 20)] <- NA
imp1682 <- mice(
    data.frame(y = dat1682$y, x1 = dat1682$x1, x2 = dat1682$x2, age = dat1682$age, year = dat1682$year),
    m = 5, seed = 123, printFlag = FALSE
)
fit_mids1682 <- with(
    imp1682,
    glm(y ~ x1 + bs(age, 3) + x2,
        family = Gamma(link = "log"),
        subset = year >= 2022)
)
# without newdata (was buggy)
res_auto <- avg_comparisons(fit_mids1682, variables = "x1", type = "response")
# manual mira approach (known correct)
fits_manual1682 <- lapply(1:5, function(i) {
    d <- complete(imp1682, i)
    d <- d[d$year >= 2022, ]
    glm(y ~ x1 + bs(age, 3) + x2, family = Gamma(link = "log"), data = d)
})
fit_manual1682 <- mice::as.mira(fits_manual1682)
res_manual <- avg_comparisons(fit_manual1682, variables = "x1", type = "response")
# estimates should be close (not near-zero vs correct)
expect_equivalent(res_auto$estimate, res_manual$estimate, tolerance = 0.1)


# Issue #1682: warning when mids data cannot be recovered and newdata is NULL
local({
    fits_local <- lapply(1:3, function(i) lm(mpg ~ cyl + disp, data = mtcars))
    mira_no_mids <- mice::as.mira(fits_local)
    expect_warning(
        avg_comparisons(mira_no_mids, variables = "cyl"),
        pattern = "Could not recover"
    )
})


exit_file("scoping")
# Issue 1269: transforms must be apply after pooling
data("lalonde_mis", package = "cobalt")
dat <- lalonde_mis
imp <- mice(dat, print = FALSE, seed = 48103)
fits <- with(imp, glm(treat ~ age + married, family = binomial))
cmp1 <- suppressWarnings(avg_comparisons(fits, variables = "married", comparison = "lnratioavg", transform = "exp"))
expect_equivalent(cmp1$estimate, 0.3380001, tol = 1e-6)
expect_equivalent(cmp1$conf.low, 0.2386019, tol = 1e-3)
cmp2 <- suppressWarnings(avg_comparisons(fits, variables = "married", comparison = "lnratioavg"))
expect_equivalent(cmp2$estimate, -1.084709, tol = 1e-6)
expect_equivalent(cmp2$conf.low, -1.432959, tol = 1e-3)


# Issue #1117
imp <- mice(dat, print = FALSE)
est <- with(imp, lm(re78 ~ treat * (age + educ + re74)))
pre <- avg_predictions(est, variables = "treat", newdata = subset(treat == 1))
expect_inherits(pre, "predictions")
cmp <- avg_comparisons(est, variables = "treat", newdata = subset(treat == 1))
expect_inherits(cmp, "comparisons")
