source("helpers.R")
using("marginaleffects")
requiet("systemfit")
requiet("MASS")

# issue #1233: duplicated coefficient names
set.seed(12345)
N <- 100
C <- matrix(c(1, .3, .3, .3, 1, .3, .3, .3, 1), 3, 3)
colnames(C) <- rownames(C) <- c("x1", "x2", "x3")
data <- as.data.frame(mvrnorm(N, rep(0, 3), C))
sys <- with(data, {
    y1 <- 2 * x1 + 3 * x2 + x3 + 10 * rnorm(N)
    m1 <- lm(y1 ~ x1 + x2 + x3)
    y2 <- x1 + 3 * x2 + 2 * x3 + 10 * rnorm(N)
    m2 <- lm(y2 ~ x1 + x2 + x3)
    systemfit(list(formula(m1), formula(m2)), data = data)
})

expect_slopes(sys)
expect_comparisons(sys)
expect_predictions(sys)
expect_hypotheses(sys)

slo <- slopes(sys)
cmp <- comparisons(sys)
hyp <- hypotheses(sys)
expect_silent(predictions(sys))
pre <- suppressWarnings(predictions(sys))
expect_false(anyNA(slo$std.error))
expect_false(anyNA(cmp$std.error))
expect_false(anyNA(hyp$std.error))
expect_false(anyNA(pre$std.error))


# set_coef.systemfit() must find the coefficients of *named* equations, whose
# `coef()` labels are the equation names rather than "eq1", "eq2", ...
sys_named <- systemfit(list(demand = mpg ~ hp, supply = qsec ~ hp), data = mtcars)
b <- get_coef(sys_named)
sys_roundtrip <- set_coef(sys_named, b)
expect_equivalent(
    coef(sys_roundtrip$eq[[1]]),
    coef(sys_named$eq[[1]]))
expect_equivalent(
    coef(sys_roundtrip$eq[[2]]),
    coef(sys_named$eq[[2]]))

# each equation is estimated by OLS, so the slopes match the standalone models
slo <- avg_slopes(sys_named)
for (i in seq_along(sys_named$eq)) {
    ols <- lm(formula(sys_named$eq[[i]]), data = mtcars)
    ref <- avg_slopes(ols)
    expect_equivalent(slo$estimate[i], ref$estimate, tolerance = 1e-6)
    expect_equivalent(slo$std.error[i], ref$std.error, tolerance = 1e-4)
}
