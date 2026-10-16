source("helpers.R")
using("marginaleffects")

if (!requiet("WeightIt") || !requiet("cobalt")) {
    exit_file("WeightIt")
}

# With estimated weights, the weight-estimation correction lives in vcov(), so
# the analytic Jacobian is the same as for glm.
data("lalonde", package = "cobalt")
dat <- lalonde
f <- re78 ~ treat * (age + educ + re74)
w <- WeightIt::weightit(
    treat ~ age + educ + race + married + nodegree + re74 + re75,
    data = dat,
    method = "glm",
    estimand = "ATE")
mod <- WeightIt::glm_weightit(f, data = dat, weightit = w)

p <- avg_predictions(mod, by = "treat")
expect_equal(components(p, "jacobian_method"), "analytic")

# exact oracle: the finite-difference fallback is only accurate to ~1e-7 here
X <- model.matrix(f, dat)
exact <- sapply(p$treat, function(g) {
    J <- colMeans(X[dat$treat == g, , drop = FALSE])
    sqrt(drop(t(J) %*% vcov(mod) %*% J))
})
expect_equivalent(p$std.error, exact, tolerance = 1e-10)
