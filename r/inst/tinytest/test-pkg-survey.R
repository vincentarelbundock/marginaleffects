source("helpers.R")
using("marginaleffects")

requiet("margins")
requiet("emmeans")
requiet("broom")
requiet("survey")

# Basic expectation tests
mod_simple <- lm(mpg ~ wt + am, data = mtcars)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# survey: marginaleffects vs. margins vs. emtrends
data("fpc", package = "survey")
svyd <- survey::svydesign(
    weights = ~weight,
    ids = ~psuid,
    strata = ~stratid,
    fpc = ~Nh,
    variables = ~ x + nh,
    data = fpc,
    nest = TRUE
)
mod <- survey::svyglm(x ~ nh, design = svyd)
f <- function(x) stats::coef(x)["nh"]
h <- hypotheses(mod, hypothesis = f)
expect_inherits(h, "hypotheses")
expect_equivalent(h$estimate, stats::coef(mod)["nh"])
h <- hypotheses(mod, hypothesis = f, wts = stats::weights(svyd))
expect_inherits(h, "hypotheses")
expect_equivalent(h$estimate, stats::coef(mod)["nh"])
res <- slopes(mod, wts = "(weights)")
mar <- suppressMessages(data.frame(margins(mod, unit_ses = TRUE)))
expect_equivalent(res$estimate, as.numeric(mar$dydx_nh))
expect_equivalent(res$std.error, as.numeric(mar$SE_dydx_nh), tolerance = 0.001)
# emtrends
em <- emtrends(mod, ~nh, "nh", at = list(nh = 4))
em <- tidy(em)
mfx <- slopes(mod, type = "link", newdata = data.frame(nh = 4))
expect_equivalent(mfx$estimate, em$nh.trend, tolerance = .001) # CRAN tolerance
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)


# Exact svyglm model matrices and analytic Jacobians. The spline term checks
# that we reproduce predict.svyglm(), rather than insight::get_modelmatrix().
set.seed(48103)
n <- 300
dat <- data.frame(
    y = rbinom(n, 1, .5),
    x = rnorm(n),
    z = runif(n),
    g = factor(sample(letters[1:3], n, replace = TRUE)),
    w = runif(n, .5, 2)
)
design <- svydesign(ids = ~1, weights = ~w, data = dat)
mod_analytic <- svyglm(
    y ~ x * g + splines::ns(z, 3),
    design = design,
    family = quasibinomial()
)
newdata <- dat[1:25, , drop = FALSE]
X <- marginaleffects:::get_model_matrix(mod_analytic, newdata)
eta <- drop(X %*% coef(mod_analytic))
expect_equivalent(
    eta,
    as.numeric(predict(mod_analytic, newdata, type = "link", se.fit = FALSE))
)

cmp_analytic <- avg_comparisons(
    mod_analytic,
    variables = "x",
    by = "g",
    wts = "(weights)"
)
J_analytic <- components(cmp_analytic, "jacobian")
expect_true(is.matrix(J_analytic))
expect_identical(colnames(J_analytic), names(coef(mod_analytic)))

fun <- function(b) {
    mod_tmp <- marginaleffects::set_coef(mod_analytic, b)
    avg_comparisons(
        mod_tmp,
        variables = "x",
        by = "g",
        wts = "(weights)",
        vcov = FALSE
    )$estimate
}
J_finite <- marginaleffects:::get_jacobian_fdforward(fun, coef(mod_analytic))
colnames(J_finite) <- names(coef(mod_analytic))
expect_equivalent(J_analytic, J_finite, tolerance = 1e-5)

pred_analytic <- predictions(
    mod_analytic,
    newdata = newdata,
    type = "response"
)
J_pred <- components(pred_analytic, "jacobian")
expected_pred <- X * as.vector(mod_analytic$family$mu.eta(eta))
attributes(expected_pred) <- list(
    dim = dim(expected_pred),
    dimnames = list(NULL, names(coef(mod_analytic)))
)
expect_equivalent(J_pred, expected_pred)

assign("svyglm_fd_calls", 0L, envir = .GlobalEnv)
suppressMessages(invisible(utils::capture.output(
    trace(
        "get_jacobian_fdforward",
        where = asNamespace("marginaleffects"),
        tracer = quote({
            .GlobalEnv$svyglm_fd_calls <- .GlobalEnv$svyglm_fd_calls + 1L
        }),
        print = FALSE
    )
)))
tryCatch(
    {
        avg_comparisons(
            mod_analytic,
            variables = "x",
            by = "g",
            wts = "(weights)",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$svyglm_fd_calls, 0L)
    },
    finally = {
        suppressMessages(invisible(utils::capture.output(
            untrace(
                "get_jacobian_fdforward",
                where = asNamespace("marginaleffects")
            )
        )))
        rm("svyglm_fd_calls", envir = .GlobalEnv)
    }
)


# Issue #1131
data("lalonde", package = "MatchIt")
fit <- survey::svyglm(re78 ~ treat, design = survey::svydesign(~1, weights = ~1, data = lalonde))
p <- marginaleffects::get_predict(fit, newdata = lalonde)
expect_inherits(p, "data.frame")


# Issue #1161
dat <- "https://vincentarelbundock.github.io/Rdatasets/csv/AER/SmokeBan.csv"
dat <- read.csv(dat, na.strings = c("*", ""))
dat$weights <- runif(n = nrow(dat), min = 1, max = 100)
dat$smoker <- factor(dat$smoker)
design1 = svydesign(ids = ~1, weights = ~weights, data = dat)
m <- suppressWarnings(svyglm(
    smoker ~ ban * education * gender + age,
    design = design1,
    family = binomial(),
    data = dat
))
cmp <- avg_comparisons(m, variables = "education", by = c("ban", "gender"), wts = "weights", hypothesis = ~reference)
expect_false(anyNA(cmp$estimate))

# svyolr delta-method standard errors
set.seed(1234)
n <- 400
z <- rbinom(n, 1, 0.5)
x <- factor(sample(c("1", "2", "3"), n, replace = TRUE))
beta_x <- c("1" = 0, "2" = 0.4, "3" = -0.3)
beta_z <- 0.6
eta <- beta_x[x] + beta_z * z + rlogis(n)
cuts <- c(-1.5, -0.5, 0.5, 1.5)
y <- cut(eta, breaks = c(-Inf, cuts, Inf), labels = 1:5, ordered_result = TRUE)
weights <- rlnorm(n, meanlog = log(50000), sdlog = 1)
design <- svydesign(ids = ~1, weights = ~weights, data = data.frame(y, x, z, weights))
ord_svy <- svyolr(y ~ x + z, method = "logistic", design = design)
cmp <- comparisons(ord_svy, wts = "(weights)")
expect_true(any(!is.na(cmp$std.error)))
avg_cmp <- avg_comparisons(ord_svy, wts = "(weights)")
expect_true(any(!is.na(avg_cmp$std.error)))
avg_pred <- avg_predictions(ord_svy, wts = "(weights)")
expect_true(any(!is.na(avg_pred$std.error)))
