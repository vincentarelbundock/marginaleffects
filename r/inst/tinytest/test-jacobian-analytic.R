source("helpers.R")
using("marginaleffects")

# The initial matrix-native path is restricted to row-wise built-in differences.
mod_lm <- lm(mpg ~ hp + wt, data = mtcars)
cmp_lm <- comparisons(
    mod_lm,
    variables = list(hp = c(100, 110)),
    newdata = "mean"
)
expect_identical(
    components(cmp_lm, "jacobian"),
    matrix(c(0, 10, 0), nrow = 1,
        dimnames = list(NULL, names(coef(mod_lm))))
)
J_lm <- components(cmp_lm, "jacobian")
expect_equivalent(
    cmp_lm$std.error,
    sqrt(rowSums(tcrossprod(J_lm, vcov(mod_lm)) * J_lm))
)

# GLM differences are exact only on the effective link scale.
mod_glm <- glm(am ~ hp + wt, family = binomial, data = mtcars)
cmp_glm <- comparisons(
    mod_glm,
    variables = list(hp = c(100, 110)),
    newdata = "mean",
    type = "link"
)
expect_identical(
    components(cmp_glm, "jacobian"),
    matrix(c(0, 10, 0), nrow = 1,
        dimnames = list(NULL, names(coef(mod_glm))))
)
J_glm <- components(cmp_glm, "jacobian")
expect_equivalent(
    cmp_glm$std.error,
    sqrt(rowSums(tcrossprod(J_glm, vcov(mod_glm)) * J_glm))
)

# Package-owned lm/glm matrices reproduce predict.lm() for factors and
# data-dependent bases on new data.
mod_basis_lm <- lm(mpg ~ factor(cyl) + splines::ns(hp, 3), data = mtcars)
newdata_basis <- mtcars[1:8, , drop = FALSE]
newdata_basis$hp <- newdata_basis$hp + 5
X_basis_lm <- marginaleffects:::get_model_matrix(mod_basis_lm, newdata_basis)
expect_equivalent(
    drop(X_basis_lm %*% coef(mod_basis_lm)),
    as.numeric(predict(mod_basis_lm, newdata_basis))
)

mod_basis_glm <- suppressWarnings(glm(
    am ~ factor(cyl) + splines::ns(hp, 3),
    family = binomial,
    data = mtcars
))
X_basis_glm <- marginaleffects:::get_model_matrix(mod_basis_glm, newdata_basis)
expect_equivalent(
    drop(X_basis_glm %*% coef(mod_basis_glm)),
    as.numeric(predict(mod_basis_glm, newdata_basis, type = "link"))
)

# Response-scale GLMs apply the inverse-link derivative to each model-matrix
# row before applying comparison and aggregation operators.
cmp_glm_response <- comparisons(
    mod_glm,
    variables = list(hp = c(100, 110)),
    newdata = "mean",
    type = "response"
)
lo <- data.frame(hp = 100, wt = mean(mtcars$wt))
hi <- data.frame(hp = 110, wt = mean(mtcars$wt))
X_lo <- marginaleffects:::get_model_matrix(mod_glm, lo)
X_hi <- marginaleffects:::get_model_matrix(mod_glm, hi)
eta_lo <- drop(X_lo %*% coef(mod_glm))
eta_hi <- drop(X_hi %*% coef(mod_glm))
expected_response <-
    X_hi * as.vector(mod_glm$family$mu.eta(eta_hi)) -
    X_lo * as.vector(mod_glm$family$mu.eta(eta_lo))
attributes(expected_response) <- list(
    dim = dim(expected_response),
    dimnames = list(NULL, names(coef(mod_glm)))
)
expect_equivalent(
    components(cmp_glm_response, "jacobian"),
    expected_response
)

pred_glm_response <- predictions(
    mod_basis_glm,
    newdata = newdata_basis,
    type = "response"
)
eta <- drop(X_basis_glm %*% coef(mod_basis_glm))
expected_pred_response <-
    X_basis_glm * as.vector(mod_basis_glm$family$mu.eta(eta))
attributes(expected_pred_response) <- list(
    dim = dim(expected_pred_response),
    dimnames = list(NULL, names(coef(mod_basis_glm)))
)
expect_equivalent(
    components(pred_glm_response, "jacobian"),
    expected_pred_response
)

# Named covariance matrices are aligned to the analytic Jacobian by coefficient.
X <- model.matrix(mod_lm, data = mtcars[1:5, , drop = FALSE])
X <- matrix(as.numeric(X), nrow = nrow(X),
    dimnames = list(NULL, colnames(X)))
V <- vcov(mod_lm)
V_reversed <- V[rev(rownames(V)), rev(colnames(V)), drop = FALSE]
propagated <- marginaleffects:::std_error_from_jacobian(X, V_reversed, mod_lm)
expected_se <- sqrt(rowSums(tcrossprod(X, V) * X))
expect_equivalent(propagated$std.error, expected_se)

cmp_reversed <- comparisons(
    mod_lm,
    variables = list(hp = c(100, 110)),
    newdata = "mean",
    vcov = V_reversed
)
expect_equivalent(cmp_reversed$std.error, cmp_lm$std.error)

# Linear matrix hypotheses transform every Jacobian column in one operation.
cmp_hypothesis <- comparisons(
    mod_lm,
    variables = list(hp = c(100, 110), wt = c(2, 3)),
    newdata = "mean",
    hypothesis = matrix(c(1, -1), ncol = 1)
)
expect_identical(
    components(cmp_hypothesis, "jacobian"),
    matrix(c(0, 10, -1), nrow = 1,
        dimnames = list(NULL, names(coef(mod_lm))))
)

# Average differences operate on the complete Jacobian matrix. The interaction
# makes the derivative vary by row, so these checks exercise the actual means.
mod_interaction <- lm(mpg ~ hp * wt, data = mtcars)
avg <- avg_comparisons(
    mod_interaction,
    variables = list(hp = c(100, 110))
)
expect_equivalent(
    components(avg, "jacobian"),
    matrix(c(0, 10, 0, 10 * mean(mtcars$wt)), nrow = 1,
        dimnames = list(NULL, names(coef(mod_interaction))))
)

avg_weighted <- avg_comparisons(
    mod_interaction,
    variables = list(hp = c(100, 110)),
    wts = "cyl"
)
expect_equivalent(
    components(avg_weighted, "jacobian"),
    matrix(c(0, 10, 0, 10 * weighted.mean(mtcars$wt, mtcars$cyl)), nrow = 1,
        dimnames = list(NULL, names(coef(mod_interaction))))
)

avg_by <- avg_comparisons(
    mod_interaction,
    variables = list(hp = c(100, 110)),
    by = "am"
)
expected_by <- vapply(split(mtcars$wt, mtcars$am), mean, numeric(1))
expected_by <- cbind(0, 10, 0, 10 * expected_by)
colnames(expected_by) <- names(coef(mod_interaction))
rownames(expected_by) <- NULL
expect_equivalent(components(avg_by, "jacobian"), expected_by)

# Average comparisons bypass the observation-level comparison Jacobian. This
# guards the allocation-saving path in addition to its numerical result above.
assign("analytic_comparison_group_calls", 0L, envir = .GlobalEnv)
suppressMessages(invisible(utils::capture.output(
    trace(
        "jacobian_analytic_comparison_groups",
        where = asNamespace("marginaleffects"),
        tracer = quote({
            .GlobalEnv$analytic_comparison_group_calls <-
                .GlobalEnv$analytic_comparison_group_calls + 1L
        }),
        print = FALSE
    )
)))
tryCatch(
    {
        avg_comparisons(
            mod_interaction,
            variables = list(hp = c(100, 110)),
            by = "am",
            wts = "cyl"
        )
        expect_equal(.GlobalEnv$analytic_comparison_group_calls, 0L)
    },
    finally = {
        suppressMessages(invisible(utils::capture.output(
            untrace(
                "jacobian_analytic_comparison_groups",
                where = asNamespace("marginaleffects")
            )
        )))
        rm("analytic_comparison_group_calls", envir = .GlobalEnv)
    }
)

avg_hypothesis <- avg_comparisons(
    mod_interaction,
    variables = list(hp = c(100, 110)),
    by = "am",
    hypothesis = matrix(c(1, -1), ncol = 1)
)
expect_equivalent(
    components(avg_hypothesis, "jacobian"),
    matrix(expected_by[1, ] - expected_by[2, ], nrow = 1,
        dimnames = list(NULL, names(coef(mod_interaction))))
)

avg_formula_hypothesis <- avg_comparisons(
    mod_interaction,
    variables = list(hp = c(100, 110)),
    by = "am",
    hypothesis = ~ sequential
)
expect_equivalent(
    components(avg_formula_hypothesis, "jacobian"),
    matrix(expected_by[2, ] - expected_by[1, ], nrow = 1,
        dimnames = list(NULL, names(coef(mod_interaction))))
)

# Linear predictions have J = X before applying the same aggregation and
# hypothesis operators used for comparisons.
newdata <- mtcars[1:4, , drop = FALSE]
pred_lm <- predictions(mod_lm, newdata = newdata)
X_lm <- model.matrix(mod_lm, data = newdata)
attributes(X_lm) <- list(
    dim = dim(X_lm),
    dimnames = list(NULL, colnames(X_lm))
)
expect_identical(components(pred_lm, "jacobian"), X_lm)

# The prediction plan may reuse values only when get_predict() confirms that
# those values came from the cached model matrix.
cached_newdata <- components(pred_lm, "newdata")
cached_prediction <- marginaleffects:::get_predict(
    mod_lm,
    newdata = cached_newdata,
    type = "response"
)
expect_true(isTRUE(attr(
    cached_prediction,
    "marginaleffects_model_matrix_used"
)))
expect_equivalent(
    attr(cached_prediction, "marginaleffects_linear_predictor"),
    cached_prediction$estimate
)

pred_hypothesis <- predictions(
    mod_lm,
    newdata = newdata,
    hypothesis = matrix(c(1, -1, 0, 0), ncol = 1)
)
expect_identical(
    components(pred_hypothesis, "jacobian"),
    matrix(X_lm[1, ] - X_lm[2, ], nrow = 1,
        dimnames = list(NULL, names(coef(mod_lm))))
)

avg_pred_weighted <- avg_predictions(mod_lm, by = "am", wts = "cyl")
X_all <- model.matrix(mod_lm, data = mtcars)
expected_pred_weighted <- vapply(split(seq_len(nrow(mtcars)), mtcars$am), function(i) {
    colSums(X_all[i, , drop = FALSE] * mtcars$cyl[i]) / sum(mtcars$cyl[i])
}, numeric(ncol(X_all)))
expected_pred_weighted <- t(expected_pred_weighted)
rownames(expected_pred_weighted) <- NULL
expect_equivalent(
    components(avg_pred_weighted, "jacobian"),
    expected_pred_weighted
)

pred_glm <- predictions(mod_glm, newdata = newdata, type = "link")
X_glm <- model.matrix(mod_glm, data = newdata)
attributes(X_glm) <- list(
    dim = dim(X_glm),
    dimnames = list(NULL, colnames(X_glm))
)
expect_identical(components(pred_glm, "jacobian"), X_glm)

pred_invlink <- predictions(
    mod_glm,
    newdata = newdata,
    type = "invlink(link)"
)
expect_identical(components(pred_invlink, "jacobian"), X_glm)

# Response-scale derivatives must include mu.eta() at every prediction row.
# Compare against the public numerical fallback across datasets, families,
# links, predictions, and simple differences.
dat_mtcars <- mtcars
dat_mtcars$cyl_f <- factor(dat_mtcars$cyl)
dat_iris <- iris
dat_iris$high_width <- as.integer(
    dat_iris$Sepal.Width > stats::median(dat_iris$Sepal.Width)
)

response_cases <- suppressWarnings(list(
    mtcars_binomial_logit = list(
        model = glm(am ~ hp + wt + cyl_f, dat_mtcars, family = binomial("logit")),
        data = dat_mtcars,
        variable = "hp"
    ),
    mtcars_binomial_probit = list(
        model = glm(am ~ hp + wt + cyl_f, dat_mtcars, family = binomial("probit")),
        data = dat_mtcars,
        variable = "hp"
    ),
    mtcars_binomial_cloglog = list(
        model = glm(am ~ hp + wt + cyl_f, dat_mtcars, family = binomial("cloglog")),
        data = dat_mtcars,
        variable = "hp"
    ),
    mtcars_binomial_cauchit = list(
        model = glm(am ~ mpg + drat, dat_mtcars, family = binomial("cauchit")),
        data = dat_mtcars,
        variable = "mpg"
    ),
    mtcars_gaussian_log = list(
        model = glm(mpg ~ hp + wt + cyl_f, dat_mtcars, family = gaussian("log")),
        data = dat_mtcars,
        variable = "hp"
    ),
    mtcars_gaussian_inverse = list(
        model = glm(mpg ~ hp + wt + cyl_f, dat_mtcars, family = gaussian("inverse")),
        data = dat_mtcars,
        variable = "hp"
    ),
    mtcars_poisson_log = list(
        model = glm(cyl ~ hp + wt, dat_mtcars, family = poisson("log")),
        data = dat_mtcars,
        variable = "hp"
    ),
    mtcars_poisson_sqrt = list(
        model = glm(cyl ~ hp + wt, dat_mtcars, family = poisson("sqrt")),
        data = dat_mtcars,
        variable = "hp"
    ),
    mtcars_gamma_log = list(
        model = glm(mpg ~ hp + wt, dat_mtcars, family = Gamma("log")),
        data = dat_mtcars,
        variable = "hp"
    ),
    mtcars_gamma_inverse = list(
        model = glm(mpg ~ hp + wt, dat_mtcars, family = Gamma("inverse")),
        data = dat_mtcars,
        variable = "hp"
    )
))
for (link in c("logit", "probit", "cloglog", "cauchit")) {
    response_cases[[paste0("iris_binomial_", link)]] <- list(
        model = glm(
            high_width ~ Sepal.Length + Petal.Length + Species,
            dat_iris,
            family = binomial(link)
        ),
        data = dat_iris,
        variable = "Sepal.Length"
    )
}

force_jacobian_fallback <- function(fun) {
    old_option <- options(marginaleffects_analytic_jacobian = FALSE)
    on.exit(options(old_option), add = TRUE)
    fun()
}

for (case in response_cases) {
    nd <- case$data[seq_len(min(25L, nrow(case$data))), , drop = FALSE]
    pred_analytic <- predictions(case$model, newdata = nd, type = "response")
    pred_fallback <- force_jacobian_fallback(function() {
        predictions(case$model, newdata = nd, type = "response")
    })
    expect_equivalent(pred_analytic$estimate, pred_fallback$estimate)
    expect_equivalent(
        components(pred_analytic, "jacobian"),
        components(pred_fallback, "jacobian"),
        tolerance = 1e-4
    )

    cmp_analytic <- comparisons(
        case$model,
        variables = case$variable,
        newdata = nd,
        type = "response"
    )
    cmp_fallback <- force_jacobian_fallback(function() {
        comparisons(
            case$model,
            variables = case$variable,
            newdata = nd,
            type = "response"
        )
    })
    expect_equivalent(cmp_analytic$estimate, cmp_fallback$estimate)
    expect_equivalent(
        components(cmp_analytic, "jacobian"),
        components(cmp_fallback, "jacobian"),
        tolerance = 1e-4
    )
}

# Exercise response-scale derivatives after weighted grouping and a matrix
# hypothesis, where the row-level mu.eta() values must be aggregated first.
mod_iris_probit <- response_cases$iris_binomial_probit$model
hypothesis_iris <- matrix(c(1, -1, 0), ncol = 1)
avg_analytic <- avg_comparisons(
    mod_iris_probit,
    variables = "Sepal.Length",
    by = "Species",
    wts = "Sepal.Width",
    hypothesis = hypothesis_iris,
    type = "response"
)
avg_fallback <- force_jacobian_fallback(function() {
    avg_comparisons(
        mod_iris_probit,
        variables = "Sepal.Length",
        by = "Species",
        wts = "Sepal.Width",
        hypothesis = hypothesis_iris,
        type = "response"
    )
})
expect_equivalent(avg_analytic$estimate, avg_fallback$estimate)
expect_equivalent(
    components(avg_analytic, "jacobian"),
    components(avg_fallback, "jacobian"),
    tolerance = 1e-4
)

# Unsupported estimands use finite differences. Instrumentation is always
# removed in `finally` so a failed assertion cannot contaminate later tests.
assign("analytic_fd_calls", 0L, envir = .GlobalEnv)
suppressMessages(invisible(utils::capture.output(
    trace(
        "get_jacobian_fdforward",
        where = asNamespace("marginaleffects"),
        tracer = quote({
            .GlobalEnv$analytic_fd_calls <- .GlobalEnv$analytic_fd_calls + 1L
        }),
        print = FALSE
    )
)))
tryCatch(
    {
        comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            newdata = "mean",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 0L)

        predictions(
            mod_lm,
            newdata = mtcars[1:5, , drop = FALSE],
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 0L)

        comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            newdata = "mean",
            comparison = "ratio",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 0L)

        avg_comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 0L)

        comparisons(
            mod_lm,
            variables = c("hp", "wt"),
            newdata = "mean",
            hypothesis = c(1, -1),
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 0L)

        comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            newdata = "mean",
            comparison = function(hi, lo) hi - lo,
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 1L)

        mod_offset <- lm(mpg ~ hp + offset(wt), data = mtcars)
        comparisons(
            mod_offset,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 2L)

        mod_rank_deficient <- lm(mpg ~ hp + I(hp), data = mtcars)
        suppressWarnings(comparisons(
            mod_rank_deficient,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        ))
        expect_equal(.GlobalEnv$analytic_fd_calls, 3L)

        mod_subclass <- mod_lm
        class(mod_subclass) <- c("analytic_audit_lm", class(mod_subclass))
        comparisons(
            mod_subclass,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 4L)

        comparisons(
            mod_glm,
            variables = "hp",
            newdata = "mean",
            type = "response",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 4L)

        # The global opt-out must actually invoke the fallback, not merely
        # produce a numerically identical result through the analytic route.
        old_option <- options(marginaleffects_analytic_jacobian = FALSE)
        tryCatch(
            comparisons(
                mod_lm,
                variables = "hp",
                newdata = "mean",
                numderiv = "fdforward"
            ),
            finally = options(old_option)
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 5L)

        mod_glm_offset <- glm(
            am ~ hp + offset(log(wt)),
            data = mtcars,
            family = binomial()
        )
        comparisons(
            mod_glm_offset,
            variables = "hp",
            newdata = "mean",
            type = "response",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 6L)

        mod_glm_rank_deficient <- glm(
            am ~ hp + I(hp),
            data = mtcars,
            family = binomial()
        )
        suppressWarnings(comparisons(
            mod_glm_rank_deficient,
            variables = "hp",
            newdata = "mean",
            type = "response",
            numderiv = "fdforward"
        ))
        expect_equal(.GlobalEnv$analytic_fd_calls, 7L)
    },
    finally = {
        suppressMessages(invisible(utils::capture.output(
            untrace(
                "get_jacobian_fdforward",
                where = asNamespace("marginaleffects")
            )
        )))
        rm("analytic_fd_calls", envir = .GlobalEnv)
    }
)


# Slopes and nonlinear contrasts have closed-form Jacobians. Each estimand is
# checked against the finite-difference fallback, which is the reference these
# derivatives replace. The tolerance reflects the accuracy of that reference,
# not of the analytic path.
mod_key_lm <- lm(mpg ~ hp + wt + factor(am), data = mtcars)
mod_key_glm <- glm(vs ~ hp + wt, family = binomial, data = mtcars)
mod_key_pois <- glm(carb ~ hp + wt, family = poisson, data = mtcars)

expect_analytic_matches_fd <- function(call, tolerance = 1e-3) {
    call <- substitute(call)
    analytic <- eval(call, parent.frame())
    old <- options(marginaleffects_analytic_jacobian = FALSE)
    numeric <- tryCatch(eval(call, parent.frame()), finally = options(old))
    expect_equivalent(analytic$std.error, numeric$std.error, tolerance = tolerance)
    expect_true(all(is.finite(analytic$std.error)))
    invisible(NULL)
}

expect_analytic_matches_fd(slopes(mod_key_lm))
expect_analytic_matches_fd(avg_slopes(mod_key_lm))
expect_analytic_matches_fd(avg_slopes(mod_key_lm, by = "am"))
expect_analytic_matches_fd(comparisons(mod_key_lm, comparison = "ratio"))
expect_analytic_matches_fd(comparisons(mod_key_lm, comparison = "lnratio"))
expect_analytic_matches_fd(comparisons(mod_key_lm, comparison = "lift"))
expect_analytic_matches_fd(avg_comparisons(mod_key_lm, comparison = "ratio"))
expect_analytic_matches_fd(avg_comparisons(mod_key_lm, comparison = "lnratio"))
expect_analytic_matches_fd(avg_comparisons(mod_key_lm, comparison = "lift"))

expect_analytic_matches_fd(slopes(mod_key_glm, type = "response"))
expect_analytic_matches_fd(avg_slopes(mod_key_glm, type = "response"))
expect_analytic_matches_fd(avg_slopes(mod_key_glm, type = "link"))
expect_analytic_matches_fd(
    comparisons(mod_key_glm, comparison = "lnor", type = "response")
)
expect_analytic_matches_fd(
    avg_comparisons(mod_key_glm, comparison = "lnor", type = "response")
)
expect_analytic_matches_fd(
    avg_comparisons(mod_key_glm, comparison = "ratio", type = "response")
)

# Unit-level Poisson slopes are the case where finite differences are least
# accurate, so they are pinned to the exact derivative below instead.
expect_analytic_matches_fd(slopes(mod_key_pois, type = "response"), tolerance = 1e-2)
expect_analytic_matches_fd(avg_slopes(mod_key_pois, type = "response"))

X_pois <- model.matrix(mod_key_pois)
beta_pois <- coef(mod_key_pois)
mu_pois <- exp(drop(X_pois %*% beta_pois))
J_pois <- mu_pois * X_pois * beta_pois[["hp"]]
J_pois[, "hp"] <- J_pois[, "hp"] + mu_pois
expect_equivalent(
    slopes(mod_key_pois, type = "response", variables = "hp")$std.error,
    sqrt(rowSums(tcrossprod(J_pois, vcov(mod_key_pois)) * J_pois)),
    tolerance = 1e-6
)

# The average marginal effect of a logit has a closed form that does not go
# through the package, so it pins down the analytic path independently.
X_ame <- model.matrix(mod_key_glm)
beta_ame <- coef(mod_key_glm)
mu_ame <- mod_key_glm$family$linkinv(drop(X_ame %*% beta_ame))
d1_ame <- mu_ame * (1 - mu_ame)
d2_ame <- d1_ame * (1 - 2 * mu_ame)
J_ame <- t(sapply(2:3, function(k) {
    colMeans(d2_ame * X_ame) * beta_ame[[k]] +
        (seq_along(beta_ame) == k) * mean(d1_ame)
}))
expect_equivalent(
    avg_slopes(mod_key_glm, type = "response")$std.error,
    sqrt(rowSums(tcrossprod(J_ame, vcov(mod_key_glm)) * J_ame)),
    tolerance = 1e-6
)

# Weighted averages divide by the recorded weights, not by the row count.
dat_wts <- transform(mtcars, wcol = seq(1, 2, length.out = nrow(mtcars)))
mod_wts <- lm(mpg ~ hp + wt, data = dat_wts)
expect_analytic_matches_fd(avg_slopes(mod_wts, wts = "wcol", newdata = dat_wts))
expect_analytic_matches_fd(
    avg_comparisons(mod_wts, comparison = "ratio", wts = "wcol", newdata = dat_wts)
)

# A `by` data frame records aggregation in blocks rather than in the comparison
# groups, so slopes reach the averaging fast path through a different branch.
dat_byframe <- transform(mtcars, cyl = as.factor(cyl))
mod_byframe <- lm(mpg ~ hp + wt + cyl, data = dat_byframe)
by_frame <- data.frame(
    cyl = factor(c(4, 6, 8)),
    by = c("small", "small", "big")
)
expect_analytic_matches_fd(
    comparisons(mod_byframe, variables = "cyl", by = by_frame)
)
expect_analytic_matches_fd(
    slopes(mod_byframe, variables = "hp", by = by_frame)
)

# Elasticities depend on the fitted response, so they keep using the fallback.
expect_equal(
    marginaleffects:::get_jacobian_analytic(
        mod_key_lm,
        plan = NULL,
        kind = "comparisons",
        type = "response",
        estimate = numeric(0)
    ),
    NULL
)
