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
        expect_equal(.GlobalEnv$analytic_fd_calls, 1L)

        avg_comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 1L)

        comparisons(
            mod_lm,
            variables = c("hp", "wt"),
            newdata = "mean",
            hypothesis = c(1, -1),
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 1L)

        comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            newdata = "mean",
            comparison = function(hi, lo) hi - lo,
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 2L)

        mod_offset <- lm(mpg ~ hp + offset(wt), data = mtcars)
        comparisons(
            mod_offset,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 3L)

        mod_rank_deficient <- lm(mpg ~ hp + I(hp), data = mtcars)
        suppressWarnings(comparisons(
            mod_rank_deficient,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        ))
        expect_equal(.GlobalEnv$analytic_fd_calls, 4L)

        mod_subclass <- mod_lm
        class(mod_subclass) <- c("analytic_audit_lm", class(mod_subclass))
        comparisons(
            mod_subclass,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 5L)

        comparisons(
            mod_glm,
            variables = "hp",
            newdata = "mean",
            type = "response",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 6L)
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
