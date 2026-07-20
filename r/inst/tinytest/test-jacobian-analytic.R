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
        expect_equal(.GlobalEnv$analytic_fd_calls, 1L)

        comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            newdata = "mean",
            comparison = "ratio",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 2L)

        avg_comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 3L)

        comparisons(
            mod_lm,
            variables = c("hp", "wt"),
            newdata = "mean",
            hypothesis = c(1, -1),
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 4L)

        comparisons(
            mod_lm,
            variables = list(hp = c(100, 110)),
            newdata = "mean",
            comparison = function(hi, lo) hi - lo,
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 5L)

        mod_offset <- lm(mpg ~ hp + offset(wt), data = mtcars)
        comparisons(
            mod_offset,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 6L)

        mod_rank_deficient <- lm(mpg ~ hp + I(hp), data = mtcars)
        suppressWarnings(comparisons(
            mod_rank_deficient,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        ))
        expect_equal(.GlobalEnv$analytic_fd_calls, 7L)

        mod_subclass <- mod_lm
        class(mod_subclass) <- c("analytic_audit_lm", class(mod_subclass))
        comparisons(
            mod_subclass,
            variables = "hp",
            newdata = "mean",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 8L)

        comparisons(
            mod_glm,
            variables = "hp",
            newdata = "mean",
            type = "response",
            numderiv = "fdforward"
        )
        expect_equal(.GlobalEnv$analytic_fd_calls, 9L)
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
