source("helpers.R")
using("marginaleffects")

mod <- lm(mpg ~ hp * wt + factor(cyl), data = mtcars)

assign("model_matrix_count", 0L, envir = .GlobalEnv)
suppressMessages(invisible(utils::capture.output(
    trace(
        "get_model_matrix.default",
        where = asNamespace("marginaleffects"),
        tracer = quote({
            .GlobalEnv$model_matrix_count <- .GlobalEnv$model_matrix_count + 1L
        }),
        print = FALSE
    )
)))
tryCatch(
    {
        comparisons(mod, variables = "hp", vcov = FALSE)
        expect_equal(.GlobalEnv$model_matrix_count, 0L)

        .GlobalEnv$model_matrix_count <- 0L
        slopes(mod, variables = "hp", slope = "eyex", vcov = FALSE)
        expect_equal(.GlobalEnv$model_matrix_count, 0L)
    },
    finally = {
        suppressMessages(invisible(utils::capture.output(
            untrace("get_model_matrix.default", where = asNamespace("marginaleffects"))
        )))
        rm("model_matrix_count", envir = .GlobalEnv)
    }
)

cmp <- avg_comparisons(
    mod,
    variables = "hp",
    comparison = function(hi, lo) mean(hi - lo)
)
expect_equal(nrow(cmp), 1)
expect_true("std.error" %in% colnames(cmp))
expect_false(anyNA(cmp$std.error))

cmp <- comparisons(
    mod,
    variables = "hp",
    comparison = function(hi, lo, x) (hi - lo) * x
)
expect_equal(nrow(cmp), nrow(mtcars))
expect_true("std.error" %in% colnames(cmp))

expect_error(
    comparisons(mod, byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)
expect_error(
    avg_comparisons(mod, byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)
expect_error(
    slopes(mod, byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)
expect_error(
    avg_slopes(mod, byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)

cmp_string <- comparisons(
    mod,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = "b1 = b2"
)
cmp_matrix <- comparisons(
    mod,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = c(1, -1)
)
expect_equivalent(cmp_string$estimate, cmp_matrix$estimate)
expect_equivalent(
    cmp_string$std.error,
    cmp_matrix$std.error,
    tolerance = 1e-6
)

cmp_string <- comparisons(
    mod,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = "plogis(b1) - plogis(b2) = 0"
)
expect_equal(nrow(cmp_string), 1)
expect_true("std.error" %in% colnames(cmp_string))

cmp_formula <- comparisons(
    mod,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = ~pairwise
)
expect_equal(nrow(cmp_formula), 1)
expect_true("std.error" %in% colnames(cmp_formula))

ok <- try(
    marginaleffects:::validate_plan_replay(
        "comparison",
        c(1, 2, 3),
        c(1, 2 + 1e-10, 3)
    ),
    silent = TRUE
)
expect_false(inherits(ok, "try-error"))

expect_error(
    marginaleffects:::validate_plan_replay(
        "comparison",
        c(1, 2, 3),
        c(1, 2.01, 3)
    ),
    pattern = "comparison plan baseline check failed"
)

model_error <- lm(mpg ~ hp, data = mtcars)
lo_error <- data.frame(hp = c(100, 120))
hi_error <- data.frame(wt = c(3, 4))
expect_error(
    marginaleffects:::predictions_hi_lo_frequentist(
        model_error,
        lo = lo_error,
        hi = hi_error,
        type = "response"
    ),
    pattern = "object 'hp' not found"
)

expect_error(
    comparisons(
        mod,
        variables = "hp",
        comparison = function(hi, lo) rep(NA_real_, length(hi))
    ),
    pattern = "no missing value"
)

bad_replay_plan <- list(
    kind = "comparisons",
    n_pred = 2L,
    need_y = FALSE,
    na_keep = NULL,
    perm = NULL,
    groups = list(list(
        idx = 1:2,
        out_idx = 1:2,
        scalar = FALSE,
        uses_y = FALSE,
        fun = function(hi, lo) hi[[1]] - lo[[1]],
        args = list()
    )),
    n_comp = 2L,
    est_keep = NULL,
    agg = NULL,
    hyp = NULL
)
expect_error(
    marginaleffects:::comparison_plan_apply(
        bad_replay_plan,
        hi = c(2, 4),
        lo = c(1, 1)
    ),
    pattern = "changed shape"
)

out_bayes <- data.table::data.table(
    term = rep("hp", 4),
    group = rep(c("a", "b"), each = 2),
    estimate = NA_real_,
    marginaleffects_wts_internal = NA_real_,
    tmp_idx = rep(1:2, 2)
)
draws_lo <- matrix(seq_len(12), nrow = 4)
draws_hi <- draws_lo + 1
variables_bayes <- list(
    hp = list(
        "function" = function(hi, lo) mean(hi - lo),
        fun_key = "custom",
        eps = 1
    )
)
context_bayes <- list(
    cross = FALSE,
    newdata = data.frame(hp = seq_len(4)),
    variables = variables_bayes,
    fun_list = list(hp = variables_bayes$hp[["function"]]),
    elasticities = list(hp = out_bayes$tmp_idx)
)
scalar_bayes <- marginaleffects:::compare_hi_lo_bayesian(
    out = out_bayes,
    draws = draws_lo,
    draws_hi = draws_hi,
    draws_lo = draws_lo,
    draws_or = draws_lo,
    by = "group",
    context = context_bayes
)
marginaleffects:::settings_rm("marginaleffects_safefun_return1")
expect_equal(dim(scalar_bayes$draws), c(2, 3))
expect_equivalent(scalar_bayes$draws, matrix(1, nrow = 2, ncol = 3))
expect_equivalent(scalar_bayes$out$estimate, c(1, 1))


fdcenter_calls <- 0L
fdcenter_func <- function(x) {
    fdcenter_calls <<- fdcenter_calls + 1L
    c(sum(x), prod(x))
}
fdcenter_x <- c(1, 2, 3)
fdcenter_J <- marginaleffects:::get_jacobian_fdcenter(
    fdcenter_func,
    fdcenter_x,
    eps = 1e-5
)
expect_equal(fdcenter_calls, 2L * length(fdcenter_x))
expect_equal(dim(fdcenter_J), c(2L, 3L))
