source("helpers.R")
using("marginaleffects")

# classic input

expect_equivalent(
    marginaleffects:::is_binary(1:10),
    FALSE
)

expect_equivalent(
    marginaleffects:::is_binary(0:1),
    TRUE
)

expect_equivalent(
    marginaleffects:::is_binary(c(0, 0.5, 1)),
    FALSE
)

# with single values

expect_equivalent(
    marginaleffects:::is_binary(1),
    TRUE
)

expect_equivalent(
    marginaleffects:::is_binary(2),
    FALSE
)

# with missings / NULL

expect_equivalent(
    marginaleffects:::is_binary(c(0, 0.5, NA, 1)),
    FALSE
)

expect_equivalent(
    marginaleffects:::is_binary(NA),
    FALSE
)

expect_equivalent(
    marginaleffects:::is_binary(NULL),
    TRUE
)

idx <- marginaleffects:::comparison_group_indices(c("b", "a", "b", "c", "a"))
expect_equivalent(
    unname(idx),
    list(c(1L, 3L), c(2L, 5L), 4L)
)
expect_equivalent(
    names(idx),
    c("b", "a", "c")
)

out <- data.table::data.table(
    predicted_lo = rep(0, 5),
    term = "x",
    g = c("b", "a", "b", "c", "a"),
    marginaleffects_wts_internal = NA_real_,
    tmp_idx = c(1L, 1L, 2L, 1L, 2L)
)
draws <- draws_lo <- draws_or <- matrix(0, nrow = 5, ncol = 2)
draws_hi <- matrix(
    c(10, 1, 30, 5, 3, 100, 2, 300, 50, 4),
    nrow = 5,
    ncol = 2
)
group_indices <- marginaleffects:::comparison_group_indices(out$g)
context_differenceavg <- list(
    cross = FALSE,
    newdata = data.frame(),
    variables = list(x = list(eps = 1)),
    fun_list = list(x = marginaleffects:::comparison_function_dict[["differenceavg"]]),
    elasticities = list(x = seq_len(nrow(out)))
)
scalar_result <- marginaleffects:::compare_hi_lo_bayesian_scalar(
    out = out,
    draws = draws,
    draws_hi = draws_hi,
    draws_lo = draws_lo,
    draws_or = draws_or,
    by = "g",
    context = context_differenceavg,
    group_indices = group_indices
)
expected_draws <- matrix(c(20, 2, 5, 200, 3, 50), nrow = 3, ncol = 2)
expect_equivalent(scalar_result$out$g, c("b", "a", "c"))
expect_equivalent(unname(scalar_result$draws), expected_draws)

result <- marginaleffects:::compare_hi_lo_bayesian(
    out = out,
    draws = draws,
    draws_hi = draws_hi,
    draws_lo = draws_lo,
    draws_or = draws_or,
    by = "g",
    context = context_differenceavg
)
expect_equivalent(result$out$g, c("b", "a", "c"))
expect_equivalent(unname(result$draws), expected_draws)
expect_equivalent(result$out$estimate, apply(expected_draws, 1, stats::median))

vector_result <- marginaleffects:::compare_hi_lo_bayesian_scalar(
    out = out,
    draws = draws,
    draws_hi = draws_hi,
    draws_lo = draws_lo,
    draws_or = draws_or,
    by = "g",
    context = utils::modifyList(
        context_differenceavg,
        list(fun_list = list(x = marginaleffects:::comparison_function_dict[["difference"]]))
    ),
    group_indices = group_indices
)
expect_null(vector_result)

marginaleffects:::settings_set("marginaleffects_safefun_return1", TRUE)
expect_error(
    marginaleffects:::comparison_plan_build(),
    pattern = "argument \"mfx\" is missing"
)
expect_false(marginaleffects:::settings_equal(
    "marginaleffects_safefun_return1",
    TRUE
))

comparison_result <- marginaleffects:::comparison_call(
    hi = c(3, 5),
    lo = c(1, 2),
    y = c(10, 20),
    n = 2,
    term = c("x", "x"),
    wts = c(1, 2),
    tmp_idx = c(1, 2),
    context = list(
        cross = FALSE,
        newdata = data.frame(a = 1:2),
        variables = list(x = list(eps = 2, fun_key = "custom")),
        fun_list = list(x = function(hi, lo, eps, x) (hi - lo) / eps + x),
        elasticities = list(x = c(10, 20))
    )
)
expect_equivalent(comparison_result$value, c(11, 21.5))
expect_equivalent(names(comparison_result$args), c("eps", "x"))
expect_false(comparison_result$uses_y)
expect_true(is.function(comparison_result$fun))

mod_finalize <- lm(mpg ~ hp, data = mtcars)
mfx_finalize <- marginaleffects:::marginaleffects_init(
    model = mod_finalize,
    calling_function = "predictions",
    newdata = NULL,
    wts = FALSE,
    vcov = TRUE,
    by = FALSE
)
mfx_finalize <- marginaleffects:::add_hypothesis(mfx_finalize, NULL)
mfx_finalize@newdata <- data.frame(rowid = 1:2, hp = c(100, 120))
mfx_finalize@conf_level <- 0.95
mfx_finalize@df <- Inf
finalized <- marginaleffects:::finalize_estimates(
    out = data.table::data.table(
        rowid = 1:2,
        estimate = c(1, 2),
        std.error = c(0.1, 0.2),
        marginaleffects_wts_internal = 1
    ),
    mfx = mfx_finalize,
    by = FALSE,
    transform = NULL,
    equivalence = NULL,
    class_name = "predictions",
    inferences_method = NULL
)
expect_true(inherits(finalized, "predictions"))
expect_true(all(c("conf.low", "conf.high") %in% colnames(finalized)))
expect_false("marginaleffects_wts_internal" %in% colnames(finalized))
