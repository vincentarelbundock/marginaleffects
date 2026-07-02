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
scalar_result <- marginaleffects:::compare_hi_lo_bayesian_scalar(
    out = out,
    draws = draws,
    draws_hi = draws_hi,
    draws_lo = draws_lo,
    draws_or = draws_or,
    by = "g",
    cross = FALSE,
    variables = list(x = list(eps = 1)),
    fun_list = list(x = marginaleffects:::comparison_function_dict[["differenceavg"]]),
    elasticities = list(x = seq_len(nrow(out))),
    newdata = data.frame(),
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
    cross = FALSE,
    variables = list(x = list(eps = 1)),
    fun_list = list(x = marginaleffects:::comparison_function_dict[["differenceavg"]]),
    elasticities = list(x = seq_len(nrow(out))),
    newdata = data.frame()
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
    cross = FALSE,
    variables = list(x = list(eps = 1)),
    fun_list = list(x = marginaleffects:::comparison_function_dict[["difference"]]),
    elasticities = list(x = seq_len(nrow(out))),
    newdata = data.frame(),
    group_indices = group_indices
)
expect_null(vector_result)
