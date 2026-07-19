source("helpers.R")
using("marginaleffects")

# Fast posterior centers preserve base missing-value behavior and custom functions.
draws <- matrix(c(1, 2, 3, 4, NA, 6, 7, 8), nrow = 2)
expect_equivalent(
    marginaleffects:::posterior_draws_center(draws, stats::median),
    apply(draws, 1, stats::median)
)
expect_equivalent(
    marginaleffects:::posterior_draws_center(draws, base::mean),
    apply(draws, 1, base::mean)
)
center_fun <- function(x) max(x, na.rm = TRUE)
expect_equivalent(
    marginaleffects:::posterior_draws_center(draws, center_fun),
    apply(draws, 1, center_fun)
)

# Pairwise shortcuts preserve the previous Cartesian-grid order without building k^2 rows.
levs <- letters[1:5]
variable <- list(value = "pairwise")
actual <- marginaleffects:::contrast_categories_shortcuts(levs, variable)
expected <- data.table::CJ(lo = levs, hi = levs, sorted = FALSE)
expected <- expected[expected$hi != expected$lo]
expected <- expected[match(expected$lo, levs) < match(expected$hi, levs)]
expect_identical(actual, expected)

variable$value <- "revpairwise"
actual <- marginaleffects:::contrast_categories_shortcuts(levs, variable)
expect_identical(actual, expected[, .(lo = hi, hi = lo)])

# Finite-difference implementations preserve values and evaluation counts.
x <- c(1, 2, 3, 4)
calls <- 0L
fun <- function(x) {
    calls <<- calls + 1L
    c(sum(x), sum(x^2))
}
J <- marginaleffects:::get_jacobian_fdforward(fun, x, eps = 1e-5)
expect_equal(calls, length(x) + 1L)
expect_equal(dim(J), c(2L, length(x)))
expect_equivalent(J[1, ], rep(1, length(x)), tolerance = 1e-8)

calls <- 0L
J <- marginaleffects:::get_jacobian_fdcenter(fun, x, eps = 1e-5)
expect_equal(calls, 2L * length(x))
expect_equal(dim(J), c(2L, length(x)))
expect_equivalent(J[1, ], rep(1, length(x)), tolerance = 1e-8)

# Mixed hypothesis directions evaluate the same row-specific reference distributions.
ci_data <- data.frame(
    estimate = c(-1, 0.5, 2, -0.25, 1.5, 0),
    std.error = c(0.5, 1, 0.75, 0.25, 2, 1),
    df = c(5, 8, 12, 20, 30, 50)
)
direction <- c("=", "<=", ">=", "=", "<=", ">=")
statistic <- ci_data$estimate / ci_data$std.error
expected_p <- numeric(nrow(ci_data))
idx <- direction == "="
expected_p[idx] <- 2 * stats::pt(-abs(statistic[idx]), df = ci_data$df[idx])
idx <- direction == "<="
expected_p[idx] <- 1 - stats::pt(statistic[idx], df = ci_data$df[idx])
idx <- direction == ">="
expected_p[idx] <- stats::pt(statistic[idx], df = ci_data$df[idx])
actual <- marginaleffects:::get_ci_internal(
    ci_data,
    conf_level = 0.95,
    df = ci_data$df,
    draws = NULL,
    hypothesis_null = 0,
    hypothesis_direction = direction,
    model = NULL
)
expect_equivalent(actual$p.value, expected_p)

# Vectorized label construction matches the previous row-wise formatting.
label_data <- data.frame(
    term = factor(c("a", "b", "c")),
    contrast = c("x - y", NA, "z - y"),
    group = c(1, 2, 3)
)
label_columns <- label_data[, c("term", "contrast", "group")]
expected_labels <- apply(label_columns, 1, paste, collapse = " ")
expect_identical(marginaleffects:::get_labels(label_data), expected_labels)
expected_index <- apply(label_data[, c("term", "contrast")], 1, toString)
expect_identical(marginaleffects:::get_unique_index(label_data), expected_index)

# Positional hypotheses compile one expression and bind only referenced estimates.
compiled <- marginaleffects:::hypothesis_string_compile_expression(
    "b1 + b10 + exp(b100000)",
    n_estimates = 100000L,
    positional = TRUE
)
expect_identical(compiled$idx, c(1L, 10L, 100000L))
estimates <- numeric(100000L)
estimates[c(1L, 10L, 100000L)] <- c(2, 3, 0)
expect_equal(
    marginaleffects:::hypothesis_string_eval_compiled(estimates, compiled),
    6
)

# Dynamic lookup retains the previous behavior by binding the complete environment.
compiled_dynamic <- marginaleffects:::hypothesis_string_compile_expression(
    "get('b10')",
    n_estimates = 10L,
    positional = TRUE
)
expect_identical(compiled_dynamic$idx, seq_len(10L))
expect_equal(
    marginaleffects:::hypothesis_string_eval_compiled(seq_len(10L), compiled_dynamic),
    10
)
compiled_dynamic <- marginaleffects:::hypothesis_string_compile_expression(
    "do.call('get', list('b10'))",
    n_estimates = 10L,
    positional = TRUE
)
expect_identical(compiled_dynamic$idx, seq_len(10L))
expect_equal(
    marginaleffects:::hypothesis_string_eval_compiled(seq_len(10L), compiled_dynamic),
    10
)

# Aligned unit-level keys use positional attachment without changing join semantics.
merge_out <- data.table::data.table(rowid = 1:4, estimate = c(0.1, 0.2, 0.3, 0.4))
merge_original <- data.table::data.table(
    rowid = 1:4,
    x = c("a", "b", "c", "d"),
    z = factor(c("u", "v", "u", "v"))
)
actual <- marginaleffects:::merge_original_data(merge_out, merge_original)
expected <- merge(merge_out, merge_original, all.x = TRUE, by = "rowid", sort = FALSE)
expect_equivalent(actual, expected)

# Shuffled and duplicated keys retain the general merge behavior.
merge_original_shuffled <- merge_original[c(4, 2, 1, 3)]
actual <- marginaleffects:::merge_original_data(merge_out, merge_original_shuffled)
expected <- merge(merge_out, merge_original_shuffled, all.x = TRUE, by = "rowid", sort = FALSE)
expect_equivalent(actual, expected)
