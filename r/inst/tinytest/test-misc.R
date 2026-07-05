source("helpers.R")

# important for modelsummary glance
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am + factor(cyl), tmp)
expect_inherits(components(predictions(mod), "model"), "lm")
expect_inherits(components(comparisons(mod), "model"), "lm")
expect_inherits(components(avg_slopes(mod), "model"), "lm")

# Issue #1089 white space in variable name
tmp <- mtcars
colnames(tmp)[1] <- "Miles per gallon"
mod <- lm(hp ~ wt * `Miles per gallon`, tmp)
s <- avg_slopes(mod) |> suppressWarnings()
expect_inherits(s, "slopes")
expect_equal(nrow(s), 2)
s <- avg_slopes(mod, variables = "Miles per gallon") |> suppressWarnings()
expect_inherits(s, "slopes")
expect_equal(nrow(s), 1)


# scale() returns a 1-column matrix
dat <- transform(mtcars, hp = scale(hp))
mod <- lm(mpg ~ hp, data = dat)
p <- predictions(mod)
expect_inherits(p, "predictions")
expect_false(anyNA(p$estimate))
expect_false(anyNA(p$std.error))


# Issue #6 marginaleffectsJAX: missing model matrix attribute
mod_factor <- lm(mpg ~ hp + factor(cyl), data = mtcars)
p <- predictions(mod_factor, by = "cyl")
M <- attr(components(p, "newdata"), "marginaleffects_model_matrix")
expect_inherits(M, "matrix")


out <- data.frame(rowid = 1:2, estimate = c(0.1, 0.2))
original <- data.frame(rowid = c(1, 1, 2), x = c("a", "b", "c"))
merged <- marginaleffects:::merge_original_data(out, original)
expect_equal(nrow(merged), nrow(out))
expect_false("x" %in% colnames(merged))
merged <- marginaleffects:::merge_original_data(out, original, deduplicate = TRUE)
expect_equal(nrow(merged), nrow(out))
expect_true("x" %in% colnames(merged))

out <- data.frame(rowid = c(1, 1, 2, 2), estimate = c(0.1, 0.2, 0.3, 0.4))
original <- data.frame(rowid = c(1, 1, 2, 2), x = c("a", "b", "c", "d"))
merged <- marginaleffects:::merge_original_data(out, original)
expect_equal(nrow(merged), nrow(out))
expect_equal(merged$x, original$x)

replay_n <- 60
estimates <- data.table::data.table(
    rowid = seq_len(replay_n),
    term = "x",
    g = rep(seq_len(30), each = 2),
    estimate = seq_len(replay_n),
    marginaleffects_wts_internal = seq_len(replay_n) / replay_n
)
estimates$estimate[c(2, 9, 27)] <- NA_real_
newdata <- data.table::data.table(
    rowid = seq_len(replay_n),
    marginaleffects_wts_internal = estimates$marginaleffects_wts_internal
)
replay <- marginaleffects:::record_plan_aggregation(
    estimates = estimates,
    newdata = newdata,
    by = "g"
)
expect_true(all(c("blocks", "n", "weighted") %in% names(replay$agg)))
expect_true(all(vapply(replay$agg$blocks, function(x) is.matrix(x$idx), logical(1))))
expect_equal(
    sum(vapply(replay$agg$blocks, function(x) length(x$idx), integer(1))),
    nrow(estimates)
)
expect_equal(
    marginaleffects:::apply_plan_aggregation(replay$agg, estimates$estimate),
    replay$out$estimate
)

estimates <- data.table::data.table(
    rowid = 1:6,
    term = "x",
    g = rep(1:2, each = 3),
    estimate = c(Inf, 2, 4, NA, 10, 20),
    marginaleffects_wts_internal = c(0, 1, 3, 5, 0, 5)
)
newdata <- data.table::data.table(
    rowid = 1:6,
    marginaleffects_wts_internal = estimates$marginaleffects_wts_internal
)
replay <- marginaleffects:::record_plan_aggregation(
    estimates = estimates,
    newdata = newdata,
    by = "g"
)
expected <- estimates[,
    .(estimate = stats::weighted.mean(
        estimate,
        marginaleffects_wts_internal,
        na.rm = TRUE
    )),
    keyby = .(term, g)
][["estimate"]]
expect_equal(replay$out$estimate, expected)
expect_equal(
    marginaleffects:::apply_plan_aggregation(replay$agg, estimates$estimate),
    expected
)

estimates <- data.table::data.table(
    rowid = 1:4,
    term = "x",
    z = letters[1:4],
    estimate = 10:13
)
newdata <- data.table::data.table(rowid = 1:4)
bydf <- data.frame(z = c("b", "d"), by = c("B", "D"))
replay <- suppressWarnings(marginaleffects:::record_plan_aggregation(
    estimates = estimates,
    newdata = newdata,
    by = bydf
))
expect_equal(
    marginaleffects:::apply_plan_aggregation(replay$agg, estimates$estimate),
    c(11, 13)
)


# # Issue #1357
# exit_file("insight::get_datagrid() no longer returns proper e42dep")
# m <- insight::download_model("brms_linear_1")
# p <- avg_predictions(
#     m,
#     by = "e42dep",
#     newdata = insight::get_datagrid(m, by = "e42dep")
# )
# expect_inherits(p, "predictions")
