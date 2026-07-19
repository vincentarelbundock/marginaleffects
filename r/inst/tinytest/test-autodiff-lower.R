source("helpers.R")
using("marginaleffects")

make_prediction_lowering <- function(model, by = FALSE, wts = FALSE, hypothesis = NULL) {
    call <- marginaleffects:::construct_call(model, "predictions")
    model <- marginaleffects:::sanitize_model(model, call = call, newdata = NULL, wts = wts, vcov = TRUE, by = by)
    mfx <- marginaleffects:::new_marginaleffects_internal(
        model = model,
        call = call,
        by = by,
        type = "response"
    )
    mfx <- marginaleffects:::add_newdata(mfx, rlang::quo(NULL), newdata = NULL, by = by, wts = wts)
    mfx <- marginaleffects:::add_by(mfx, by)
    mfx <- marginaleffects:::add_hypothesis(mfx, hypothesis)
    mfx@newdata <- marginaleffects:::add_model_matrix_attribute(mfx)
    built <- marginaleffects:::prediction_plan_build(
        mfx = mfx,
        type = "response",
        by = by,
        hypothesis = hypothesis
    )
    list(
        low = marginaleffects:::autodiff_lower_predictions(built$plan, mfx, "response"),
        plan = built$plan,
        mfx = mfx
    )
}

mod <- lm(mpg ~ hp + wt + factor(cyl), data = mtcars)

pred <- make_prediction_lowering(mod)
expect_true(pred$low$ok)
expect_equal(pred$low$coefs, get_coef(mod))
expect_equal(pred$low$spec$kind, "predictions")
expect_equal(nrow(pred$low$spec$X), nrow(mtcars))
expect_null(pred$low$spec$agg)
expect_null(pred$low$spec$hyp)

avg <- make_prediction_lowering(mod, by = "cyl")
expect_true(avg$low$ok)
expect_true(is.list(avg$low$spec$agg))
expect_equal(avg$low$spec$agg$num_segments, 3L)
expect_true(all(avg$low$spec$agg$segments %in% seq_len(3L)))

H <- matrix(c(1, -1, 0), nrow = 3)
hyp <- make_prediction_lowering(mod, by = "cyl", hypothesis = H)
expect_true(hyp$low$ok)
expect_true(is.matrix(hyp$low$spec$hyp))
expect_equal(ncol(hyp$low$spec$hyp), 1L)

bad <- pred$plan
bad$has_na <- TRUE
low <- marginaleffects:::autodiff_lower_predictions(bad, pred$mfx, "response")
expect_false(low$ok)
expect_equal(low$reason, "missing values in predictions")

bad <- avg$plan
bad$agg$weighted <- TRUE
bad$agg$blocks[[1]]$w <- matrix(
    NA_real_,
    nrow = nrow(bad$agg$blocks[[1]]$idx),
    ncol = ncol(bad$agg$blocks[[1]]$idx)
)
low <- marginaleffects:::autodiff_lower_predictions(bad, avg$mfx, "response")
expect_false(low$ok)
expect_equal(low$reason, "missing values in weights")

X <- attr(pred$mfx@newdata, "marginaleffects_model_matrix")
hi <- pred$mfx@newdata
lo <- pred$mfx@newdata
attr(hi, "marginaleffects_model_matrix") <- X + 1
attr(lo, "marginaleffects_model_matrix") <- X
plan <- list(
    n_pred = nrow(X),
    need_y = FALSE,
    na_keep = NULL,
    perm = NULL,
    groups = list(list(
        idx = seq_len(nrow(X)),
        out_idx = seq_len(nrow(X)),
        scalar = FALSE,
        uses_y = FALSE,
        fun_key = "difference",
        args = list()
    )),
    n_comp = nrow(X),
    est_keep = NULL,
    agg = NULL,
    hyp = NULL,
    check = list(n_pred = nrow(X))
)
cmp <- marginaleffects:::autodiff_lower_comparisons(plan, pred$mfx, "response", hi = hi, lo = lo)
expect_true(cmp$ok)
expect_equal(cmp$spec$kind, "comparisons")
expect_equal(cmp$spec$ops[[1]]$op, "difference")

plan$groups[[1]]$fun_key <- NA_character_
cmp <- marginaleffects:::autodiff_lower_comparisons(plan, pred$mfx, "response", hi = hi, lo = lo)
expect_false(cmp$ok)
expect_equal(cmp$reason, "custom comparison functions")
