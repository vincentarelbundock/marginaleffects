source("helpers.R")
using("marginaleffects")
requiet("survival")

mod <- lm(mpg ~ hp * wt + factor(cyl), data = mtcars)

pred <- predictions(
    mod,
    newdata = datagrid(hp = c(100, 120)),
    hypothesis = "b1 = b2"
)
expect_equal(nrow(pred), 1)
expect_true("std.error" %in% colnames(pred))
expect_false(anyNA(pred$std.error))

pred <- avg_predictions(
    mod,
    by = "cyl",
    hypothesis = "plogis(b1) - plogis(b2) = 0"
)
expect_equal(nrow(pred), 1)
expect_true("std.error" %in% colnames(pred))

cmp_skeleton <- data.table::data.table(
    term = "term",
    group = letters[1:4],
    segment = c("x", "x", "y", "y"),
    estimate = c(1, 2, 4, 8)
)

# Reference implementations of the formula shortcuts, written directly in
# base R, independently of the package's compiled stages.
shortcut_reference <- function(rhs, x) {
    n <- length(x)
    pairs <- function(tri) {
        ij <- which(tri(matrix(FALSE, n, n)), arr.ind = TRUE)
        x[ij[, 1]] - x[ij[, 2]]
    }
    switch(rhs,
        reference = (x - x[1])[-1],
        revreference = (x[1] - x)[-1],
        sequential = diff(x),
        pairwise = pairs(lower.tri),
        revpairwise = pairs(upper.tri),
        meandev = x - mean(x),
        meanotherdev = x - (sum(x) - x) / (n - 1),
        trt_vs_ctrl = mean(x[-1]) - x[1],
        poly = as.vector(crossprod(stats::contr.poly(n)[, seq_len(min(5, n - 1)), drop = FALSE], x)),
        helmert = as.vector(x %*% stats::contr.helmert(n))
    )
}

form <- marginaleffects:::hypothesis_compile(~pairwise, cmp_skeleton)
expect_equal(form$hyp$kind, "matrix")
expect_inherits(form$hyp$H, "sparseMatrix")
expect_equivalent(
    form$hyp$apply(c(2, 4, 8, 16)),
    shortcut_reference("pairwise", c(2, 4, 8, 16))
)

linear_forms <- list(
    ~reference,
    ~revreference,
    ~sequential,
    ~pairwise,
    ~revpairwise,
    ~meandev,
    ~meanotherdev,
    ~trt_vs_ctrl,
    ~poly,
    ~helmert,
    difference ~ pairwise | segment
)
# The centering shortcuts compile to a structured pullback rather than a
# matrix: their operators are dense in every entry, so materializing them is
# O(n^2) memory for a rank-one update.
pullback_forms <- c("meandev", "meanotherdev")
for (hypothesis in linear_forms) {
    form <- marginaleffects:::hypothesis_compile(
        hypothesis,
        cmp_skeleton,
        newdata = cmp_skeleton
    )
    rhs <- all.vars(hypothesis[[length(hypothesis)]])[1]
    if (rhs %in% pullback_forms) {
        expect_equal(form$hyp$kind, "formula")
        expect_true(is.function(form$hyp$pullback))
    } else {
        expect_equal(form$hyp$kind, "matrix")
        expect_inherits(form$hyp$H, "sparseMatrix")
    }
    reference <- function(est) {
        if (length(hypothesis) == 3) {
            unlist(lapply(split(est, cmp_skeleton$segment), shortcut_reference, rhs = rhs), use.names = FALSE)
        } else {
            shortcut_reference(rhs, est)
        }
    }
    expect_equivalent(
        form$hyp$apply(cmp_skeleton$estimate * 2),
        reference(cmp_skeleton$estimate * 2)
    )
    expect_equivalent(form$cmp$estimate, reference(cmp_skeleton$estimate))
}

cmp_missing <- data.table::data.table(
    term = "term",
    segment = rep(c("x", "y"), each = 3),
    estimate = c(1, NA, 3, 4, 5, 6)
)
form <- marginaleffects:::hypothesis_compile(
    difference ~ reference | segment,
    cmp_missing,
    newdata = cmp_missing
)
expect_equivalent(form$hyp$apply(cmp_missing$estimate), c(NA, 2, 1, 2))

cmp_interleaved <- data.table::data.table(
    term = "term",
    segment = c("y", "x", "y", "x", "y"),
    estimate = seq_len(5)
)
form <- marginaleffects:::hypothesis_compile(
    difference ~ pairwise | segment,
    cmp_interleaved,
    newdata = cmp_interleaved
)
expect_equal(form$hyp$kind, "matrix")
# groups are sorted by key: x = (2, 4), then y = (1, 3, 5)
expect_equivalent(form$hyp$apply(cmp_interleaved$estimate), c(2, 2, 4, 2))

form <- marginaleffects:::hypothesis_compile(ratio ~ sequential | segment, cmp_skeleton, newdata = cmp_skeleton)
expect_equal(form$hyp$kind, "formula")
expect_equivalent(form$hyp$apply(c(2, 4, 8, 16)), c(4 / 2, 16 / 8))

# A custom formula function stays a closure even when it happens to be
# linear: probing cannot prove linearity of arbitrary code, so promotion is
# reserved for syntax the package itself compiled. The composed Jacobian
# still differentiates the closure exactly at the estimate.
custom_formula_fun <- function(x) c(first = x[1], total = sum(x))
form <- marginaleffects:::hypothesis_compile(~ I(custom_formula_fun(x)) | segment, cmp_skeleton, newdata = cmp_skeleton)
expect_equal(form$hyp$kind, "formula")
expect_equivalent(form$hyp$apply(c(2, 4, 8, 16)), c(2, 6, 8, 24))

local({
    assign("prediction_plan_string_helper", function(x) x + 1, envir = .GlobalEnv)
    on.exit(rm("prediction_plan_string_helper", envir = .GlobalEnv), add = TRUE)
    pred <- avg_predictions(
        mod,
        by = "cyl",
        hypothesis = "prediction_plan_string_helper(b1) - prediction_plan_string_helper(b2) = 0"
    )
    expect_equal(nrow(pred), 1)
    expect_true("std.error" %in% colnames(pred))
})

avg <- avg_predictions(mod, by = "cyl")
expect_equal(nrow(avg), 3)
expect_true("std.error" %in% colnames(avg))

expect_error(
    predictions(mod, by = "cyl", byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)
expect_error(
    avg_predictions(mod, by = "cyl", byfun = sum),
    pattern = "`byfun`.*not supported.*`hypothesis`"
)

surv <- survival::survreg(
    survival::Surv(time, status) ~ ph.ecog + age + sex,
    data = survival::lung,
    dist = "weibull"
)
quant <- predictions(surv, type = "quantile", p = 0.5)
expect_true("std.error" %in% colnames(quant))
expect_false(anyNA(quant$std.error))

expect_error(
    marginaleffects:::plan_std_error(
        built = list(plan = list(kind = "unknown")),
        mfx = NULL,
        estimates = data.frame(estimate = 1),
        type = "response",
        dots = list()
    ),
    pattern = "Unknown plan kind"
)

mfx_draws <- marginaleffects:::new_marginaleffects_internal(
    model = mod,
    call = quote(predictions(mod)),
    draws = matrix(1, nrow = 1, ncol = 2),
    vcov_model = stats::vcov(mod)
)
out <- marginaleffects:::plan_std_error(
    built = list(plan = NULL),
    mfx = mfx_draws,
    estimates = data.frame(estimate = 1),
    type = "response",
    dots = list()
)
expect_equal(out$estimates$estimate, 1)
