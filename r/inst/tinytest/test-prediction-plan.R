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

form <- marginaleffects:::hypothesis_compile(~pairwise, cmp_skeleton)
expect_equal(form$hyp$kind, "matrix")
expect_inherits(form$hyp$H, "sparseMatrix")
expect_equivalent(
    form$hyp$apply(c(2, 4, 8, 16)),
    marginaleffects:::hypothesis_formula(
        data.table::data.table(
            term = "term",
            group = letters[1:4],
            estimate = c(2, 4, 8, 16)
        ),
        hypothesis = ~pairwise,
        newdata = cmp_skeleton,
        by = NULL
    )$estimate
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
for (hypothesis in linear_forms) {
    form <- marginaleffects:::hypothesis_compile(
        hypothesis,
        cmp_skeleton,
        newdata = cmp_skeleton
    )
    expect_equal(form$hyp$kind, "matrix")
    expect_inherits(form$hyp$H, "sparseMatrix")
    expected <- marginaleffects:::hypothesis_formula(
        data.table::copy(cmp_skeleton),
        hypothesis = hypothesis,
        newdata = cmp_skeleton,
        by = NULL
    )
    expect_equivalent(
        form$hyp$apply(cmp_skeleton$estimate * 2),
        marginaleffects:::hypothesis_formula(
            data.table::data.table(
                term = cmp_skeleton$term,
                group = cmp_skeleton$group,
                segment = cmp_skeleton$segment,
                estimate = cmp_skeleton$estimate * 2
            ),
            hypothesis = hypothesis,
            newdata = cmp_skeleton,
            by = NULL
        )$estimate
    )
    expect_equivalent(form$cmp$estimate, expected$estimate)
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
expected <- marginaleffects:::hypothesis_formula(
    data.table::copy(cmp_missing),
    hypothesis = difference ~ reference | segment,
    newdata = cmp_missing,
    by = NULL
)
expect_equivalent(form$hyp$apply(cmp_missing$estimate), expected$estimate)

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
expected <- marginaleffects:::hypothesis_formula(
    data.table::copy(cmp_interleaved),
    hypothesis = difference ~ pairwise | segment,
    newdata = cmp_interleaved,
    by = NULL
)
expect_equal(form$hyp$kind, "matrix")
expect_equivalent(form$hyp$apply(cmp_interleaved$estimate), expected$estimate)

form <- marginaleffects:::hypothesis_compile(ratio ~ sequential | segment, cmp_skeleton, newdata = cmp_skeleton)
expect_equal(form$hyp$kind, "formula")
expect_equivalent(
    form$hyp$apply(c(2, 4, 8, 16)),
    marginaleffects:::hypothesis_formula(
        data.table::data.table(
            term = "term",
            group = letters[1:4],
            segment = c("x", "x", "y", "y"),
            estimate = c(2, 4, 8, 16)
        ),
        hypothesis = ratio ~ sequential | segment,
        newdata = cmp_skeleton,
        by = NULL
    )$estimate
)

custom_formula_fun <- function(x) c(first = x[1], total = sum(x))
form <- marginaleffects:::hypothesis_compile(~ I(custom_formula_fun(x)) | segment, cmp_skeleton, newdata = cmp_skeleton)
expect_equal(form$hyp$kind, "formula")
expect_equivalent(
    form$hyp$apply(c(2, 4, 8, 16)),
    marginaleffects:::hypothesis_formula(
        data.table::data.table(
            term = "term",
            group = letters[1:4],
            segment = c("x", "x", "y", "y"),
            estimate = c(2, 4, 8, 16)
        ),
        hypothesis = ~ I(custom_formula_fun(x)) | segment,
        newdata = cmp_skeleton,
        by = NULL
    )$estimate
)

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
