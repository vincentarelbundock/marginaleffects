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

byfun_sum <- suppressWarnings(avg_predictions(mod, by = "cyl", byfun = sum))
hyp_sum <- function(x) {
    data.frame(
        hypothesis = sort(unique(x$cyl)),
        estimate = as.numeric(tapply(x$estimate, x$cyl, sum))
    )
}
hyp_sum <- predictions(mod, hypothesis = hyp_sum)
expect_equivalent(byfun_sum$estimate, hyp_sum$estimate)
expect_equivalent(byfun_sum$std.error, hyp_sum$std.error, tolerance = 1e-7)

options(
    marginaleffects_byfun_deprecated = TRUE,
    marginaleffects_safe = TRUE
)
expect_warning(
    avg_predictions(mod, by = "cyl", byfun = sum),
    pattern = "`byfun` is deprecated"
)
options(marginaleffects_safe = FALSE)

surv <- survival::survreg(
    survival::Surv(time, status) ~ ph.ecog + age + sex,
    data = survival::lung,
    dist = "weibull"
)
quant <- predictions(surv, type = "quantile", p = 0.5)
expect_true("std.error" %in% colnames(quant))
expect_false(anyNA(quant$std.error))
