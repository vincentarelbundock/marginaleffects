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
