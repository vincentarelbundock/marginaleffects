testthat::skip_if_not_installed("afex")
testthat::skip_if_not_installed("emmeans")

requiet("afex")
requiet("emmeans")

# Basic expectation tests
data(md_12.1, package = "afex")
mod_simple <- afex::aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

data(md_12.1, package = "afex")
mod <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

# no validity
expect_slopes2(mod)
pre <- predictions(mod)
expect_s3_class(pre, "predictions")
cmp <- comparisons(mod)
expect_s3_class(cmp, "comparisons")

# contrasts vs emmeans
cmp <- avg_comparisons(mod, variables = "angle", newdata = "balanced", by = "angle") |>
    subset(angle == "X0")
em <- emmeans(mod, ~angle)
em <- emmeans::contrast(em, method = "trt.vs.ctrl1")
em <- data.frame(em)
expect_equal(cmp$estimate, em$estimate)
expect_equal(cmp$std.error, em$SE, tolerance = 1e-6)

# predictions vs emmeans
pre <- predictions(
    mod,
    newdata = datagrid(
        angle = c("X0", "X4", "X8"),
        noise = md_12.1$noise
    )
)
emm <- emmeans(mod, c("noise", "angle"))
emm <- data.frame(emm)
expect_equal(pre$estimate, emm$emmean, ignore_attr = TRUE)
expect_equal(pre$std.error, emm$SE, tolerance = 1e-6, ignore_attr = TRUE)


# coefficient matrix (ANOVA on full design)
data(obk.long, package = "afex")
mod <- suppressMessages(aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long,
    observed = "gender"
))

em <- data.frame(emmeans(mod, ~phase))
mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "phase")
expect_equal(mm$estimate, em$emmean, ignore_attr = TRUE)
expect_equal(mm$std.error, em$SE, tolerance = 1e-6, ignore_attr = TRUE)


# data from https://github.com/mattansb/Analysis-of-Factorial-Designs-foR-Psychologists/03 Main and simple effects analysis
Phobia <- readRDS(testing_path("stata/databases/Phobia.rds"))
mod <- suppressMessages(aov_ez(
    id = "ID",
    dv = "BehavioralAvoidance",
    between = c("Condition", "Gender"),
    data = Phobia,
    anova_table = list(es = "pes")
))

pre <- predictions(mod)
mfx <- slopes(mod)
expect_s3_class(pre, "predictions")
expect_s3_class(cmp, "comparisons")
expect_false(anyNA(pre$std.error))
expect_false(anyNA(cmp$std.error))
