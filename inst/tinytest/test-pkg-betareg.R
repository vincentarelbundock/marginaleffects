source("helpers.R")
using("marginaleffects")

requiet("betareg")
requiet("margins")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
data("GasolineYield", package = "betareg")
mod_simple <- betareg::betareg(yield ~ temp + batch, data = GasolineYield)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

data("GasolineYield", package = "betareg")
tmp <- GasolineYield
tmp$batch <- factor(tmp$batch)
dat <- tmp
mod <- betareg::betareg(yield ~ batch + temp, data = dat)

# marginaleffects: vs. margins vs. emmeans
set.seed(1024)
res <- slopes(mod, variables = "temp")
mar <- data.frame(margins::margins(mod, unit_ses = TRUE))
expect_true(expect_margins(res, mar, tolerance = 0.1))

# emtrends
mfx <- slopes(mod, newdata = datagrid(batch = 1), variables = "temp")
em <- suppressWarnings(
    emtrends(mod, ~temp, "temp", at = list("batch" = tmp$batch[1]))
)
em <- tidy(em)
expect_equivalent(mfx$estimate, em$temp.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)


# marginaleffects: vs. Stata
# stata does not include contrasts
stata <- readRDS(testing_path("stata/stata.rds"))[["betareg_betareg_01"]]
mfx <- merge(avg_slopes(mod), stata)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .0001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .0001)


# predictions: no validity
pred <- suppressWarnings(predictions(mod))
expect_predictions(mod, n_row = nrow(GasolineYield))
pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)))
expect_predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)), n_row = 6)

# link
mm <- predictions(mod, type = "link", by = "batch", newdata = datagrid(grid_type = "balanced")) |>
    dplyr::arrange(batch)

# marginalmeans: vs. emmeans
mm <- predictions(mod,
    type = "response", by = "batch",
    newdata = datagrid(grid_type = "balanced", FUN_integer = mean)) |>
    dplyr::arrange(batch)
expect_inherits(mm, "predictions")
expect_equal(nrow(mm), 10)
em <- broom::tidy(emmeans::emmeans(mod, "batch"))
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$std.error, tolerance = 0.01)

# Issue #1391
mm_link <- predictions(mod, type = "link", by = "batch", newdata = datagrid(grid_type = "balanced")) |>
    dplyr::arrange(batch)
expect_true(all(mm_link$estimate < mm$estimate))
