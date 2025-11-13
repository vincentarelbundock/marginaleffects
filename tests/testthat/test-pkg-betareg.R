testthat::skip_if_not_installed("betareg")
testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")

requiet("betareg")
requiet("margins")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
data("GasolineYield", package = "betareg")
mod_simple <- betareg::betareg(yield ~ temp + batch, data = GasolineYield)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

data("GasolineYield", package = "betareg")
tmp <- GasolineYield
tmp$batch <- factor(tmp$batch)
dat <- tmp
mod <- betareg::betareg(yield ~ batch + temp, data = dat)

# marginaleffects: vs. margins vs. emmeans
set.seed(1024)
res <- slopes(mod, variables = "temp")
mar <- data.frame(margins::margins(mod, unit_ses = TRUE))
expect_margins2(res, mar, tolerance = 0.1)

# emtrends
mfx <- slopes(mod, newdata = datagrid(batch = 1), variables = "temp")
em <- suppressWarnings(
    emtrends(mod, ~temp, "temp", at = list("batch" = tmp$batch[1]))
)
em <- tidy(em)
expect_equal(mfx$estimate, em$temp.trend, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)


# marginaleffects: vs. Stata
# stata does not include contrasts
stata <- readRDS(testing_path("stata/stata.rds"))[["betareg_betareg_01"]]
mfx <- merge(avg_slopes(mod), stata)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .0001, ignore_attr = TRUE)


# predictions: no validity
pred <- suppressWarnings(predictions(mod))
expect_predictions2(mod, n_row = nrow(GasolineYield))
pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)))
expect_predictions2(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)), n_row = 6)

# link
mm <- predictions(mod, type = "link", by = "batch", newdata = datagrid(grid_type = "balanced")) |>
    dplyr::arrange(batch)

# marginalmeans: vs. emmeans
mm <- predictions(mod,
    type = "response", by = "batch",
    newdata = datagrid(grid_type = "balanced", FUN_integer = mean)) |>
    dplyr::arrange(batch)
expect_s3_class(mm, "predictions")
expect_equal(nrow(mm), 10)
em <- broom::tidy(emmeans::emmeans(mod, "batch"))
expect_equal(mm$estimate, em$estimate, ignore_attr = TRUE)
expect_equal(mm$std.error, em$std.error, tolerance = 0.01, ignore_attr = TRUE)

# Issue #1391
mm_link <- predictions(mod, type = "link", by = "batch", newdata = datagrid(grid_type = "balanced")) |>
    dplyr::arrange(batch)
expect_true(all(mm_link$estimate < mm$estimate))


# Issue #1568: Log(nu) parameter
dat_loss <- get_dataset("LossAversion", "betareg")
mod_loss <- betareg(invest ~ grade * (arrangement + age) + male |
    arrangement + male + grade, data = dat_loss)
p <- avg_predictions(mod_loss, by = "grade")
expect_s3_class(p, "predictions")
expect_false(anyNA(p$estimate))
expect_false(anyNA(p$std.error))
