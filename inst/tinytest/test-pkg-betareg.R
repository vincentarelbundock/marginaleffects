source("helpers.R")
using("marginaleffects")

requiet("betareg")
requiet("margins")
requiet("emmeans")
requiet("broom")

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
emtrends(mod, ~temp, "temp", at = list("batch" = tmp$batch[1])))
em <- tidy(em)
expect_equivalent(mfx$estimate, em$temp.trend, tolerance = .001)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)


# marginaleffects: vs. Stata
# stata does not include contrasts
stata <- readRDS(testing_path("stata/stata.rds"))[["betareg_betareg_01"]]
mfx <- merge(tidy(slopes(mod)), stata)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .0001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .0001)


# predictions: no validity
pred <- suppressWarnings(predictions(mod))
expect_predictions(pred, n_row = nrow(GasolineYield))
pred <- predictions(mod, newdata = datagrid(batch = 1:3, temp = c(300, 350)))
expect_predictions(pred, n_row = 6)


# marginalmeans: vs. emmeans
mm <- marginal_means(mod, type = "link")
expect_inherits(mm, "marginalmeans")
expect_equal(nrow(mm), 10)
mm <- tidy(mm)
em <- broom::tidy(emmeans::emmeans(mod, "batch"))
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$std.error, tolerance = 0.01)


rm(list = ls())