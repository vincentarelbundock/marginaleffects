source("helpers.R")

# requiet("AER")
# requiet("emmeans")
# requiet("broom")
# tol_se <- 1e-4
#
#
# # tobit: marginaleffects vs. Stata
# data("Affairs", package = "AER")
# stata <- readRDS(testing_path("stata/stata.rds"))$aer_tobit
# mod1 <- AER::tobit(
# affairs ~ age + yearsmarried + religiousness + occupation + rating, 
# data = Affairs)
# mfx <- merge(tidy(marginaleffects(mod1)), stata)
# expect_marginaleffects(mod1, n_unique = 1)
# expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .00001)
# expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .00001)
# stata <- readRDS(testing_path("stata/stata.rds"))$aer_tobit_right
# mod2 <- AER::tobit(
# affairs ~ age + yearsmarried + religiousness + occupation + rating, 
# right = 4, data = Affairs)
# mfx <- merge(tidy(marginaleffects(mod2)), stata)
# expect_marginaleffects(mod2, n_unique = 1)
# expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .1)
# expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .1)
#
#
# # tobit: marginalmeans vs. emmeans
# data("Affairs", package = "AER")
# tmp <- Affairs
# tmp$religiousness <- as.logical(tmp$religiousness)
# mod <- tobit(
# affairs ~ age + yearsmarried + religiousness + occupation + rating,
# data = tmp)
# em <- emmeans(mod, specs = "religiousness")
# em <- tidy(em)
# mm <- tidy(marginalmeans(mod, variables = "religiousness"))
# expect_equivalent(mm$estimate, em$estimate)
# expect_equivalent(mm$std.error, em$std.error, tolerance = tol_se)
#
#
#
# # marginaleffects vs. emtrends
# data("Affairs", package = "AER")
# mod <- AER::tobit(affairs ~ age + yearsmarried, data = Affairs)
# mfx <- marginaleffects(mod, newdata = datagrid(age = 30, yearsmarried = 5))
# em1 <- emmeans::emtrends(mod, ~age, "age", at = list(age = 30, yearsmarried = 5))
# em2 <- emmeans::emtrends(mod, ~yearsmarried, "yearsmarried", at = list(age = 30, yearsmarried = 5))
# em1 <- tidy(em1)
# em2 <- tidy(em2)
# expect_equivalent(mfx$dydx[1], em1$age.trend)
# expect_equivalent(mfx$std.error[1], em1$std.error, tolerance = .00001)
# expect_equivalent(mfx$dydx[2], em2$yearsmarried.trend)
# expect_equivalent(mfx$std.error[2], em2$std.error, tolerance = .00002)
#
#
# # predictions: tobit: no validity
# data("Affairs", package = "AER")
# mod <- AER::tobit(
# affairs ~ age + yearsmarried + religiousness + occupation + rating, 
# data = Affairs)
# pred <- predictions(mod)
# expect_predictions(pred, n_row = nrow(Affairs))
#
