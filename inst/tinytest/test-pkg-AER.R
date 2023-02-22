# newdata must be explicit otherwise this only works interactively
source("helpers.R")
using("marginaleffects")

requiet("AER")
requiet("emmeans")
requiet("broom")
tol_se <- 1e-4

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/Affairs.csv")


# tobit: marginaleffects vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$aer_tobit
mod1 <- tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating,
    data = dat)
mfx <- merge(tidy(slopes(mod1, newdata = dat)), stata)
expect_slopes(mod1, n_unique = 1, newdata = dat)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .00001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .00001)

stata <- readRDS(testing_path("stata/stata.rds"))$aer_tobit_right
mod2 <- tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating,
    right = 4, data = dat
)
mfx <- merge(tidy(slopes(mod2, newdata = dat)), stata)
expect_slopes(mod2, n_unique = 1, newdata = dat)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .1)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .1)


# TODO: Works interactively but not in CI
# # tobit: marginalmeans vs. emmeans
# tmp <- dat
# tmp$religiousness <- as.logical(tmp$religiousness)
# mod <- tobit(
#     affairs ~ age + yearsmarried + religiousness + occupation + rating,
#     data = tmp)
# em <- emmeans(mod, specs = "religiousness")
# em <- tidy(em)
# mm <- tidy(marginal_means(mod))
# expect_equivalent(mm$estimate, em$estimate)
# expect_equivalent(mm$std.error, em$std.error, tolerance = tol_se)


# marginaleffects vs. emtrends
mod <- tobit(affairs ~ age + yearsmarried, data = dat)
mfx <- slopes(mod, newdata = datagrid(age = 30, yearsmarried = 5))
em1 <- emmeans::emtrends(mod, ~age, "age", at = list(age = 30, yearsmarried = 5))
em2 <- emmeans::emtrends(mod, ~yearsmarried, "yearsmarried", at = list(age = 30, yearsmarried = 5))
em1 <- tidy(em1)
em2 <- tidy(em2)
expect_equivalent(mfx$estimate[1], em1$age.trend)
expect_equivalent(mfx$std.error[1], em1$std.error, tolerance = .00001)
expect_equivalent(mfx$estimate[2], em2$yearsmarried.trend)
expect_equivalent(mfx$std.error[2], em2$std.error, tolerance = .00002)


# predictions: tobit: no validity
mod <- AER::tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating,
    data = dat)
pred <- predictions(mod, newdata = dat)
expect_predictions(pred, n_row = nrow(dat))



rm(list = ls())