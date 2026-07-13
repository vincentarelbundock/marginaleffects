source("helpers.R")
using("marginaleffects")

if (!requiet("adrftools")) exit_file("adrftools not installed")

values <- c(2, 4)

mod <- lm(mpg ~ wt + hp + factor(am), mtcars)
adrf <- adrftools::adrf(mod, "wt", range = values, n = 2)
adr <- summary(adrf(wt = values), simultaneous = FALSE, transform = FALSE)
mfx <- avg_predictions(mod, variables = list(wt = values), vcov = vcovUnconditional("HC0"))
expect_equivalent(mfx$std.error, adr$std.error, tolerance = 1e-6)
adr <- summary(adrftools::point_contrast(adrf(wt = values)), simultaneous = FALSE)
mfx <- avg_comparisons(mod, variables = list(wt = values), vcov = vcovUnconditional("HC0"))
expect_equivalent(mfx$std.error, adr$std.error, tolerance = 1e-6)

mod <- glm(vs ~ wt + hp + factor(am), binomial, mtcars)
adrf <- adrftools::adrf(mod, "wt", range = values, n = 2)
adr <- summary(adrf(wt = values), simultaneous = FALSE, transform = FALSE)
mfx <- avg_predictions(mod, variables = list(wt = values), vcov = vcovUnconditional("HC0"))
expect_equivalent(mfx$std.error, adr$std.error, tolerance = 1e-6)
adr <- summary(adrftools::point_contrast(adrf(wt = values)), simultaneous = FALSE)
mfx <- avg_comparisons(mod, variables = list(wt = values), vcov = vcovUnconditional("HC0"))
expect_equivalent(mfx$std.error, adr$std.error, tolerance = 1e-6)

value <- 1.25
mod <- lm(Sepal.Length ~ Petal.Width * Species + Sepal.Width, iris)
adrf <- adrftools::adrf(mod, "Petal.Width", by = "Species", range = c(.5, 2), n = 3)
adr <- summary(adrf(Petal.Width = value), simultaneous = FALSE, transform = FALSE)
mfx <- avg_predictions(mod, variables = list(Petal.Width = value), by = "Species", vcov = vcovUnconditional("HC0"))
expect_equivalent(mfx$std.error, adr$std.error, tolerance = 1e-6)
