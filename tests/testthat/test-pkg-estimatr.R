testthat::skip_if_not_installed("estimatr")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("broom")

requiet("estimatr")
requiet("emmeans")
requiet("margins")
requiet("broom")

# Basic expectation tests
mod_simple <- estimatr::lm_robust(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

Km <- get_dataset("Kmenta", "sem")
dat <- transform(mtcars, cyl = factor(cyl))

# lm_lin: no validity
mod <- lm_lin(mpg ~ am, ~ hp + cyl, data = dat)
expect_slopes2(mod)
expect_slopes2(mod, n_unique = 9)

# iv_robust vs. stata
stata <- readRDS(testing_path("stata/stata.rds"))$estimatr_iv_robust
model <- iv_robust(
    Q ~ P + D | D + F + A,
    se_type = "stata",
    data = Km
)
mfx <- slopes(model, newdata = Km)
tid <- tidy(mfx)
mfx <- merge(tid, stata)
expect_equal(mfx$estimate, mfx$dydxstata, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1, ignore_attr = TRUE)


# lm_robust vs. stata vs. emtrends
model <- lm_robust(carb ~ wt + factor(cyl), se_type = "HC2", data = dat)
stata <- readRDS(testing_path("stata/stata.rds"))$estimatr_lm_robust
mfx <- tidy(slopes(model))
mfx$term <- ifelse(mfx$contrast == "6 - 4", "6.cyl", mfx$term)
mfx$term <- ifelse(mfx$contrast == "8 - 4", "8.cyl", mfx$term)
mfx <- merge(mfx, stata)
expect_equal(mfx$estimate, mfx$dydxstata, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1, ignore_attr = TRUE)
# emtrends
mfx <- slopes(model, newdata = datagrid(cyl = 4, wt = 2, newdata = dat), variables = "wt")
em <- emtrends(model, ~wt, "wt", at = list(cyl = 4, wt = 2))
em <- tidy(em)
expect_equal(mfx$estimate, em$wt.trend, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)
# margins does not support standard errors
tmp <- mtcars
tmp$cyl <- factor(tmp$cyl)
mod <- lm_robust(carb ~ wt + cyl, data = tmp, se_type = "stata")
mar <- margins(mod, data = head(tmp))
mfx <- slopes(mod, newdata = head(tmp))
expect_margins2(mfx, mar, se = FALSE)


# iv_robust: predictions: no validity
# skip_if_not_installed("insight", minimum_version = "0.17.1")
model <- iv_robust(Q ~ P + D | D + F + A, se_type = "stata", data = Km)
expect_predictions2(model, newdata = Km, n_row = nrow(Km))
expect_predictions2(model, newdata = head(Km), n_row = 6)


# lm_robust: marginalmeans predictions: no validity
# skip_if_not_installed("insight", minimum_version = "0.17.1")
tmp <- mtcars
tmp$cyl <- as.factor(tmp$cyl)
tmp$am <- as.logical(tmp$am)
model <- lm_robust(carb ~ wt + am + cyl, se_type = "stata", data = tmp)
expect_predictions2(model, n_row = nrow(tmp))
expect_predictions2(model, newdata = head(tmp), n_row = 6)
