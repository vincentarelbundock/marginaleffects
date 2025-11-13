testthat::skip_if(!EXPENSIVE, "EXPENSIVE")

testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("haven")
testthat::skip_if_not_installed("lme4")
testthat::skip_if_not_installed("insight")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")

requiet("margins")
requiet("haven")
requiet("lme4")
requiet("insight")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- lme4::lmer(mpg ~ wt + am + (1 | cyl), data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# satterthwaite (no validity)
# skip_if_not_installed("insight", minimum_version = "0.17.1")
dat_lme4 <- mtcars
dat_lme4$cyl <- factor(dat_lme4$cyl)
dat_lme4 <- dat_lme4
mod <- lme4::lmer(mpg ~ hp + (1 | cyl), data = dat_lme4)
x <- predictions(mod, re.form = NA)
y <- predictions(mod, vcov = "satterthwaite", re.form = NA)
z <- predictions(mod, vcov = "kenward-roger", re.form = NA)
expect_true(all(x$conf.low != y$conf.low))
expect_true(all(x$conf.low != z$conf.low))
expect_true(all(y$conf.low != z$conf.low))
expect_true(all(x$p.value != y$p.value))
expect_true(all(x$p.value != z$p.value))
expect_true(all(y$p.value != z$p.value))
# kenward-roger adjusts vcov but not satterthwaite
expect_equal(x$std.error, y$std.error, ignore_attr = TRUE)
expect_true(all(x$std.error != z$std.error))
expect_true(all(y$std.error != z$std.error))


x <- plot_predictions(mod, condition = "hp", draw = FALSE, re.form = NA)
y <- plot_predictions(mod, condition = "hp", vcov = "satterthwaite", draw = FALSE, re.form = NA)
z <- plot_predictions(mod, condition = "hp", vcov = "kenward-roger", draw = FALSE, re.form = NA)
expect_true(all(x$conf.low != y$conf.low))
expect_true(all(x$conf.low != z$conf.low))
expect_true(all(y$conf.low != z$conf.low))
expect_equal(x$std.error, y$std.error, ignore_attr = TRUE)
expect_true(all(x$std.error != z$std.error))
expect_true(all(y$std.error != z$std.error))

# comparisons
x <- comparisons(mod, re.form = NA)
y <- comparisons(mod, vcov = "satterthwaite", re.form = NA)
z <- comparisons(mod, vcov = "kenward-roger", re.form = NA)
expect_true(all(x$conf.low != y$conf.low))
expect_true(all(x$conf.low != z$conf.low))
expect_true(all(y$conf.low != z$conf.low))
expect_true(all(x$std.error == y$std.error))
expect_true(all(x$std.error != z$std.error))
expect_true(all(y$std.error != z$std.error))

# at the mean (regression test)
mfx <- slopes(
    mod,
    newdata = datagrid(),
    vcov = "satterthwaite",
    re.form = NA
)
expect_s3_class(mfx, "marginaleffects")


# GLM not supported
mod <- glmer(am ~ hp + (1 | cyl), family = binomial, data = dat_lme4)
expect_error(comparisons(mod, vcov = "satterthwaite", re.form = NA), regexp = "(?i)unable.*satter")
expect_error(predictions(mod, vcov = "kenward-roger", re.form = NA), regexp = "(?i)unable.*kenward")

# type = "link"
w <- predict(mod, type = "link")
x <- get_predict(mod, type = "link")
y <- get_predict(mod, type = "link", conf.level = .9)
z <- get_predicted(mod, predict = "link")
expect_equal(w, x$estimate, ignore_attr = TRUE)
expect_equal(w, y$estimate, ignore_attr = TRUE)
expect_equal(w, as.numeric(z), ignore_attr = TRUE)

# type = "response"
w <- predict(mod, type = "response")
x <- get_predict(mod, type = "response")
y <- get_predict(mod, type = "response", conf.level = .9)
z <- get_predicted(mod, predict = "expectation")
expect_equal(w, x$estimate, ignore_attr = TRUE)
expect_equal(w, y$estimate, ignore_attr = TRUE)
expect_equal(w, as.numeric(z), ignore_attr = TRUE)

# confidence intervals (weak test)
w <- get_predict(mod, conf.level = .95)
x <- get_predict(mod, conf.level = .90)
expect_true(all(w$conf.low < x$conf.low))
expect_true(all(w$conf.high > x$conf.high))

# no random effects: grand mean
w <- predict(mod, re.form = NA, type = "response")
x <- get_predict(mod, re.form = NA, type = "response")
expect_equal(w, x$estimate, ignore_attr = TRUE)
