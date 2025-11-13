testthat::skip_if_not_installed("nlme")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")
requiet("nlme")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- nlme::lme(mpg ~ wt + am, random = ~ 1|cyl, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

dat_nlme <<- get_dataset("Ovary", "nlme")


# nlme::gls: marginaleffects vs. emtrends
model <- gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), dat_nlme, correlation = corAR1(form = ~ 1 | Mare))
mfx <- slopes(model)
expect_s3_class(mfx, "data.frame")
expect_false(any(mfx$estimate == 0 | is.na(mfx$estimate)))
expect_false(any(mfx$std.error == 0 | is.na(mfx$std.error)))
# emtrends
nd <- datagrid(newdata = dat_nlme, Time = 1)
mfx <- slopes(model, variables = "Time", type = "link", newdata = datagrid(Time = 1))
em <- suppressMessages(emtrends(model, ~Time, "Time", mode = "df.error", at = list(Time = 1)))
em <- tidy(em)
expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx$estimate, em$Time.trend, tolerance = .01, ignore_attr = TRUE)


# predictions: nlme::gls: no validity
model <- gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), data = dat_nlme, correlation = corAR1(form = ~ 1 | Mare))
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(dat_nlme))
expect_predictions2(model, n_row = nrow(dat_nlme))
expect_predictions2(model, newdata = head(dat_nlme), n_row = 6)


# glm: marginalmeans vs emmeans
tmp_nlme <- dat_nlme
tmp_nlme$categ <- factor(sample(letters[1:5], nrow(tmp_nlme), replace = TRUE))
tmp_nlme <<- tmp_nlme
mod <- gls(
    follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time) + categ,
    data = tmp_nlme,
    correlation = corAR1(form = ~ 1 | Mare)
)
em <- suppressMessages(emmeans(mod, specs = "categ"))
em <- tidy(em)
mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "categ") |> dplyr::arrange(categ)
expect_equal(mm$estimate, em$estimate, ignore_attr = TRUE)
expect_equal(mm$std.error, em$std.error, tolerance = 1e-5, ignore_attr = TRUE)


# issue #99: Support `lme`
mod <- lme(distance ~ age + Sex, data = Orthodont, random = ~1)
mfx <- avg_slopes(mod)
cmp <- comparisons(mod)
pre <- predictions(mod)
expect_s3_class(mfx, "slopes")
expect_s3_class(cmp, "comparisons")
expect_s3_class(pre, "predictions")
