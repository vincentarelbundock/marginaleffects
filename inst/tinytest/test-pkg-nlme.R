source("helpers.R")
using("marginaleffects")

requiet("nlme")
requiet("emmeans")
requiet("broom")

dat <<- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/nlme/Ovary.csv")


# nlme::gls: marginaleffects vs. emtrends
model <- gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), dat,
    correlation = corAR1(form = ~ 1 | Mare))
mfx <- slopes(model)
expect_inherits(mfx, "data.frame")
expect_false(any(mfx$estimate == 0 | is.na(mfx$estimate)))
expect_false(any(mfx$std.error == 0 | is.na(mfx$std.error)))
# emtrends
nd <- datagrid(newdata = dat, Time = 1)
mfx <- slopes(model,
    variables = "Time",
    type = "link",
    newdata = datagrid(Time = 1))
em <- suppressMessages(emtrends(model, ~Time, "Time", mode = "df.error", at = list(Time = 1)))
em <- tidy(em)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)
expect_equivalent(mfx$estimate, em$Time.trend, tolerance = .01)


# predictions: nlme::gls: no validity
model <- gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time),
    data = dat, correlation = corAR1(form = ~ 1 | Mare))
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(dat))
expect_predictions(pred1, n_row = nrow(dat))
expect_predictions(pred2, n_row = 6)


# glm: marginalmeans vs emmeans
tmp <- dat
tmp$categ <- factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
tmp <<- tmp
mod <- gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time) + categ,
    data = tmp, correlation = corAR1(form = ~ 1 | Mare))
em <- suppressMessages(emmeans(mod, specs = "categ"))
em <- tidy(em)
mm <- marginal_means(mod, variables = "categ")
expect_marginal_means(mm)
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$std.error)



# issue #99: Support `lme`
if (packageVersion("insight") < "0.19.0.12") exit_file("insight version")
mod <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
mfx <- avg_slopes(mod)
cmp <- comparisons(mod)
pre <- predictions(mod)
expect_inherits(mfx, "slopes")
expect_inherits(cmp, "comparisons")
expect_inherits(pre, "predictions")






source("helpers.R")
rm(list = ls())