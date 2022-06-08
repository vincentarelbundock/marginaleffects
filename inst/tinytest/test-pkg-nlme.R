source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("nlme")
requiet("emmeans")
requiet("broom")

# nlme::gls: marginaleffects vs. emtrends
model <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
         correlation = corAR1(form = ~ 1 | Mare))
mfx <- marginaleffects(model)
expect_inherits(mfx, "data.frame")
expect_false(any(mfx$dydx == 0 |  is.na(mfx$dydx)))
expect_false(any(mfx$std.error == 0 |  is.na(mfx$std.error)))
# emtrends
mfx <- marginaleffects(model,
                   variables = "Time",
                   type = "link",
                   newdata = datagrid(Time = 1)) 
em <- suppressMessages(emtrends(model, ~Time, "Time", mode = "df.error", at = list(Time = 1)))
em <- tidy(em)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)
expect_equivalent(mfx$dydx, em$Time.trend, tolerance = .01)


# predictions: nlme::gls: no validity
model <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
         data = Ovary, correlation = corAR1(form = ~ 1 | Mare))
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(Ovary))
expect_predictions(pred1, n_row = nrow(Ovary))
expect_predictions(pred2, n_row = 6)


# glm: marginalmeans vs emmeans
data(package = "nlme", "Ovary")
tmp <- Ovary
tmp$categ <- factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
tmp <<- tmp
mod <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time) + categ,
       data = tmp, correlation = corAR1(form = ~ 1 | Mare))
em <- suppressMessages(emmeans(mod, specs = "categ"))
em <- tidy(em)
mm <- marginalmeans(mod, variables = "categ")
expect_marginalmeans(mm)
expect_equivalent(mm$marginalmean, em$estimate)
expect_equivalent(mm$std.error, em$std.error)

