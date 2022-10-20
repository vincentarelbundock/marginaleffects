source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("margins")
requiet("broom")
requiet("emmeans")
requiet("dplyr")


guerry <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv")

# glm: marginaleffects
set.seed(1024)
N <- 1e2
dat <- data.frame(x1 = rnorm(N),
                  x2 = rnorm(N),
                  x3 = rnorm(N),
                  x4 = rnorm(N),
                  e = rnorm(N))
dat$y <- as.numeric(plogis(
    dat$x1 + dat$x2 + dat$x3 + dat$x4 + dat$x3 * dat$x4 + dat$e) > 0.5)
mod <- glm(y ~ x1 + x2 + x3 * x4, data = dat, family = binomial)
res <- marginaleffects(mod)
mar <- margins(mod, unit_ses = TRUE)
expect_true(expect_margins(res, mar, tolerance = 0.1, verbose=TRUE))


# predictions
pre <- predictions(mod)
expect_predictions(pre)


# emmeans comparison
# type = "response" works at lower tolerance
em <- emmeans::emtrends(mod, ~x2, var = "x2", at = list(x1 = 0, x2 = 0, x3 = 0, x4 = 0))
em <- tidy(em)
mfx <- marginaleffects(mod, newdata = datagrid(x1 = 0, x2 = 0, x3 = 0, x4 = 0), variable = "x2", type = "link")
expect_equivalent(mfx$dydx, em$x2.trend)
expect_equivalent(mfx$std.error, em$std.error)



# glm vs. Stata: marginaleffects
stata <- readRDS(testing_path("stata/stata.rds"))[["stats_glm_01"]]
dat <- read.csv(testing_path("stata/databases/stats_glm_01.csv"))
mod <- glm(y ~ x1 * x2, family = binomial, data = dat)
ame <- merge(tidy(marginaleffects(mod)), stata)
expect_equivalent(ame$dydx, ame$dydxstata)
expect_equivalent(ame$std.error, ame$std.errorstata, tolerance = 0.0001)



# lm vs. Stata: marginaleffects
stata <- readRDS(testing_path("stata/stata.rds"))[["stats_lm_01"]]
dat <- read.csv(testing_path("stata/databases/stats_lm_01.csv"))
mod <- lm(y ~ x1 * x2, data = dat)
ame <- merge(tidy(marginaleffects(mod)), stata)
expect_equivalent(ame$dydx, ame$dydxstata)
expect_equivalent(ame$std.error, ame$std.errorstata)



# lm with interactions vs. margins vs. emmeans: marginaleffects
counterfactuals <- expand.grid(hp = 100, am = 0:1)
mod <- lm(mpg ~ hp * am, data = mtcars)
res <- marginaleffects(mod, variable = "hp", newdata = counterfactuals)
mar <- margins(mod, variable = "hp", data = counterfactuals, unit_ses = TRUE)
expect_true(expect_margins(res, mar, tolerance = 1e-3))

# emmeans
void <- capture.output({
    em1 <- suppressMessages(emmeans::emtrends(mod, ~hp, var = "hp", at = list(hp = 100, am = 0)))
    em2 <- suppressMessages(emmeans::emtrends(mod, ~hp, var = "hp", at = list(hp = 100, am = 1)))
    em1 <- tidy(em1)
    em2 <- tidy(em2)
})

res <- marginaleffects(mod, variable = "hp", newdata = counterfactuals)
expect_equivalent(res$dydx[1], em1$hp.trend)
expect_equivalent(res$std.error[1], em1$std.error, tolerance = .001)
expect_equivalent(res$dydx[2], em2$hp.trend)
expect_equivalent(res$std.error[2], em2$std.error, tolerance = .0001)



# lm vs. emmeans: marginalmeans
dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + cyl + am, data = dat)
mm <- tidy(marginalmeans(mod, variables = "cyl"))
em <- broom::tidy(emmeans::emmeans(mod, specs = "cyl"))
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$std.error)
mm <- tidy(marginalmeans(mod, variables = "am"))
em <- broom::tidy(emmeans::emmeans(mod, specs = "am"))
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$std.error)



# factors seem to behave differently in model.matrix
#skip_if_not_installed("emmeans", minimum_version = "1.7.3")
dat <- guerry
dat$binary <- dat$Crime_prop > median(dat$Crime_prop)
# character variables sometimes break the order
mod <- glm(binary ~ Region + MainCity + Commerce, data = dat, family = "binomial")

# factor variables are safer
dat$Region <- as.factor(dat$Region)
dat$MainCity <- as.factor(dat$MainCity)
mod <- glm(binary ~ Region + MainCity + Commerce, data = dat, family = "binomial")

mm <- marginalmeans(mod, type = "link", variables = "Region")
em <- data.frame(emmeans::emmeans(mod, specs = "Region"))
expect_equivalent(as.character(mm$value), as.character(em$Region))
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$SE, tol = 0.001)

mm <- marginalmeans(mod, type = "link", variables = "MainCity")
em <- data.frame(emmeans::emmeans(mod, specs = "MainCity"))
expect_equivalent(as.character(mm$value), as.character(em$MainCity))
expect_equivalent(mm$estimate, em$estimate)
expect_equivalent(mm$std.error, em$SE, tol = 0.001)

mm <- marginalmeans(mod, variables = "MainCity")
em <- data.frame(emmeans(mod, specs = "MainCity", type = "response"))
expect_equivalent(as.character(mm$value), as.character(em$MainCity))
expect_equivalent(mm$marginalmean, em$prob, tolerance = .01)
expect_equivalent(mm$std.error, em$std.error, tolerance = .01)
expect_equivalent(mm$conf.low, em$asymp.LCL, tolerance = .01)
expect_equivalent(mm$conf.high, em$asymp.UCL, tolerance = .01)



###################################################
#  note sure if stats::loess should be supported  #
###################################################

# vcov(loess) does not exist
mod <- loess(mpg ~ wt, data = mtcars)
expect_warning(marginaleffects(mod), pattern = "Unable")


# loess vs. margins
mod <- loess(mpg ~ wt, data = mtcars)
res <- marginaleffects(mod, vcov = FALSE, newdata = head(mtcars))$dydx
mar <- data.frame(margins(mod, data = head(mtcars)))$dydx_wt
expect_equivalent(as.numeric(res), as.numeric(mar), tolerance = 1e-3)


# loess predictions
mod <- loess(mpg ~ wt, data = mtcars)
expect_warning(predictions(mod))
pred <- predictions(mod, vcov = FALSE)
expect_predictions(pred, se = FALSE)

