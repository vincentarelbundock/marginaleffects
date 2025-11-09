source("helpers.R")
using("marginaleffects")
requiet("margins")
requiet("broom")
requiet("emmeans")
requiet("dplyr")


options(marginaleffects_numDeriv = list(method = "simple", method.args = list(eps = 1e-7)))

# Basic expectation tests
mod_simple <- lm(mpg ~ wt + am, data = mtcars)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

guerry <- get_dataset("Guerry", "HistData")

# glm: marginaleffects
set.seed(1024)
N <- 1e2
dat <- data.frame(x1 = rnorm(N), x2 = rnorm(N), x3 = rnorm(N), x4 = rnorm(N), e = rnorm(N)) |>
    transform(y = plogis(x1 + x2 + x3 + x4 + x4 * x4)) |>
    transform(y = rbinom(N, 1, y))
mod <- glm(y ~ x1 + x2 + x3 * x4, data = dat, family = binomial)
res <- slopes(mod, eps = 1e-7)
mar <- margins(mod, unit_ses = TRUE, eps = 1e-7)
# TODO: bad tolerance?
for (x in c("x1", "x2", "x3", "x4")) {
    expect_equivalent(
        as.numeric(res[res$term == x, "estimate"]),
        as.numeric(mar[[paste0("dydx_", x)]]),
        tolerance = 3e-2
    )
    expect_equivalent(
        as.numeric(res[res$term == x, "std.error"]),
        as.numeric(mar[[paste0("SE_dydx_", x)]]),
        tolerance = 4e-2
    )
}


# predictions
expect_predictions(mod, se = FALSE)


# emmeans comparison
# type = "response" works at lower tolerance
em <- emmeans::emtrends(mod, ~x2, var = "x2", at = list(x1 = 0, x2 = 0, x3 = 0, x4 = 0))
em <- tidy(em)
mfx <- slopes(mod, newdata = datagrid(x1 = 0, x2 = 0, x3 = 0, x4 = 0), variable = "x2", type = "link")
expect_equivalent(mfx$estimate, em$x2.trend)
expect_equivalent(mfx$std.error, em$std.error, tolerance = 1e-5)


# glm vs. Stata: marginaleffects
stata <- readRDS(testing_path("stata/stata.rds"))[["stats_glm_01"]]
dat <- read.csv(testing_path("stata/databases/stats_glm_01.csv"))
mod <- glm(y ~ x1 * x2, family = binomial, data = dat)
ame <- merge(avg_slopes(mod, eps = 1e-4), stata)
expect_equivalent(ame$estimate, ame$dydxstata, tolerance = 1e-4)
expect_equivalent(ame$std.error, ame$std.errorstata, tolerance = 1e-4)


# lm vs. Stata: marginaleffects
stata <- readRDS(testing_path("stata/stata.rds"))[["stats_lm_01"]]
dat <- read.csv(testing_path("stata/databases/stats_lm_01.csv"))
mod <- lm(y ~ x1 * x2, data = dat)
ame <- merge(avg_slopes(mod, eps = 1e-4), stata)
expect_equivalent(ame$estimate, ame$dydxstata, tolerance = 1e-4)
expect_equivalent(ame$std.error, ame$std.errorstata, tolerance = 1e-4)


# lm with interactions vs. margins vs. emmeans: marginaleffects
counterfactuals <- expand.grid(hp = 100, am = 0:1)
mod <- lm(mpg ~ hp * am, data = mtcars)
res <- slopes(mod, variable = "hp", newdata = counterfactuals)
mar <- margins(mod, variable = "hp", data = counterfactuals, unit_ses = TRUE)
expect_true(expect_margins(res, mar, tolerance = 1e-3))

# emmeans
void <- capture.output({
    em1 <- suppressMessages(emmeans::emtrends(mod, ~hp, var = "hp", at = list(hp = 100, am = 0)))
    em2 <- suppressMessages(emmeans::emtrends(mod, ~hp, var = "hp", at = list(hp = 100, am = 1)))
    em1 <- tidy(em1)
    em2 <- tidy(em2)
})

res <- slopes(mod, variable = "hp", newdata = counterfactuals)
expect_equivalent(res$estimate[1], em1$hp.trend)
expect_equivalent(res$std.error[1], em1$std.error, tolerance = .001)
expect_equivalent(res$estimate[2], em2$hp.trend)
expect_equivalent(res$std.error[2], em2$std.error, tolerance = .001)


# lm vs. emmeans: marginalmeans
dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + cyl + am, data = dat)
mm <- predictions(mod,
    by = "cyl",
    newdata = datagrid(grid_type = "balanced", FUN_integer = mean)) |>
    dplyr::arrange(cyl)
em <- broom::tidy(emmeans::emmeans(mod, specs = "cyl"))
expect_equivalent(mm$estimate, em$estimate, tolerance = 1e-6)
expect_equivalent(mm$std.error, em$std.error, tolerance = 1e-6)
mm <- predictions(mod,
    by = "am",
    newdata = datagrid(grid_type = "balanced", FUN_integer = mean)) |>
    dplyr::arrange(am)
em <- broom::tidy(emmeans::emmeans(mod, specs = "am"))
expect_equivalent(mm$estimate, em$estimate, tolerance = 1e-6)
expect_equivalent(mm$std.error, em$std.error, tolerance = 1e-5)


# factors seem to behave differently in model.matrix
# skip_if_not_installed("emmeans", minimum_version = "1.7.3")
dat <- guerry
dat$binary <- dat$Crime_prop > median(dat$Crime_prop)
# character variables sometimes break the order
mod <- glm(binary ~ Region + MainCity + Commerce, data = dat, family = "binomial")

# factor variables are safer
dat$Region <- as.factor(dat$Region)
dat$MainCity <- as.factor(dat$MainCity)
mod <- glm(binary ~ Region + MainCity + Commerce, data = dat, family = "binomial")

mm <- predictions(mod, type = "link", by = "Region", newdata = datagrid(grid_type = "balanced")) |>
    dplyr::arrange(Region)
em <- data.frame(emmeans::emmeans(mod, specs = "Region"))
expect_equivalent(as.character(mm$Region), as.character(em$Region))
expect_equivalent(mm$estimate, em$emmean, tol = 0.05) # not sure why tolerance is not good
expect_equivalent(mm$std.error, em$SE, tol = 0.001)

mm <- predictions(mod, type = "link", newdata = datagrid(grid_type = "balanced"), by = "MainCity") |>
    dplyr::arrange(MainCity)
em <- data.frame(emmeans::emmeans(mod, specs = "MainCity", type = "link"))
expect_equivalent(as.character(mm$MainCity), as.character(em$MainCity))
expect_equivalent(mm$estimate, em$emmean, tol = 0.01) # not sure why tolerance is not good
expect_equivalent(mm$std.error, em$SE, tol = 0.001)


mm <- predictions(
    mod,
    type = "link",
    by = "MainCity",
    newdata = datagrid(grid_type = "balanced"),
    transform = plogis
) |>
    dplyr::arrange(MainCity)
em <- data.frame(emmeans(mod, specs = "MainCity", type = "response"))
expect_equivalent(as.character(mm$MainCity), as.character(em$MainCity))
expect_equivalent(mm$estimate, em$prob, tolerance = .01)
expect_equivalent(mm$conf.low, em$asymp.LCL, tolerance = .01)
expect_equivalent(mm$conf.high, em$asymp.UCL, tolerance = .01)


###################################################
#  note sure if stats::loess should be supported  #
###################################################

# vcov(loess) does not exist
mod <- loess(mpg ~ wt + hp, data = mtcars)
# expect_warning(comparisons(mod), pattern = "Unable")
# expect_warning(slopes(mod), pattern = "Unable")

# loess vs. margins
mod <- loess(mpg ~ wt, data = mtcars)
res <- slopes(mod, vcov = FALSE, newdata = head(mtcars))$estimate
mar <- data.frame(margins(mod, data = head(mtcars)))$dydx_wt
expect_equivalent(as.numeric(res), as.numeric(mar), tolerance = 1e-3)


# loess predictions
mod <- loess(mpg ~ wt, data = mtcars)
expect_warning(predictions(mod))
pred <- predictions(mod, vcov = FALSE)
expect_predictions(mod, vcov = FALSE, se = FALSE)


# Issue #548: mlm support
mod <- lm(cbind(mpg, cyl) ~ disp + am, data = mtcars)
tid <- avg_slopes(mod)
expect_inherits(tid, "marginaleffects")
expect_equivalent(nrow(tid), 4)


# Issue #547: standardize column order
mod <- lm(cbind(mpg, cyl) ~ disp + am, data = mtcars)
expect_equivalent(colnames(get_predict(mod)), c("group", "estimate"))

mod <- lm(mpg ~ disp + am, data = mtcars)
expect_equivalent(colnames(get_predict(mod)), c("estimate"))


# Issue #833: Support nls() no validity
DNase1 <- subset(datasets::DNase, Run == 1)
mod <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), DNase1)
cmp <- avg_comparisons(mod, variables = "conc")
expect_inherits(cmp, "comparisons")
expect_false(any(is.na(cmp$estimate)))
expect_false(any(is.na(cmp$std.error)))
