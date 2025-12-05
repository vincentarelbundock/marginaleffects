# TODO: emtrends not clear what it computes for polr
testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("MASS")
testthat::skip_if_not_installed("broom")
testthat::skip_if_not_installed("emmeans")

requiet("margins")
requiet("MASS")
requiet("broom")
requiet("emmeans")
tol <- 0.0001
tol_se <- 0.001

# Basic expectation tests
mod_simple <- MASS::rlm(mpg ~ wt + am, data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

### marginaleffects
# rlm: marginaleffects: vs. margins vs. emmeans
dat <- mtcars
model <- MASS::rlm(mpg ~ hp + drat, dat)
expect_slopes2(model, n_unique = 1)

# margins
mfx <- slopes(model)
mar <- margins(model, unit_ses = TRUE)
expect_margins2(mfx, mar, verbose = TRUE, tolerance = tol_se)

# emmeans
mfx <- slopes(model, newdata = datagrid(drat = 3.9, hp = 110)) |> dplyr::arrange(term)
em1 <- emmeans::emtrends(model, ~hp, "hp", at = list(hp = 110, drat = 3.9))
em2 <- emmeans::emtrends(model, ~drat, "drat", at = list(hp = 110, drat = 3.9))
em1 <- tidy(em1)
em2 <- tidy(em2)
expect_equal(mfx$estimate[2], em1$hp.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error[2], em1$std.error, tolerance = .002, ignore_attr = TRUE)
expect_equal(mfx$estimate[1], em2$drat.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error[1], em2$std.error, tolerance = .002, ignore_attr = TRUE)

# glm.nb: marginaleffects: vs. margins vs. emmeans
model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))

# margins does not support unit-level standard errors
mar <- margins(model)
mfx <- slopes(model)
expect_margins2(mfx, mar, se = FALSE)


# margins: standard errors at mean gradient
mfx_tid <- avg_slopes(model)
mar_tid <- tidy(mar)[, c("term", "estimate", "std.error")]
mar_tid <- setNames(mar_tid, c("term", "mar_estimate", "mar_std.error"))
tmp <- merge(mfx_tid, mar_tid)
expect_equal(tmp$estimate, tmp$mar_estimate, tolerance = .0001, ignore_attr = TRUE)
expect_equal(tmp$std.error, tmp$mar_std.error, tolerance = .001, ignore_attr = TRUE)

# emmeans::emtrends
mfx <- slopes(model, newdata = datagrid(wt = 2.6, cyl = 4), type = "link")
em <- emtrends(model, ~wt, "wt", at = list(wt = 2.6, cyl = 4))
em <- tidy(em)
expect_equal(mfx$estimate[mfx$term == "wt"], em$wt.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error[mfx$term == "wt"], em$std.error, tolerance = 1e-3, ignore_attr = TRUE)

# emmeans contrasts
mfx <- slopes(model, type = "link", newdata = datagrid(wt = 3, cyl = 4))
em <- emmeans(model, specs = "cyl")
em <- emmeans::contrast(em, method = "revpairwise", at = list(wt = 3, cyl = 4))
em <- tidy(em)
expect_equal(mfx$estimate[mfx$contrast == "6 - 4"], em$estimate[em$contrast == "cyl6 - cyl4"], ignore_attr = TRUE)
expect_equal(mfx$std.error[mfx$contrast == "6 - 4"], em$std.error[em$contrast == "cyl6 - cyl4"], tolerance = 1e-4, ignore_attr = TRUE)
expect_equal(mfx$estimate[mfx$contrast == "8 - 4"], em$estimate[em$contrast == "cyl8 - cyl4"], ignore_attr = TRUE)
expect_equal(mfx$std.error[mfx$contrast == "8 - 4"], em$std.error[em$contrast == "cyl8 - cyl4"], tolerance = 1e-4, ignore_attr = TRUE)


# glm.nb: marginaleffects: vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$mass_glm_nb
model <- suppressWarnings(
    MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars)
)
mfx <- avg_slopes(model)
stata$contrast <- ifelse(stata$term == "factor(cyl)6", "6 - 4", "")
stata$contrast <- ifelse(stata$term == "factor(cyl)8", "8 - 4", stata$contrast)
stata$term <- ifelse(grepl("cyl", stata$term), "cyl", stata$term)
mfx <- merge(mfx, stata)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001, ignore_attr = TRUE)

# polr: marginaleffects: vs. Stata
# Hess=TRUE otherwise breaks in the test environment via MASS:::vcov() -> update()
stata <- readRDS(testing_path("stata/stata.rds"))[["MASS_polr_01"]]
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat, Hess = TRUE)
mfx <- avg_slopes(mod, type = "probs")
mfx <- merge(mfx, stata)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .01, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .01, ignore_attr = TRUE)
expect_slopes2(mod, type = "probs")


# bugs stay dead: polr with 1 row newdata
# Hess=TRUE otherwise breaks in the test environment via MASS:::vcov() -> update()
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
dat$y <- factor(dat$y)
mod <- MASS::polr(y ~ x1, data = dat, Hess = TRUE)
mfx <- slopes(mod, type = "probs", newdata = datagrid(x1 = 0))
expect_s3_class(mfx, "marginaleffects")


# marginaleffects vs. emmeans
# skip_if_not_installed("emmeans", minimum_version = "1.7.1.9")
# Hess=TRUE otherwise breaks in the test environment via MASS:::vcov() -> update()
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
dat$y <- factor(dat$y)
mod <- MASS::polr(y ~ x1 + x2, data = dat, Hess = TRUE)
em <- emmeans::emtrends(mod, ~y, var = "x1", mode = "prob", at = list(x1 = 0, x2 = 0))
em <- tidy(em)
mfx <- slopes(mod, newdata = datagrid(x1 = 0, x2 = 0), type = "probs", variables = "x1")
expect_equal(mfx$estimate, em$x1.trend, tolerance = .01, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .01, ignore_attr = TRUE)


### predictions

# polr: predictions: no validity
mod <- MASS::polr(factor(gear) ~ mpg + factor(cyl), data = mtcars, Hess = TRUE)
pred <- suppressMessages(predictions(mod, type = "probs"))
expect_predictions2(mod, type = "probs")
# bugs stay dead
expect_true(all(c("rowid", "estimate", "std.error", "group") %in% colnames(pred)))


# glm.nb: predictions: no validity
model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
pred <- predictions(model)
expect_predictions2(model, se = FALSE)


# rlm: predictions: no validity
model <- MASS::rlm(mpg ~ hp + drat, mtcars)
pred <- predictions(model)
expect_predictions2(model, n_row = nrow(mtcars))
pred <- predictions(model, newdata = head(mtcars))
expect_predictions2(model, newdata = head(mtcars), n_row = 6)

### marginalmeans

# glm.nb: marginalmeans: vs. emmeans
dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
dat <- dat
model <- suppressWarnings(MASS::glm.nb(carb ~ am + cyl, data = dat))
mm <- predictions(model, type = "link", by = "cyl", newdata = datagrid(grid_type = "balanced"))
ti <- mm |> dplyr::arrange(cyl)
em <- tidy(emmeans::emmeans(model, "cyl"))
expect_equal(ti$estimate, em$estimate, ignore_attr = TRUE)
expect_equal(ti$std.error, em$std.error, tolerance = 1e-4, ignore_attr = TRUE)

# rlm: marginalmeans: vs. emmeans
dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
model <- MASS::rlm(mpg ~ cyl + am, dat)
ti <- predictions(model, by = "cyl", newdata = datagrid(grid_type = "balanced")) |> dplyr::arrange(cyl)
em <- tidy(emmeans::emmeans(model, "cyl"))
expect_equal(ti$estimate, em$estimate, ignore_attr = TRUE)
expect_equal(ti$std.error, em$std.error, tolerance = 1e-4, ignore_attr = TRUE)


# glmmPQL
testthat::skip_if_not_installed("lme4")
requiet("lme4") # glmmPQL fails when lme4 is not installed

# glmmPQL: no validity
tmp_glmmPQL <- bacteria
tmp_glmmPQL$week_bin <- tmp_glmmPQL$week > 2
mod <- glmmPQL(
    y ~ trt + week_bin,
    random = ~ 1 | ID,
    family = binomial,
    verbose = FALSE,
    data = tmp_glmmPQL
)
expect_slopes2(mod, type = "link", n_unique = 1)
expect_slopes2(mod, type = "response")
expect_predictions2(mod)

# emtrends
em <- emmeans::emtrends(mod, ~week_bin, "week_bin", at = list(week_bin = 0))
em <- tidy(em)
mfx <- slopes(mod, newdata = datagrid(week_bin = 0), type = "link")
expect_equal(mfx$estimate[3], em$week_bin.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error[3], em$std.error, tolerance = .01, ignore_attr = TRUE)


# bugs stay dead: character regressor with categorical outcome
dat <- mtcars
dat$cyl <- as.character(dat$cyl)
dat <- dat
mod <- polr(factor(gear) ~ cyl, data = dat, Hess = TRUE)
# not clear why this generates a warning only on CI
tid <- suppressMessages(avg_slopes(mod, type = "probs"))
expect_equal(nrow(tid), 6, ignore_attr = TRUE)


# polr: average predictions by group against Stata
mod <- polr(factor(gear) ~ hp, data = mtcars, Hess = TRUE)
p <- suppressMessages(avg_predictions(mod, type = "probs"))
expect_equal(
    p$estimate,
    c(.4933237, .363384, .1432922),
    tolerance = tol
)
expect_equal(
    p$std.error,
    c(.0867256, .0838539, .0591208),
    tolerance = tol_se
)

# Predictive margins                              Number of obs     =         32
# Model VCE    : OIM

# 1._predict   : Pr(gear==1), predict(pr outcome(1))
# 2._predict   : Pr(gear==2), predict(pr outcome(2))
# 3._predict   : Pr(gear==3), predict(pr outcome(3))

# ------------------------------------------------------------------------------
#              |            Delta-method
#              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
# -------------+----------------------------------------------------------------
#     _predict |
#           1  |   .4933237   .0867256     5.69   0.000     .3233448    .6633027
#           2  |    .363384   .0838539     4.33   0.000     .1990335    .5277346
#           3  |   .1432922   .0591208     2.42   0.015     .0274175    .2591669

# polr: marginalmeans vs. emmeans
k <- mtcars
k$vs <- as.factor(k$vs)
k$am <- as.logical(k$am)
mod <- suppressWarnings(MASS::polr(factor(gear) ~ vs + am, data = k, Hess = TRUE))
# TODO: emmeans seems broken at the moment
# em <- emmeans(mod, specs = "am", type = "response")
# em <- tidy(em)
mm <- predictions(mod, by = "am", type = "probs", newdata = datagrid(grid_type = "balanced"))
expect_equal(nrow(mm), 6, ignore_attr = TRUE)


# Issue #896: polr returns mean of binary instead of median
mtcars$gear <- as.factor(mtcars$gear)
mod <- polr(
    gear ~ mpg + cyl + vs,
    data = mtcars,
    method = "probit",
    Hess = TRUE
)
p <- predictions(mod, newdata = "median", type = "probs")
expect_true(all(p$vs == 0))
