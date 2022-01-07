# TODO: emtrends not clear what it computes for polr
requiet("margins")
requiet("MASS")
requiet("emmeans")
requiet("broom")

### marginaleffects
test_that("rlm: marginaleffects: vs. margins vs. emmeans", {
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    expect_marginaleffects(model, n_unique = 1)

    # margins
    mfx <- tidy(marginaleffects(model))
    mar <- tidy(margins(model))
    expect_equal(mfx$estimate, mar$estimate, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mar$std.error, ignore_attr = TRUE, tolerance = .00001)

    # emmeans
    mfx <- marginaleffects(model, newdata = datagrid(drat = 3.9, hp = 110))
    em1 <- emmeans::emtrends(model, ~hp, "hp", at = list(hp = 110, drat = 3.9))
    em2 <- emmeans::emtrends(model, ~drat, "drat", at = list(hp = 110, drat = 3.9))
    em1 <- tidy(em1)
    em2 <- tidy(em2)
    expect_equal(mfx$dydx[1], em1$hp.trend)
    expect_equal(mfx$std.error[1], em1$std.error, tolerance = .001)
    expect_equal(mfx$dydx[2], em2$drat.trend)
    expect_equal(mfx$std.error[2], em2$std.error, tolerance = .002)
})

test_that("glm.nb: marginaleffects: vs. margins vs. emmeans", {

    # margins does not support unit-level standard errors
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- marginaleffects(model)
    mar <- margins(model)
    expect_equal(mfx$dydx[mfx$term == "wt"], mar$dydx_wt, tolerance = .0001, ignore_attr = TRUE)
    expect_equal(mfx$dydx[mfx$contrast == "6 - 4"], mar$dydx_cyl6, tolerance = .0001, ignore_attr = TRUE)
    expect_equal(mfx$dydx[mfx$contrast == "8 - 4"], mar$dydx_cyl8, tolerance = .0001, ignore_attr = TRUE)

    mfx <- marginaleffects(model)
    mar <- margins(model)

    # margins: standard errors at mean gradient
    mfx <- tidy(mfx)
    mar <- tidy(mar)
    expect_equal(mfx$estimate, mar$estimate, ignore_attr = TRUE, tolerance = .0001)
    expect_equal(mfx$std.error, mar$std.error, ignore_attr = TRUE, tolerance = .001)

    # emmeans::emtrends
    mfx <- marginaleffects(model, newdata = datagrid(wt = 2.6, cyl = 4), type = "link")
    em <- emtrends(model, ~wt, "wt", at = list(wt = 2.6, cyl = 4))
    em <- tidy(em)
    expect_equal(mfx$dydx[1], em$wt.trend)
    expect_equal(mfx$std.error[1], em$std.error)

    # emmeans contrasts
    mfx <- marginaleffects(model, type = "link", newdata = datagrid(wt = 3, cyl = 4))
    em <- emmeans(model, specs = "cyl") 
    em <- contrast(em, method = "revpairwise", at = list(wt = 3, cyl = 4))
    em <- tidy(em)
    expect_equal(mfx$dydx[mfx$contrast == "6 - 4"], em$estimate[em$contrast == "6 - 4"])
    expect_equal(mfx$std.error[mfx$contrast == "6 - 4"], em$std.error[em$contrast == "6 - 4"])
    expect_equal(mfx$dydx[mfx$contrast == "8 - 4"], em$estimate[em$contrast == "8 - 4"])
    expect_equal(mfx$std.error[mfx$contrast == "8 - 4"], em$std.error[em$contrast == "8 - 4"])

})

test_that("glm.nb: marginaleffects: vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))$mass_glm_nb
    model <- suppressWarnings(
        MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    mfx <- tidy(marginaleffects(model))
    stata$contrast <- ifelse(stata$term == "factor(cyl)6", "6 - 4", "")
    stata$contrast <- ifelse(stata$term == "factor(cyl)8", "8 - 4", stata$contrast)
    stata$term <- ifelse(grepl("cyl", stata$term), "cyl", stata$term)
    mfx <- merge(mfx, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .0001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
})

test_that("polr: marginaleffects: vs. Stata", {
    # Hess=TRUE otherwise breaks in the test environment via MASS:::vcov() -> update()
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    mod <- MASS::polr(factor(y) ~ x1 + x2, data = dat, Hess = TRUE)
    mfx <- marginaleffects(mod, type = "probs")
    mfx <- tidy(mfx)
    mfx <- merge(mfx, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
    expect_marginaleffects(mod, type = "probs")
})

test_that("bugs stay dead: polr with 1 row newdata", {
    # Hess=TRUE otherwise breaks in the test environment via MASS:::vcov() -> update()
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    dat$y <- factor(dat$y)
    mod <- MASS::polr(y ~ x1, data = dat, Hess = TRUE)
    mfx <- marginaleffects(mod, type = "probs", newdata = datagrid(x1 = 0))
    expect_s3_class(mfx, "marginaleffects")
})

test_that("marginaleffects vs. emmeans", {
    skip_if_not_installed("emmeans", minimum_version = "1.7.1.9")
    # Hess=TRUE otherwise breaks in the test environment via MASS:::vcov() -> update()
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    dat$y <- factor(dat$y)
    mod <- MASS::polr(y ~ x1 + x2, data = dat, Hess = TRUE)
    em <- emmeans::emtrends(mod, ~y, var = "x1", mode = "prob", at = list(x1 = 0, x2 = 0))
    em <- tidy(em)
    mfx <- marginaleffects(mod, newdata = datagrid(x1 = 0, x2 = 0), 
                           type = "probs", variables = "x1")
    expect_equal(mfx$dydx, em$x1.trend, tolerance = .01)
    expect_equal(mfx$std.error, em$std.error, tolerance = .01)
})

### predictions

test_that("polr: predictions: no validity", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    mod <- MASS::polr(factor(gear) ~ mpg + factor(cyl), data = mtcars)
    pred <- predictions(mod, type = "probs")
    expect_predictions(pred)
    # bugs stay dead
    expect_true(all(c("rowid", "type", "predicted", "std.error", "group") %in% colnames(pred)))
})

test_that("glm.nb: predictions: no validity", {
    model <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
    pred <- predictions(model)
    expect_predictions(pred)
})

test_that("rlm: predictions: no validity", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    model <- MASS::rlm(mpg ~ hp + drat, mtcars)
    pred <- predictions(model)
    expect_predictions(pred, n_row = nrow(mtcars))
    pred <- predictions(model, newdata = head(mtcars))
    expect_predictions(pred, n_row = 6)
})


### marginalmeans

test_that("glm.nb: marginalmeans: vs. emmeans", {
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    model <- suppressWarnings(MASS::glm.nb(carb ~ am + cyl, data = dat))
    mm <- marginalmeans(model, type = "link", variables = "cyl")
    ti <- tidy(mm)
    em <- tidy(emmeans::emmeans(model, "cyl"))
    expect_marginalmeans(mm, se = TRUE)
    expect_equal(ti$estimate, em$estimate)
    expect_equal(ti$std.error, em$std.error)
})


test_that("rlm: marginalmeans: vs. emmeans", {
    skip_if_not_installed("insight", minimum_version = "0.14.4.1")
    dat <- mtcars
    dat$cyl <- as.factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    model <- MASS::rlm(mpg ~ cyl + am, dat)
    mm <- marginalmeans(model)
    expect_marginalmeans(mm, se = TRUE)
    ti <- tidy(marginalmeans(model, variables = "cyl"))
    em <- tidy(emmeans::emmeans(model, "cyl"))
    expect_equal(ti$estimate, em$estimate)
    expect_equal(ti$std.error, em$std.error)
})


test_that("polr: marginalmeans vs. emmeans", {
    skip("works interactively")
    tmp <- mtcars
    tmp$vs <- as.factor(tmp$vs)
    tmp$am <- as.logical(tmp$am)
    mod <- suppressWarnings(MASS::polr(factor(gear) ~ vs + am, data = tmp))
    # TODO: emmeans seems broken at the moment
    # em <- emmeans(mod, specs = "am", transform = "response")
    # em <- tidy(em)
    mm <- marginalmeans(mod, variables = "am", type = "probs")
    expect_equal(nrow(mm), 6)
})


# glmmPQL

test_that("glmmPQL: no validity", {
    tmp <- bacteria
    tmp$week_bin <- tmp$week > 2
    mod <- glmmPQL(y ~ trt + week_bin, random = ~ 1 | ID,
                   family = binomial,
                   verbose = FALSE,
                   data = tmp)
    expect_marginaleffects(mod, type = "link", n_unique = 1)
    expect_marginaleffects(mod, type = "response")
    expect_predictions(predictions(mod))

    # emtrends
    em <- emmeans::emtrends(mod, ~week_bin, "week_bin", at = list(week_bin = 0))
    em <- tidy(em)
    mfx <- marginaleffects(mod, newdata = datagrid(week_bin = 0), type = "link")
    expect_equal(mfx$dydx[3], em$week_bin.trend)
    expect_equal(mfx$std.error[3], em$std.error, tolerance = .01)
})
