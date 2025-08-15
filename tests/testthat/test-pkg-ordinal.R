test_that("ordinal package: clm vs MASS comparison", {
    skip_on_cran()
    skip_if_not_installed("MASS")
    skip_if_not_installed("ordinal")

    withr_library("MASS")
    withr_library("ordinal")

    dat <- get_dataset("housing", "MASS")
    for (i in seq_along(dat)) {
        if (is.character(dat[[i]])) {
            dat[[i]] <- factor(dat[[i]])
        }
    }

    # marginaleffects: clm: vs. MASS
    known <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = dat, Hess = TRUE)

    known <- suppressMessages(avg_slopes(known, type = "probs"))
    unknown <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = dat)
    unknown <- avg_slopes(unknown)
    expect_equal(unknown$estimate, known$estimate, tolerance = .00001, ignore_attr = TRUE)
    expect_equal(unknown$std.error, known$std.error, tolerance = .00001, ignore_attr = TRUE)
})

test_that("ordinal package: clm vs Stata comparison", {
    skip_on_cran()
    skip_if_not_installed("ordinal")

    withr_library("ordinal")

    # marginaleffects: protect against corner cases
    # do not convert numeric to factor in formula
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    mod <- ordinal::clm(factor(y) ~ x1 + x2, data = dat)
    expect_error(slopes(mod), pattern = "Please convert the variable to factor")

    # marginaleffects: clm: vs. Stata
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    dat$y <- factor(dat$y)
    mod <- ordinal::clm(y ~ x1 + x2, data = dat)
    mfx <- avg_slopes(mod)
    mfx <- merge(mfx, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001, ignore_attr = TRUE)
    slo <- slopes(mod)
    expect_s3_class(slo, "slopes")
})

test_that("ordinal package: predictions functionality", {
    skip_on_cran()
    skip_if_not_installed("ordinal")

    withr_library("ordinal")

    # Issue 717: no validity
    data("wine", package = "ordinal")
    mod <- clm(rating ~ contact + temp, data = wine)
    p <- predictions(mod, type = "linear.predictor")
    expect_s3_class(p, "predictions")
    p <- predictions(mod, type = "cum.prob")
    expect_s3_class(p, "predictions")
    expect_error(predictions(mod, type = "junk"), pattern = "Assertion")
    p <- avg_slopes(mod, type = "cum.prob")
    expect_s3_class(p, "slopes")
})

test_that("ordinal package: different link functions", {
    skip_on_cran()
    skip_if_not_installed("ordinal")

    withr_library("ordinal")

    # marginaleffects: clm: no validity
    tmp <- get_dataset("soup", "ordinal")
    tab26 <- with(tmp, table("Product" = PROD, "Response" = SURENESS))
    dimnames(tab26)[[2]] <- c("Sure", "Not Sure", "Guess", "Guess", "Not Sure", "Sure")
    dat26 <- expand.grid(sureness = as.factor(1:6), prod = c("Ref", "Test"))
    dat26$wghts <- c(t(tab26))
    m1 <- clm(sureness ~ prod, scale = ~prod, data = dat26, weights = wghts, link = "logit")
    m2 <- update(m1, link = "probit")
    m3 <- update(m1, link = "cloglog")
    m4 <- update(m1, link = "loglog")
    m5 <- update(m1, link = "cauchit", start = coef(m1))
    slo1 <- slopes(m1)
    expect_s3_class(slo1, "slopes")
    slo2 <- slopes(m2)
    expect_s3_class(slo2, "slopes")
    slo3 <- slopes(m3)
    expect_s3_class(slo3, "slopes")
    slo4 <- slopes(m4)
    expect_s3_class(slo4, "slopes")
    slo5 <- slopes(m5)
    expect_s3_class(slo5, "slopes")
})

test_that("ordinal package: scale and location effects", {
    skip_on_cran()
    skip_if_not_installed("ordinal")

    withr_library("ordinal")

    # Issue #718: incorrect standard errors when scale and location are the same
    dat <- transform(mtcars, cyl = factor(cyl), vs2 = vs)
    mod1 <- clm(
        cyl ~ hp + vs, # vs has a location effect
        scale = ~vs, # vs also has a scale effect
        data = dat
    )
    mod2 <- clm(
        cyl ~ hp + vs, # vs has a location effect
        scale = ~vs2, # vs also has a scale effect
        data = dat
    )
    nd <- subset(dat, select = -cyl)
    pre1 <- predictions(mod1)
    pre2 <- predictions(mod2)
    pre3 <- predict(mod1, newdata = nd, type = "prob", se.fit = TRUE)
    expect_equal(pre1$estimate, pre2$estimate, ignore_attr = TRUE)
    expect_equal(pre1$std.error, pre2$std.error, ignore_attr = TRUE)
    expect_equal(subset(pre1, group == 4)$estimate, pre3$fit[, 1], ignore_attr = TRUE)
    expect_equal(subset(pre1, group == 4)$std.error, pre3$se.fit[, 1], tolerance = 1e-4, ignore_attr = TRUE)

    # Issue #718: incorrect
    dat <- transform(mtcars, cyl = factor(cyl))
    mod <- suppressWarnings(clm(cyl ~ vs + carb, scale = ~vs, nominal = ~carb, data = dat))
    dat$cyl <- NULL
    p1 <- predictions(mod)
    p2 <- suppressWarnings(predict(mod, newdata = dat, se.fit = TRUE))
    expect_equal(subset(p1, group == 4)$estimate, p2$fit[, 1], ignore_attr = TRUE)
    expect_equal(subset(p1, group == 4)$std.error, p2$se.fit[, 1], tolerance = 1e4, ignore_attr = TRUE)
    expect_equal(subset(p1, group == 6)$estimate, p2$fit[, 2], ignore_attr = TRUE)
    expect_equal(subset(p1, group == 6)$std.error, p2$se.fit[, 2], tolerance = 1e4, ignore_attr = TRUE)
    expect_equal(subset(p1, group == 8)$estimate, p2$fit[, 3], ignore_attr = TRUE)
    expect_equal(subset(p1, group == 8)$std.error, p2$se.fit[, 3], tolerance = 1e4, ignore_attr = TRUE)
})

test_that("ordinal package: elasticity functionality", {
    skip_on_cran()
    skip_if_not_installed("ordinal")

    withr_library("ordinal")

    # Issue #729
    dat <- transform(
        mtcars,
        cyl = factor(
            cyl,
            levels = c(4, 6, 8),
            labels = c("small", "medium", "large")
        )
    )
    mod <- clm(cyl ~ hp + carb, scale = ~vs, data = dat)
    mfx <- avg_slopes(mod, slope = "eyex")
    expect_s3_class(mfx, "slopes")
    mfx <- avg_slopes(mod, slope = "dyex")
    expect_s3_class(mfx, "slopes")

    # Check elasticities for carb
    mfx1 <- slopes(mod, variables = "carb", slope = "dydx")
    mfx2 <- slopes(mod, variables = "carb", slope = "eyex")
    mfx3 <- slopes(mod, variables = "carb", slope = "eydx")
    mfx4 <- slopes(mod, variables = "carb", slope = "dyex")
    expect_equal(mfx2$estimate, mfx1$estimate * (mfx1$carb / mfx1$predicted), ignore_attr = TRUE)
    expect_equal(mfx3$estimate, mfx1$estimate / mfx1$predicted, ignore_attr = TRUE)
    expect_equal(mfx4$estimate, mfx1$estimate * mfx1$carb, ignore_attr = TRUE)

    # Check elasticities for hp
    mfx1_hp <- slopes(mod, variables = "hp", slope = "dydx")
    mfx2_hp <- slopes(mod, variables = "hp", slope = "eyex")
    mfx3_hp <- slopes(mod, variables = "hp", slope = "eydx")
    mfx4_hp <- slopes(mod, variables = "hp", slope = "dyex")
    expect_equal(mfx2_hp$estimate, mfx1_hp$estimate * (mfx1_hp$hp / mfx1_hp$predicted), ignore_attr = TRUE)
    expect_equal(mfx3_hp$estimate, mfx1_hp$estimate / mfx1_hp$predicted, ignore_attr = TRUE)
    expect_equal(mfx4_hp$estimate, mfx1_hp$estimate * mfx1_hp$hp, ignore_attr = TRUE)
})
