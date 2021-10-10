requiet("ordinal")


test_that("marginaleffects: clm: vs. MASS", {
    data(housing, package = "MASS")
    known <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    known <- tidy(marginaleffects(known, type = "probs"))
    unknown <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
    unknown <- tidy(marginaleffects(unknown))
    expect_equal(unknown$estimate, known$estimate, tolerance = .00001)
    expect_equal(unknown$std.error, known$std.error, tolerance = .00001)
})


test_that("marginaleffects: protect against corner cases", { 
    # do not convert numeric to factor in formula
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    mod <- ordinal::clm(factor(y) ~ x1 + x2, data = dat)
    expect_error(marginaleffects(mod), regexp = "Please convert the variable to factor")
})


test_that("marginaleffects: clm: vs. Stata", {
    stata <- readRDS(test_path("stata/stata.rds"))[["MASS_polr_01"]]
    dat <- read.csv(test_path("stata/databases/MASS_polr_01.csv"))
    dat$y <- factor(dat$y)
    mod <- ordinal::clm(y ~ x1 + x2, data = dat)
    mfx <- marginaleffects(mod)
    mfx <- tidy(mfx)
    mfx <- merge(mfx, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .001)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001)
    expect_marginaleffects(mod)
})


test_that("marginaleffects: clm: no validity", {
    data(soup, package = "ordinal")
    (tab26 <- with(soup, table("Product" = PROD, "Response" = SURENESS)))
    dimnames(tab26)[[2]] <- c("Sure", "Not Sure", "Guess", "Guess", "Not Sure", "Sure")
    dat26 <- expand.grid(sureness = as.factor(1:6), prod = c("Ref", "Test"))
    dat26$wghts <- c(t(tab26))
    m1 <- clm(sureness ~ prod, scale = ~prod, data = dat26, weights = wghts, link = "logit")
    m2 <- update(m1, link = "probit")
    m3 <- update(m1, link = "cloglog")
    m4 <- update(m1, link = "loglog")
    m5 <- update(m1, link = "cauchit", start = coef(m1))
    expect_marginaleffects(m1, n_unique = 6)
    expect_marginaleffects(m2, n_unique = 6)
    expect_marginaleffects(m3, n_unique = 6)
    expect_marginaleffects(m4, n_unique = 6)
    expect_marginaleffects(m5, n_unique = 6)
})
