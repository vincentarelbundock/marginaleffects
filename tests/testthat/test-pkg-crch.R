skip_if_not_installed("crch")
skip_if_not_installed("ordinal")
requiet("crch")
requiet("ordinal")

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/crch/RainIbk.csv")
dat$sqrtensmean <- apply(sqrt(dat[,grep('^rainfc',names(dat))]), 1, mean)
dat$sqrtenssd <- apply(sqrt(dat[,grep('^rainfc',names(dat))]), 1, sd)
dat$enssd <- apply(dat[,grep('^rainfc',names(dat))], 1, sd)
dat$ensmean <- apply(dat[,grep('^rainfc',names(dat))], 1, mean)
dat <- subset(dat, enssd > 0)

test_that("marginalmeans: crch gaussian: no validity", {
    model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd, data = dat, dist = "gaussian")
    expect_marginaleffects(model, n_unique = 1, type = "location")
})

test_that("logistic: no validity", {
    model <- crch(sqrt(rain) ~ sqrtensmean | sqrtenssd, data = dat, dist = "logistic", left = 0)
    expect_marginaleffects(model, type = "location", n_unique = 1)

    mfx <- marginaleffects(model, type = "location", variables = "sqrtensmean")
    expect_true(!any(mfx$dydx == 0))

    mfx <- marginaleffects(model, type = "location", variables = "sqrtenssd")
    expect_true(all(mfx$dydx == 0))

    mfx <- marginaleffects(model, type = "scale", variables = "sqrtensmean")
    expect_true(all(mfx$dydx == 0))

    mfx <- marginaleffects(model, type = "scale", variables = "sqrtenssd")
    expect_true(!any(mfx$dydx == 0))
})

test_that("hlxr: no validity", {
    skip("works interactively")
    q <- unique(quantile(dat$rain, seq(0.1, 0.9, 0.1)))
    dat$rain_sqrt <- sqrt(dat$rain)
    mod <- hxlr(rain_sqrt ~ sqrtensmean, data = dat, thresholds = sqrt(q))
    expect_marginaleffects(mod, type = "location", n_unique = 1)
})

test_that("predictions: crch gaussian: no validity", {
    model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd, data = dat, dist = "gaussian")
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(dat))
    expect_predictions(pred1, n_row = 1, se = FALSE)
    expect_predictions(pred2, n_row = 6, se = FALSE)
})


test_that("marginalmeans: crch gaussian: no validity", {
    tmp <- dat
    tmp$categ <- as.factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
    model <- crch(sqrt(rain) ~ sqrtensmean + sqrtenssd + categ, 
                  data = tmp, dist = "gaussian")
    mm <- marginalmeans(model)
    expect_marginalmeans(mm, n_row = 5, se = FALSE)
})
