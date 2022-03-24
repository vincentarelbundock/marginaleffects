requiet("mclogit")
requiet("MASS")
requiet("emmeans")
requiet("broom")

test_that("mclogit: no validity", {
    data(Transport, package = "mclogit")
    void <- capture.output(
        model <- mclogit(cbind(resp, suburb) ~ distance + cost, data = Transport)
    )
    # type = "response" produces 0 dydx and standard error. Not sure why
    # because `get_predict(newdata)` seems to work
    expect_error(marginaleffects(model, type = "response"), regexp = "type. argument")
    # type = "link" works
    expect_marginaleffects(model, type = "link", n_unique = 1)
    pred <- predictions(model, type = "link")
    expect_predictions(pred)
})

test_that("mblogit: error on character regressors", {
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MASS/housing.csv")
    dat$x <- rnorm(nrow(dat))
    dat$Sat <- factor(dat$Sat)
    void <- capture.output(
        mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat))
    expect_predictions(predictions(mod))
    expect_error(marginaleffects(mod, type = "link"), regexp = "character")
})

test_that("mblogit: works on factor regressors", {
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MASS/housing.csv")
    dat$x <- rnorm(nrow(dat))
    dat$Sat <- factor(dat$Sat)
    dat$Infl <- factor(dat$Infl)
    dat$Cont <- factor(dat$Cont)
    dat$Type <- factor(dat$Type)
    void <- capture.output(
        mod <- mblogit(Sat ~ Infl + Type + Cont + x, weights = Freq, data = dat))
    expect_predictions(predictions(mod))
    mfx <- marginaleffects(mod, type = "link")
    expect_s3_class(mfx, "marginaleffects")
})

test_that("mblogit: marginaleffects vs. emmeans", {
    skip("emmeans does not support `mblogit`")
    mm <- marginalmeans(mod, variables = c("Infl"))
    em <- emmeans(mod, ~Infl)
    em <- tidy(emmeans::emtrends(mod, ~x, "x"))
    mfx <- marginaleffects(mod)
})
