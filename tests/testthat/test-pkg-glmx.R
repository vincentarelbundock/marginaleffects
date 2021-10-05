requiet("glmx")
requiet("MASS")

test_that("marginaleffects: glmx: no validity check", {
    d <- data.frame(x = runif(200, -1, 1))
    d$y <- rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
    model <- glmx(y ~ x, data = d, family = negative.binomial, 
                  xlink = "log", xstart = 0)
    expect_marginaleffects(model)
})

test_that("predictions: glmx: no validity check", {
    d <- data.frame(x = runif(200, -1, 1))
    d$y <- rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
    model <- glmx(y ~ x, data = d, family = negative.binomial,
                  xlink = "log", xstart = 0)
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(d))
    expect_predictions(pred1, n_row = 1, se = FALSE)
    expect_predictions(pred2, n_row = 6, se = FALSE)
})
