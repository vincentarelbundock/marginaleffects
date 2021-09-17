requiet("glmx")
requiet("MASS")

test_that("glmx: no validity check", {
    d <- data.frame(x = runif(200, -1, 1))
    d$y <- rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
    model <- glmx(y ~ x, data = d, family = negative.binomial, xlink = "log", xstart = 0)
    expect_mfx(model)
})
