testthat::skip_if_not_installed("phylolm")
requiet("phylolm")

set.seed(123456)
tre = rcoal(60)
taxa = sort(tre$tip.label)
b0 = 0
b1 = 1
x <- rTrait(
    n = 1,
    phy = tre,
    model = "BM",
    parameters = list(ancestral.state = 0, sigma2 = 10)
)
y <- b0 +
    b1 * x +
    rTrait(
        n = 1,
        phy = tre,
        model = "lambda",
        parameters = list(
            ancestral.state = 0,
            sigma2 = 1,
            lambda = 0.5
        )
    )
dat_phylolm <<- data.frame(trait = y[taxa], pred = x[taxa])
fit <- phylolm(trait ~ pred, data = dat_phylolm, phy = tre, model = "lambda")

# Basic expectation tests (simplified for phylolm)
expect_slopes2(fit)
expect_predictions2(fit)
expect_hypotheses2(fit)
expect_comparisons2(fit)

set.seed(123456)
tre = rcoal(60)
taxa = sort(tre$tip.label)
b0 = 0
b1 = 1
x <- rTrait(
    n = 1,
    phy = tre,
    model = "BM",
    parameters = list(ancestral.state = 0, sigma2 = 10)
)
y <- b0 +
    b1 * x +
    rTrait(
        n = 1,
        phy = tre,
        model = "lambda",
        parameters = list(
            ancestral.state = 0,
            sigma2 = 1,
            lambda = 0.5
        )
    )
dat_phylolm <<- data.frame(trait = y[taxa], pred = x[taxa])
fit <- phylolm(trait ~ pred, data = dat_phylolm, phy = tre, model = "lambda")
pre <- predictions(fit)
cmp <- comparisons(fit)
mfx <- avg_slopes(fit)
expect_s3_class(pre, "predictions")
expect_s3_class(cmp, "comparisons")
expect_s3_class(mfx, "slopes")
