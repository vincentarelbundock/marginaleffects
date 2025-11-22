testthat::skip_if_not_installed("glmmTMB")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")

ON_CI <- Sys.getenv("ON_CI") == "true"
testthat::skip_if(ON_CI, "on ci") # install and test fails on Github
requiet("glmmTMB")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- glmmTMB::glmmTMB(mpg ~ wt + am + (1 | cyl), data = mtcars)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

data("Owls", package = "glmmTMB")

# marginaleffects no validity
Owls <- transform(
    Owls,
    Nest = reorder(Nest, NegPerChick),
    NCalls = SiblingNegotiation,
    FT = FoodTreatment
)

m0 <- glmmTMB(
    NCalls ~ (FT + ArrivalTime) * SexParent + offset(log(BroodSize)) + (1 | Nest),
    data = Owls,
    ziformula = ~1,
    family = poisson
)
expect_slopes2(m0, re.form = NA)

m1 <- glmmTMB(
    count ~ mined + (1 | site),
    zi = ~mined,
    family = poisson,
    data = Salamanders
)
expect_slopes2(m1, re.form = NA)

# Binomial model
testthat::skip_if_not_installed("lme4")
data(cbpp, package = "lme4")
m4 <- glmmTMB(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial,
    data = cbpp
)
expect_slopes2(m4, re.form = NA)

# comparisons vs. emmeans

# Zero-inflated negative binomial model
m2 <- glmmTMB(
    count ~ spp + mined + (1 | site),
    zi = ~ spp + mined,
    family = nbinom2,
    data = Salamanders
)

co <- comparisons(
    m2,
    type = "link",
    variables = "mined",
    re.form = NA,
    newdata = datagrid(
        mined = "no",
        spp = "GP",
        site = "VF-1"
    )
)
em <- tidy(pairs(emmeans(m2, "mined", at = list(spp = "GP", site = "VF-1"))))
expect_slopes2(m2)
expect_equal(co$estimate, -1 * em$estimate, ignore_attr = TRUE)
expect_equal(co$std.error, em$std.error, tolerance = 1e-6, ignore_attr = TRUE)


# Issue reported by email by Olivier Baumais
bug <- glmmTMB(
    count ~ spp + mined,
    ziformula = ~ spp + mined,
    family = "nbinom2",
    data = Salamanders
)
mfx <- slopes(bug, re.form = NA)
tid1 <- comparisons(bug, comparison = "dydxavg", re.form = NA)
tid2 <- avg_slopes(bug, re.form = NA)

expect_equal(tid1$estimate, tid2$estimate, ignore_attr = TRUE)
expect_equal(tid1$std.error, tid2$std.error, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(tid1$statistic, tid2$statistic, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(tid1$p.value, tid2$p.value, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(length(unique(abs(tid1$statistic))), 7, ignore_attr = TRUE)

bed <- marginaleffects:::modelarchive_data("new_bedford")
mzip_3 <- glmmTMB(
    x ~ cfp + c1 + pfp,
    ziformula = ~ res + inc + age,
    family = "nbinom2",
    data = bed
)
pred <- suppressWarnings(predictions(mzip_3, re.form = NA))
expect_s3_class(pred, "predictions")
