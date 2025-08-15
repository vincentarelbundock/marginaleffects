test_that("interaction functionality works", {
    skip_if_not_installed("ggplot2")

    withr_library("ggplot2")

    data("diamonds", package = "ggplot2")
    dat <- diamonds[1:1000, ]
    dat$cut <- factor(as.character(dat$cut), levels = levels(dat$cut))
    dat$color <- factor(as.character(dat$color), levels = levels(dat$color))
    dat$clarity <- factor(as.character(dat$clarity), levels = levels(dat$clarity))
    mod <- lm(price ~ cut * color + clarity + carat, data = dat)
    cmp1 <- comparisons(mod, variables = c("cut", "color"), cross = TRUE)
    cmp2 <- comparisons(mod, variables = "cut")

    expect_equal(nrow(subset(cmp1, rowid == 1)), 24, ignore_attr = TRUE)
    expect_equal(nrow(subset(cmp2, rowid == 1)), 4, ignore_attr = TRUE)

    n_unique <- nrow(unique(subset(cmp2, rowid == 1, "contrast")))
    expect_equal(n_unique, 4, ignore_attr = TRUE)

    mod <- lm(mpg ~ hp * drat, mtcars)
    dm <- hypotheses(mod, "`hp:drat` = drat")
    expect_s3_class(dm, "hypotheses")
    expect_equal(nrow(dm), 1, ignore_attr = TRUE)

    ## Issue #684
    # . use "~/penguins.dta", clear
    # . encode species, gen(speciesid)
    # . qui logit large_penguin c.bill_length_mm##c.flipper_length_mm i.speciesid
    # . margins, dydx(*)
    #
    # Average marginal effects                                   Number of obs = 342
    # Model VCE: OIM
    #
    # Expression: Pr(large_penguin), predict()
    # dy/dx wrt:  bill_length_mm flipper_length_mm 2.speciesid 3.speciesid
    #
    # -----------------------------------------------------------------------------------
    #                   |            Delta-method
    #                   |      dy/dx   std. err.      z    P>|z|     [95% conf. interval]
    # ------------------+----------------------------------------------------------------
    #    bill_length_mm |   .0278588   .0059463     4.69   0.000     .0162043    .0395134
    # flipper_length_mm |   .0104927   .0023708     4.43   0.000      .005846    .0151394
    #                   |
    #         speciesid |
    #        Chinstrap  |  -.4127852   .0560029    -7.37   0.000    -.5225488   -.3030216
    #           Gentoo  |   .0609265   .1073649     0.57   0.570    -.1495048    .2713578
    # -----------------------------------------------------------------------------------
    # Note: dy/dx for factor levels is the discrete change from the base level.
    dat <- get_dataset("penguins", "palmerpenguins")
    dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
    mod <- glm(large_penguin ~ bill_length_mm * flipper_length_mm + species, data = dat, family = binomial)
    mfx <- avg_slopes(mod)
    expect_equal(mfx$estimate, c(.0278588, .0104927, -.4127852, .0609265), tolerance = 1e-4, ignore_attr = TRUE)
    expect_equal(mfx$std.error, c(.0059463, .0023708, .0560029, .1073649), tolerance = 1e-3, ignore_attr = TRUE)
})
