requiet("glmmTMB")
requiet("broom")

test_that("marginaleffects no validity", {
    Owls <- transform(Owls,
        Nest = reorder(Nest, NegPerChick),
        NCalls = SiblingNegotiation,
        FT = FoodTreatment)

    m0 <- glmmTMB(NCalls ~ (FT + ArrivalTime) * SexParent + offset(log(BroodSize)) + (1 | Nest),
        data = Owls,
        ziformula = ~1,
        family = poisson)
    expect_marginaleffects(m0)

    m1 <- glmmTMB(count ~ mined + (1 | site),
      zi = ~mined,
      family = poisson, data = Salamanders)
    expect_marginaleffects(m1)

    # Binomial model
    data(cbpp, package = "lme4")
    m4 <- glmmTMB(cbind(incidence, size - incidence) ~ period + (1 | herd),
        family = binomial, data = cbpp)
    expect_marginaleffects(m4)
})


test_that("comparisons vs. emmeans", {
    # Zero-inflated negative binomial model
    m2 <- glmmTMB(count ~ spp + mined + (1 | site),
      zi = ~spp + mined,
      family = nbinom2,
      data = Salamanders)
    co <- comparisons(m2,
                      type = "link",
                      variables = "mined",
                      newdata = datagrid(mined = "no",
                                         spp = "GP",
                                         site = "VF-1"))
    em <- tidy(pairs(emmeans(m2, "mined", at = list(spp = "GP", site = "VF-1"))))
    expect_marginaleffects(m2)
    expect_equal(co$comparison, -1 * em$estimate)
    expect_equal(co$std.error, em$std.error)

    # Hurdle Poisson model
    m3 <- glmmTMB(count ~ spp + mined + (1 | site),
      zi = ~spp + mined,
      family = truncated_poisson, data = Salamanders)
    expect_marginaleffects(m3)
    co <- comparisons(m3,
                      type = "link",
                      variables = "mined",
                      newdata = datagrid(mined = "no",
                                         spp = "GP",
                                         site = "VF-1"))
    em <- tidy(pairs(emmeans(m3, "mined", at = list(spp = "GP", site = "VF-1"))))
    expect_marginaleffects(m3)
    expect_equal(co$comparison, -1 * em$estimate)
    expect_equal(co$std.error, em$std.error)

})


test_that("contrast: manual check", {
    mod <- glmmTMB(count ~ spp + mined + (1 | site),
      zi = ~spp + mined,
      family = nbinom2,
      data = Salamanders)
    dat1 <- dat2 <- Salamanders
    dat1$mined <- "yes"
    dat2$mined <- "no"
    cont1 <- predict(mod, type = "response", newdata = dat2) -
             predict(mod, type = "response", newdata = dat1)
    cont2 <- comparisons(mod, variables = "mined")
    expect_equal(cont2$comparison, cont1)
})
