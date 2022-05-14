requiet("glmmTMB")

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
    requiet("emmeans")
    requiet("broom")
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


test_that("informative errors", {
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/lme4/VerbAgg.csv")
    dat$woman <- as.numeric(dat$Gender == "F")
    dat$item <- as.factor(dat$item)
    mod <- glmmTMB(
        woman ~ btype + resp + (1 + Anger | item),
        family = binomial,
        data = dat)
    expect_error(predictions(mod, newdata = datagrid(), vcov = "HC3"), regexp = "not supported")
    expect_error(predictions(mod, newdata = datagrid(), vcov = NULL), NA)
    expect_error(predictions(mod, newdata = datagrid(), vcov = FALSE), NA)
    expect_error(predictions(mod, newdata = datagrid(), vcov = TRUE), NA)
    expect_error(predictions(mod, newdata = datagrid(), vcov = stats::vcov(mod)), NA)
})



test_that("marginalmeans (no validity)", {
    dat <- "https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Titanic.csv"
    dat <- read.csv(dat)
    dat$z <- factor(sample(1:4, nrow(dat), replace = TRUE))
    mod <- glmmTMB(
        Survived ~ Sex + z + (1 + Age | PClass),
        family = binomial,
        data = dat)
    mm1 <- marginalmeans(mod, type = "response", variables = c("Sex", "PClass"))
    mm2 <- marginalmeans(mod, type = "link", variables = c("Sex", "PClass"))
    mm3 <- marginalmeans(mod, type = "response", variables = c("Sex", "PClass"), interaction = TRUE)
    mm4 <- marginalmeans(mod, type = "link", variables = c("Sex", "PClass"), interaction = TRUE)
    expect_true(all(mm1$marginalmean != mm2$marginalmean))
    expect_true(all(mm1$std.error != mm2$std.error))
    expect_true(all(mm3$marginalmean != mm4$marginalmean))
    expect_true(all(mm3$std.error != mm4$std.error))
    expect_true(nrow(mm3) > nrow(mm1))
})


test_that("regression bug: needs allow.new.levels = TRUE", {
    m1 <- glmmTMB(
        count ~ mined + (1 | site),
        zi = ~mined,
        family = poisson, data = Salamanders)
    expect_error(marginalmeans(m1, variables = "mined"), NA)
})
