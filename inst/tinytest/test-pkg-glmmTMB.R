source("helpers.R")
# if (!EXPENSIVE) exit_file("EXPENSIVE")
using("marginaleffects")
# exit_file("glmmTMB always causes problems")

if (ON_CI) exit_file("on ci") # install and test fails on Github
requiet("glmmTMB")
requiet("emmeans")
requiet("broom")

data("Owls", package = "glmmTMB")

# marginaleffects no validity
Owls <- transform(Owls,
    Nest = reorder(Nest, NegPerChick),
    NCalls = SiblingNegotiation,
    FT = FoodTreatment)

m0 <- glmmTMB(NCalls ~ (FT + ArrivalTime) * SexParent + offset(log(BroodSize)) + (1 | Nest),
    data = Owls,
    ziformula = ~1,
    family = poisson)
expect_slopes(m0)

m1 <- glmmTMB(count ~ mined + (1 | site),
  zi = ~mined,
  family = poisson, data = Salamanders)
expect_slopes(m1)

# Binomial model
data(cbpp, package = "lme4")
m4 <- glmmTMB(cbind(incidence, size - incidence) ~ period + (1 | herd),
family = binomial, data = cbpp)
expect_slopes(m4)

# comparisons vs. emmeans

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
expect_slopes(m2)
expect_equivalent(co$estimate, -1 * em$estimate)
expect_equivalent(co$std.error, em$std.error)


# Issue reported by email by Olivier Baumais
bug <- glmmTMB(count ~ spp + mined,
  ziformula = ~spp + mined,
  family = "nbinom2",
  data = Salamanders)
mfx <- slopes(bug)
tid1 <- comparisons(bug, comparison = "dydxavg")
tid2 <- tidy(slopes(bug))

expect_equivalent(tid1$estimate, tid2$estimate)
expect_equivalent(tid1$std.error, tid2$std.error)
expect_equivalent(tid1$statistic, tid2$statistic)
expect_equivalent(tid1$p.value, tid2$p.value)
expect_equivalent(length(unique(abs(tid1$statistic))), 7)

bed <- marginaleffects:::modelarchive_data("new_bedford")
mzip_3 <- glmmTMB(
  x ~ cfp + c1 + pfp,
  ziformula = ~ res + inc + age,
  family = "nbinom2",
  data = bed)
tid <- avg_slopes(mzip_3, type = "response")

# TODO: half-checked against Stata. Slight difference on binary predictors. Stata probably dydx
b <- c(-0.0357107397803255, 0.116113581361053, -0.703975123794627, -0.322385169497792, 2.29943403870235, 0.313970669520973)
se <- c(0.0137118286464027, 0.335617116221601, 0.333707103584788, 0.0899355981887107, 
2.51759246321455, 2.10076503002941)
expect_equivalent(b, tid$estimate)
expect_equivalent(se, tid$std.error, tolerance = 1e-4)


# Hurdle Poisson model
m3 <- glmmTMB(count ~ spp + mined + (1 | site),
  zi = ~spp + mined,
  family = truncated_poisson, data = Salamanders)
expect_slopes(m3)
co <- comparisons(m3,
              type = "link",
              variables = "mined",
              newdata = datagrid(mined = "no",
                                 spp = "GP",
                                 site = "VF-1"))
em <- tidy(pairs(emmeans(m3, "mined", at = list(spp = "GP", site = "VF-1"))))
expect_slopes(m3)
expect_equivalent(co$estimate, -1 * em$estimate)
expect_equivalent(co$std.error, em$std.error)




# contrast: manual check
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
expect_equivalent(cont2$estimate, cont1)



# informative errors
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/lme4/VerbAgg.csv")
dat$woman <- as.numeric(dat$Gender == "F")
dat$item <- as.factor(dat$item)
mod <- glmmTMB(
    woman ~ btype + resp + (1 + Anger | item),
    family = binomial,
    data = dat)
expect_error(predictions(mod, newdata = datagrid(), vcov = "HC3"), pattern = "not supported")
expect_inherits(predictions(mod, newdata = datagrid(), vcov = NULL), "predictions")
expect_inherits(predictions(mod, newdata = datagrid(), vcov = FALSE), "predictions")
expect_inherits(predictions(mod, newdata = datagrid(), vcov = TRUE), "predictions")
expect_inherits(predictions(mod, newdata = datagrid(), vcov = insight::get_varcov(mod)), "predictions")




# marginalmeans (no validity)
dat <- "https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Titanic.csv"
dat <- read.csv(dat)
dat$z <- factor(sample(1:4, nrow(dat), replace = TRUE))
mod <- glmmTMB(
    Survived ~ Sex + z + (1 + Age | PClass),
    family = binomial,
    data = dat)
mm1 <- marginal_means(mod, variables = c("Sex", "PClass"))
mm2 <- marginal_means(mod, type = "link", variables = c("Sex", "PClass"))
mm3 <- marginal_means(mod, variables = c("Sex", "PClass"), cross = TRUE)
mm4 <- marginal_means(mod, type = "link", variables = c("Sex", "PClass"), cross = TRUE)
expect_true(all(mm1$estimate != mm2$estimate))
expect_true(all(mm1$std.error != mm2$std.error))
expect_true(all(mm3$estimate != mm4$estimate))
expect_true(all(mm3$std.error != mm4$std.error))
expect_true(nrow(mm3) > nrow(mm1))



# marginalmeans: some validity
p <- predictions(mod, type = "link", re.form = NA)
expect_inherits(p, "predictions")
em <- data.frame(emmeans(mod, ~Sex))
mm <- marginal_means(mod, variables = "Sex", type = "link", re.form = NA)
expect_equivalent(em$emmean, mm$estimate)
expect_equivalent(em$SE, mm$std.error)


mfx <- slopes(m1)
m1 <- glmmTMB(
    count ~ mined + (1 | site),
    zi = ~mined,
    family = poisson, data = Salamanders)
expect_inherits(marginal_means(m1, variables = "mined"), "marginalmeans")



# Issue #466: REML not supported
# Problem is that model$fit$par does not include all the parameters when
# REML=TRUE, so when we set `set_coef()`, we can't change the fixed effects,
# and the predictions are not altered. In turn, this produced 0 standard errors
# in `get_se_delta()`.
set.seed(42)
dat <- do.call("rbind", list(
  transform(PlantGrowth, trial = "A"),
  transform(PlantGrowth, trial = "B", weight = runif(30) * weight),
  transform(PlantGrowth, trial = "C", weight = runif(30) * weight)))
colnames(dat)[2] <- "groupid"

model <- glmmTMB(
  weight ~ groupid + trial + (1 | groupid:trial),
  REML = FALSE,
  data = dat)
em <- data.frame(emmeans(model, ~trial + groupid, df = Inf))
mm <- marginal_means(model, variables = c("trial", "groupid"), cross = TRUE, re.form = NA)
mm <- mm[order(mm$groupid, mm$trial),]
expect_equivalent(mm$estimate, em$emmean)
expect_equivalent(mm$conf.high, em$asymp.UCL)

model_REML <- glmmTMB(
  weight ~ groupid + trial + (1 | groupid:trial),
  REML = TRUE,
  data = dat)

expect_error(slopes(model_REML), pattern = "REML")
expect_error(comparisons(model_REML), pattern = "REML")
expect_error(predictions(model_REML), pattern = "REML")
expect_error(marginal_means(model_REML), pattern = "REML")
expect_inherits(slopes(model_REML, vcov = FALSE), "marginaleffects")
expect_inherits(predictions(model_REML, re.form = NA, vcov = FALSE), "predictions")
expect_inherits(predictions(model_REML, vcov = FALSE, re.form = NA), "predictions")


# Issue #663
if (!requiet("ordbetareg")) exit_file("ordbetareg")
requiet("dplyr")

data(pew, package = "ordbetareg")
model_data <- select(
  pew,
  therm,
  age = "F_AGECAT_FINAL",
  sex = "F_SEX_FINAL",
  income = "F_INCOME_FINAL",
  ideology = "F_IDEO_FINAL",
  race = "F_RACETHN_RECRUITMENT",
  education = "F_EDUCCAT2_FINAL",
  region = "F_CREGION_FINAL",
  approval = "POL1DT_W28",
  born_again = "F_BORN_FINAL",
  relig = "F_RELIG_FINAL",
  news = "NEWS_PLATFORMA_W28") %>%
  mutate_at(c("race", "ideology", "income", "approval", "sex", "education", "born_again", "relig"), function(c) {
    factor(c, exclude = levels(c)[length(levels(c))]) }) |>
  # need to make these ordered factors for BRMS
  transform(
    education = ordered(education),
    income = ordered(income))
model_data$therm_norm <- (model_data$therm - min(model_data$therm)) / (max(model_data$therm) - min(model_data$therm))
mod <- glmmTMB(
  therm_norm ~ approval + (1 | region),
  data = model_data,
  family = ordbeta(),
  start = list(psi = c(-1, 1)))
mfx <- avg_slopes(mod)
expect_inherits(mfx, 'slopes')


# Issue #707
set.seed(123)
n <- 200
d <- data.frame(
  outcome = rnorm(n),
  groups = as.factor(sample(c("treatment", "control"), n, TRUE)),
  episode = as.factor(sample(1:2, n, TRUE)),
  ID = as.factor(rep(1:10, n / 10)),
  wt = abs(rnorm(n, mean = 1, sd = 0.1)),
  sex = as.factor(sample(c("female", "male"), n, TRUE, prob = c(.4, .6))))
mod <- glmmTMB(outcome ~ groups * episode + (1 | ID), data = d, weights = wt)
tmp <<- head(d)
p <- avg_predictions(mod, variables = "groups", newdata = tmp)
expect_inherits(p, "predictions")



source("helpers.R")
rm(list = ls())