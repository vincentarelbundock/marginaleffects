source("helpers.R")
using("marginaleffects")

if (ON_CI) exit_file("on ci") # install and test fails on Github
requiet("glmmTMB")
requiet("emmeans")
requiet("broom")

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
expect_slopes(m0, re.form = NA)

m1 <- glmmTMB(
    count ~ mined + (1 | site),
    zi = ~mined,
    family = poisson,
    data = Salamanders
)
expect_slopes(m1, re.form = NA)

# Binomial model
data(cbpp, package = "lme4")
m4 <- glmmTMB(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    family = binomial,
    data = cbpp
)
expect_slopes(m4, re.form = NA)

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
expect_slopes(m2)
expect_equivalent(co$estimate, -1 * em$estimate)
expect_equivalent(co$std.error, em$std.error)


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

expect_equivalent(tid1$estimate, tid2$estimate)
expect_equivalent(tid1$std.error, tid2$std.error, tolerance = 1e-6)
expect_equivalent(tid1$statistic, tid2$statistic, tolerance = 1e-6)
expect_equivalent(tid1$p.value, tid2$p.value, tolerance = 1e-6)
expect_equivalent(length(unique(abs(tid1$statistic))), 7)

bed <- marginaleffects:::modelarchive_data("new_bedford")
mzip_3 <- glmmTMB(
    x ~ cfp + c1 + pfp,
    ziformula = ~ res + inc + age,
    family = "nbinom2",
    data = bed
)
tid <- avg_slopes(mzip_3, type = "response", re.form = NA) |>
    dplyr::arrange(term)

# TODO: half-checked against Stata. Slight difference on binary predictors. Stata probably dydx
# Stata can't be right here, or I mischecked.
b <- c(
    -0.0357107397803255,
    0.116113581361053,
    -0.703975123794627,
    -0.322385169497792,
    2.29943403870235,
    0.313970669520973
)
# se <- c(0.0137118286464027, 0.335617116221601, 0.333707103584788, 0.0899355981887107, 2.51759246321455, 2.10076503002941)
expect_equivalent(b, tid$estimate, tolerance = 1e-3)
# expect_equivalent(se, tid$std.error, tolerance = 1e-4)

# Hurdle Poisson model
m3 <- glmmTMB(
    count ~ spp + mined + (1 | site),
    zi = ~ spp + mined,
    family = truncated_poisson,
    data = Salamanders
)
expect_slopes(m3, re.form = NA)
co <- comparisons(
    m3,
    type = "link",
    variables = "mined",
    re.form = NA,
    newdata = datagrid(
        mined = "no",
        spp = "GP",
        site = "VF-1"
    )
)
em <- tidy(pairs(emmeans(m3, "mined", at = list(spp = "GP", site = "VF-1"))))
expect_slopes(m3)
expect_equivalent(co$estimate, -1 * em$estimate)
expect_equivalent(co$std.error, em$std.error)


# contrast: manual check
mod <- glmmTMB(
    count ~ spp + mined + (1 | site),
    zi = ~ spp + mined,
    family = nbinom2,
    data = Salamanders
)
dat1 <- dat2 <- Salamanders
dat1$mined <- "yes"
dat2$mined <- "no"
cont1 <- predict(mod, type = "response", newdata = dat2, re.form = NA) -
    predict(mod, type = "response", newdata = dat1, re.form = NA)
cont2 <- comparisons(mod, variables = "mined", re.form = NA)
expect_equivalent(cont2$estimate, cont1)


# informative errors
dat <- get_dataset("VerbAgg", "lme4")
dat$woman <- as.numeric(dat$Gender == "F")
dat$item <- as.factor(dat$item)
mod <- glmmTMB(
    woman ~ btype + resp + (1 + Anger | item),
    family = binomial,
    data = dat
)

expect_error(
    predictions(mod, newdata = datagrid(), vcov = "HC3", re.form = NA),
    pattern = "vcov"
)
expect_error(
    predictions(mod, newdata = datagrid(), vcov = NULL, re.form = NA),
    pattern = "vcov"
)
expect_error(
    predictions(
        mod,
        newdata = datagrid(),
        vcov = insight::get_varcov(mod),
        re.form = NA
    ),
    pattern = "vcov"
)
expect_inherits(
    predictions(mod, newdata = datagrid(), vcov = FALSE, re.form = NA),
    "predictions"
)
expect_inherits(
    predictions(mod, newdata = datagrid(), vcov = TRUE, re.form = NA),
    "predictions"
)


# Where is that model?
# # marginalmeans: some validity
# p <- predictions(mod, type = "link", re.form = NA)
# expect_inherits(p, "predictions")
# em <- data.frame(emmeans(mod, ~Sex))
# mm <- predictions(mod, by = "Sex", newdata = datagrid(grid_type = "balanced"), type = "link", re.form = NA)
# expect_equivalent(em$emmean, mm$estimate)
# expect_equivalent(em$SE, mm$std.error)

# Issue #663
if (!requiet("ordbetareg")) exit_file("ordbetareg")
requiet("dplyr")

data(pew, package = "ordbetareg")
model_data <- dplyr::select(
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
    news = "NEWS_PLATFORMA_W28"
) %>%
    mutate_at(
        c(
            "race",
            "ideology",
            "income",
            "approval",
            "sex",
            "education",
            "born_again",
            "relig"
        ),
        function(c) {
            factor(c, exclude = levels(c)[length(levels(c))])
        }
    ) |>
    # need to make these ordered factors for BRMS
    transform(
        education = ordered(education),
        income = ordered(income)
    )
model_data$therm_norm <- (model_data$therm - min(model_data$therm)) /
    (max(model_data$therm) - min(model_data$therm))
mod <- glmmTMB(
    therm_norm ~ approval + (1 | region),
    data = model_data,
    family = ordbeta(),
    start = list(psi = c(-1, 1))
)
mfx <- avg_slopes(mod, re.form = NA)
expect_inherits(mfx, "slopes")


# Issue #707
set.seed(123)
n <- 200
d <- data.frame(
    outcome = rnorm(n),
    groups = as.factor(sample(c("treatment", "control"), n, TRUE)),
    episode = as.factor(sample(1:2, n, TRUE)),
    ID = as.factor(rep(1:10, n / 10)),
    wt = abs(rnorm(n, mean = 1, sd = 0.1)),
    sex = as.factor(sample(c("female", "male"), n, TRUE, prob = c(.4, .6)))
)
mod <- glmmTMB(outcome ~ groups * episode + (1 | ID), data = d, weights = wt)
tmp <<- head(d)
p <- avg_predictions(mod, variables = "groups", newdata = tmp, re.form = NA)
expect_inherits(p, "predictions")


# Simple prediction standard errors
m <- glmmTMB(
    mpg ~ hp + (1 | carb),
    data = transform(mtcars, carb = as.character(carb))
)
p1 <- predictions(m, re.form = NA)
p2 <- data.frame(predict(m, se.fit = TRUE))
expect_equivalent(p1$estimate, p2$fit)
expect_equivalent(p1$std.error, p2$se.fit, tol = 1e-6)


# Issue #810
m <- glmmTMB(Sepal.Length ~ Sepal.Width, data = iris)
p1 <- predictions(m, newdata = iris, re.form = NA) |> head()
p2 <- data.frame(predict(m, newdata = iris, se.fit = TRUE)) |> head()
expect_equivalent(p1$estimate, p2$fit)
expect_equivalent(p1$std.error, p2$se.fit, tol = 1e-6)

m <- glmmTMB(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)
p1 <- predictions(m, newdata = iris, re.form = NA) |> head()
p2 <- data.frame(predict(m, newdata = iris, se.fit = TRUE, re.form = NA)) |>
    head()
expect_equivalent(p1$estimate, p2$fit)
expect_equivalent(p1$std.error, p2$se.fit, tol = 1e-6)

m <- glmmTMB(
    Sepal.Length ~ Sepal.Width + (1 | Petal.Width * Species),
    data = iris
)
p1 <- predictions(m, newdata = iris, re.form = NA)
p2 <- data.frame(predict(m, newdata = iris, se.fit = TRUE, re.form = NA))
expect_equivalent(p1$estimate, p2$fit)
expect_equivalent(p1$std.error, p2$se.fit, tol = 1e-6)


# Issue #1064
m <- glmmTMB(mpg ~ hp + am + (1 | cyl), data = mtcars)
p1 <- predictions(m, newdata = mtcars, re.form = NA)
p2 <- predict(m, se.fit = TRUE, re.form = NA)
expect_equivalent(p1$estimate, p2$fit)
expect_equivalent(p1$std.error, p2$se.fit)


# Issue #1189
dat <- transform(
    Owls,
    Nest = reorder(Nest, NegPerChick),
    NCalls = SiblingNegotiation,
    FT = FoodTreatment
)

mod <- glmmTMB(
    NCalls ~ (FT + ArrivalTime) * SexParent + offset(log(BroodSize)) + (1 | Nest),
    data = dat,
    ziformula = ~SexParent,
    family = poisson
)

p <- avg_predictions(mod, type = "zprob", re.form = NA)
s <- avg_slopes(mod, variables = "SexParent", type = "zprob", re.form = NA)
expect_false(anyNA(p$std.error))
expect_false(anyNA(s$std.error))


# Issue 1221
ord_fit <- glmmTMB(
    formula = therm / 100 ~ F_EDUCCAT2_FINAL + F_INCOME_FINAL + (1 | F_INCOME_FINAL),
    data = pew,
    family = ordbeta
)
p <- avg_predictions(ord_fit, re.form = NA)
expect_false(anyNA(p$estimate))
expect_false(anyNA(p$std.error))


# Issue 1224
mod <- glmmTMB(Sepal.Length ~ Petal.Width + (1 | Species), data = iris)
h <- hypotheses(mod, hypothesis = "b1 - b2 = 0")
expect_false(anyNA(h$estimate))
expect_false(anyNA(h$std.error))


# Issue 1435: non-conformable
data(sleepstudy, package = "lme4")
g0 <- glmmTMB(Reaction ~ Days + (Days | Subject), sleepstudy)
p <- predictions(
    g0,
    by = "Days",
    re.form = NA
) |>
    hypotheses()
expect_inherits(p, "hypotheses")
expect_equal(nrow(p), 10)


# Issue #1490: no warning when vcov=FALSE
options(marginaleffects_safe = TRUE)
mod <- glmmTMB(Sepal.Length ~ Sepal.Width + (1|Species), data = iris)
expect_warning(avg_comparisons(mod, vcov = TRUE))
expect_warning(avg_comparisons(mod))
expect_false(ignore(expect_warning)(avg_comparisons(mod, vcov = FALSE)))
options(marginaleffects_safe = NULL)
