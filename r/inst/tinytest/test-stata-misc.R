source("helpers.R")
using("marginaleffects")

requiet("MASS")
requiet("quantreg")
requiet("AER")
requiet("lme4")
requiet("geepack")
requiet("sandwich")
requiet("numDeriv")
requiet("sampleSelection")
requiet("pscl")
requiet("nnet")
requiet("survival")
requiet("survey")

golden_path <- "stata/results/misc.csv"
if (!file.exists(golden_path)) exit_file(golden_path)
golden <- read.csv(golden_path, stringsAsFactors = FALSE)
tested <- character()

# all.equal() silently falls back to an *absolute* comparison when the mean
# magnitude of the target drops below `tolerance`. Several of these fixtures
# have standard errors in the 1e-3 range, so a nominal tolerance of 1e-1 would
# accept an unbounded relative error. Passing `scale` explicitly keeps every
# comparison relative, so the declared bound always means what it says.
expect_rel <- function(current, target, tolerance, info) {
    scale <- max(mean(abs(target)), .Machine$double.eps)
    expect_equal(current, target, tolerance = tolerance, scale = scale, info = info)
}

# Stata's maximum-likelihood commands report the observed information matrix --
# the Hessian of the log-likelihood at the estimates. R's glm() fits by IRLS and
# reports the expected (Fisher) information. The two are identical for a
# canonical link, which is why logit, Poisson-log and Gamma-inverse agree to
# seven digits here, and differ otherwise: on these data the gap reaches 3% for
# probit and 11% for cloglog. Supplying the observed matrix removes it, and
# what is left is the delta-method machinery this suite is meant to test.
observed_information_vcov <- function(model) {
    y <- insight::get_response(model)
    X <- stats::model.matrix(model)
    w <- stats::weights(model, type = "prior")
    if (is.null(w)) w <- rep(1, length(y))
    offset <- model$offset
    if (is.null(offset)) offset <- rep(0, length(y))
    fam <- stats::family(model)
    deviance_at <- function(b) {
        mu <- fam$linkinv(as.vector(X %*% b) + offset)
        sum(fam$dev.resids(y, mu, w))
    }
    # -2 * loglik = deviance / dispersion + a constant that does not depend on
    # the coefficients, so the observed information for beta is half the
    # deviance Hessian divided by the dispersion.
    V <- summary(model)$dispersion * solve(0.5 * numDeriv::hessian(deviance_at, stats::coef(model)))
    dimnames(V) <- list(names(stats::coef(model)), names(stats::coef(model)))
    V
}

check_stata <- function(
    fixture,
    x,
    tolerance_estimate,
    tolerance_se,
    tolerance_ci = NA_real_,
    stata_order = NULL
) {
    s <- golden[golden$fixture == fixture, , drop = FALSE]
    if (!is.null(stata_order)) s <- s[stata_order, , drop = FALSE]
    tested <<- union(tested, fixture)
    expect_equal(nrow(x), nrow(s), info = paste(fixture, "nrow"))
    if (nrow(x) != nrow(s)) return(invisible(NULL))
    expect_rel(x$estimate, s$estimate, tolerance_estimate, paste(fixture, "estimate"))
    if (!is.na(tolerance_se)) {
        expect_rel(x$std.error, s$std_error, tolerance_se, paste(fixture, "std.error"))
    }
    # Stata exports the whole of r(table), so the interval bounds can be
    # compared too. This is opt-in because the two packages do not always use
    # the same reference distribution: `regress` margins are t-based on df_r
    # while marginaleffects defaults to z, and for a glm the default
    # type = "invlink(link)" builds the interval on the link scale and back
    # transforms it rather than adding a symmetric delta-method margin.
    if (!is.na(tolerance_ci)) {
        expect_rel(x$conf.low, s$conf_low, tolerance_ci, paste(fixture, "conf.low"))
        expect_rel(x$conf.high, s$conf_high, tolerance_ci, paste(fixture, "conf.high"))
    }
    invisible(NULL)
}

# The broad Stata matrix uses these same data transformations. `am` is a
# factor because Stata fits `i.am`; `cyl` remains numeric and is only used by
# margins, over(cyl), corresponding to marginaleffects' `by = "cyl"`.
dat <- transform(
    mtcars,
    am = factor(am),
    mpg_censored = pmax(mpg, 18),
    mpg_fraction = mpg / 40,
    panel = factor((seq_len(nrow(mtcars)) - 1L) %% 8L + 1L)
)

models <- list(
    lm = lm(mpg ~ hp + wt + am, data = dat),
    probit = suppressWarnings(glm(vs ~ hp + wt + am, family = binomial("probit"), data = dat)),
    cloglog = suppressWarnings(glm(vs ~ hp + wt + am, family = binomial("cloglog"), data = dat)),
    poisson = glm(carb ~ hp + wt + am, family = poisson(), data = dat),
    negative_binomial = suppressWarnings(MASS::glm.nb(carb ~ hp + wt + am, data = dat)),
    gamma_log = glm(mpg ~ hp + wt + am, family = Gamma("log"), data = dat),
    quantile_median = suppressWarnings(quantreg::rq(mpg ~ hp + wt + am, tau = .5, data = dat)),
    tobit_left = suppressWarnings(AER::tobit(
        mpg_censored ~ hp + wt + am, left = 18, data = dat
    )),
    fractional_logit = suppressWarnings(glm(
        mpg_fraction ~ hp + wt + am,
        family = binomial("logit"),
        data = dat
    )),
    mixed_gaussian = lme4::lmer(
        mpg ~ hp + wt + am + (1 | panel),
        data = dat,
        REML = FALSE
    )
)

dat_gee <- dat[order(dat$panel), ]
models$gee_poisson <- geepack::geeglm(
    carb ~ hp + wt + am,
    id = panel,
    data = dat_gee,
    family = poisson(),
    corstr = "exchangeable"
)

run_command <- function(model, command, vcov = TRUE) {
    switch(command,
        avg_predictions = avg_predictions(model, vcov = vcov),
        avg_predictions_by = avg_predictions(model, by = "cyl", vcov = vcov),
        avg_comparisons_factor = avg_comparisons(model, variables = "am", vcov = vcov),
        avg_comparisons_factor_by = avg_comparisons(
            model, variables = "am", by = "cyl", vcov = vcov
        ),
        avg_slopes = avg_slopes(model, variables = "hp", vcov = vcov),
        avg_slopes_by = avg_slopes(model, variables = "hp", by = "cyl", vcov = vcov)
    )
}

commands <- c(
    "avg_predictions",
    "avg_predictions_by",
    "avg_comparisons_factor",
    "avg_comparisons_factor_by",
    "avg_slopes",
    "avg_slopes_by"
)

# These bounds are relative under tinytest/all.equal, with automatic absolute
# comparison near zero. They accommodate optimizer and numerical-derivative
# differences while still detecting substantively different results.
#
# Two kinds of standard error bound appear below, and they must not be confused
# for one another. The tight ones -- lm, poisson, negative_binomial,
# tobit_left, mixed_gaussian -- sit a small multiple above the deviation this
# package actually produces, so they fail if the delta-method Jacobian
# degrades. Those bounds depend on the analytic Jacobians: a whole-pipeline
# numeric derivative leaves finite-difference error an order of magnitude or
# more above them, and `lm` alone moved from 6.9e-05 to 1.8e-07 when the
# Jacobian for slopes stopped being differenced.
#
# The loose ones -- probit, cloglog, gamma_log, quantile_median,
# Every bound below binds: `expect_rel` keeps the comparison relative, and each
# one sits within about a factor of four of what the pair actually achieves.
# The two entries with no standard-error bound are cases where the two packages
# estimate a genuinely different quantity rather than the same one imprecisely,
# and a loose numeric tolerance there would only look like a test.
tol <- list(
    lm = c(estimate = 1e-6, se = 1e-6),
    probit = c(estimate = 1e-4, se = 1e-4),
    cloglog = c(estimate = 1e-4, se = 5e-3),
    poisson = c(estimate = 1e-5, se = 1e-5),
    negative_binomial = c(estimate = 1e-4, se = 1e-4),
    gamma_log = c(estimate = 1e-4, se = 5e-4),
    # Stata's qreg and quantreg::rq both invert a Koenker-Bassett sandwich, but
    # they estimate the sparsity function -- the conditional density at the
    # quantile -- with different bandwidth rules. insight returns rq's "nid"
    # variance, which is roughly half Stata's; "iid" is the closest of the
    # three and is still 13% away. The two are not the same estimator, so only
    # the coefficients are compared.
    quantile_median = c(estimate = 1e-4, se = NA_real_),
    tobit_left = c(estimate = 1e-4, se = 5e-4),
    fractional_logit = c(estimate = 1e-4, se = 1e-4),
    mixed_gaussian = c(estimate = 5e-3, se = 5e-4),
    # Stata and geepack use different scale/covariance conventions here. Point
    # estimates are comparable; GEE standard errors are deliberately omitted.
    gee_poisson = c(estimate = 1e-3, se = NA_real_)
)

# probit, cloglog and gamma-log all use a non-canonical link, so Stata's
# observed information and glm()'s expected information do not coincide. See
# observed_information_vcov() above.
non_canonical_link <- c("probit", "cloglog", "gamma_log")

for (model_name in names(models)) {
    V <- TRUE
    if (model_name == "fractional_logit") {
        # fracreg is a quasi-likelihood estimator and Stata reports the robust
        # sandwich by default, with the same n / (n - 1) multiplier it applies
        # after any maximum-likelihood command.
        n <- stats::nobs(models[[model_name]])
        V <- sandwich::vcovHC(models[[model_name]], type = "HC0") * n / (n - 1)
    } else if (model_name %in% non_canonical_link) {
        V <- observed_information_vcov(models[[model_name]])
    }
    for (command in commands) {
        fixture <- paste(model_name, command, sep = "_")
        ans <- suppressWarnings(run_command(models[[model_name]], command, vcov = V))
        check_stata(fixture, ans, tol[[model_name]][["estimate"]], tol[[model_name]][["se"]])
    }
}

# Independently validated GLM transformations introduced before the broad
# matrix. These exercise nonlinear comparison functions and elasticities.
mod_logit <- glm(am ~ hp + wt + factor(vs), family = binomial(), data = mtcars)
check_stata("logit_dydx", avg_slopes(mod_logit, variables = "hp"), 1e-6, 1e-5)

nonlinear <- lapply(c("ratio", "lnratio", "lift", "lnor"), function(key) {
    out <- avg_comparisons(mod_logit, variables = "vs", comparison = key)
    data.frame(estimate = out$estimate, std.error = out$std.error)
})
nonlinear <- do.call(rbind, nonlinear)
check_stata("logit_nonlinear", nonlinear, 1e-6, 1e-5)

mod_poisson <- glm(carb ~ hp + wt, family = poisson(), data = mtcars)
# `eyex` and `eydx` divide by the prediction, so their comparison groups are
# marked `uses_y` and stay on the numeric whole-pipeline path; their standard
# errors carry finite-difference error and need the looser bound. `dyex` has an
# analytic Jacobian and agrees with Stata three orders of magnitude more
# closely.
for (slope in c("eyex", "eydx")) {
    check_stata(
        paste0("poisson_", slope),
        avg_slopes(mod_poisson, variables = c("hp", "wt"), slope = slope),
        1e-6,
        1e-3
    )
}
check_stata(
    "poisson_dyex",
    avg_slopes(mod_poisson, variables = c("hp", "wt"), slope = "dyex"),
    1e-6,
    1e-5
)

# Selection model: Stata's conditional outcome is conditional on selection,
# corresponding to the E[yo|ys=1] rows returned by marginaleffects.
data("Mroz87", package = "sampleSelection")
mod_selection <- sampleSelection::selection(
    lfp ~ age + educ + kids5 + kids618 + nwifeinc,
    wage ~ educ + exper,
    data = Mroz87,
    method = "ml"
)
x <- avg_slopes(
    mod_selection,
    variables = c("age", "educ", "kids5", "kids618", "nwifeinc"),
    part = "selection",
    type = "response"
)
x <- x[match(c("educ", "age", "kids5", "kids618", "nwifeinc"), x$term), ]
check_stata("heckman_selection", x, 1e-6, 1e-4)
x <- avg_slopes(
    mod_selection,
    variables = c("educ", "exper"),
    part = "outcome",
    type = "conditional"
)
x <- x[x$group == "E[yo|ys=1]", ]
x <- x[match(c("educ", "exper"), x$term), ]
check_stata("heckman_outcome_conditional", x, 1e-6, 1e-4)

# Zero-inflated negative binomial components.
# The releveled copy gets a name of its own on purpose. data() loads into the
# global environment, and insight::get_data() re-resolves the model's `data`
# argument there rather than in this file's environment, so overwriting
# `bioChemists` locally would leave the counterfactual grids built from pscl's
# original level order -- silently flipping the sign of the `mar` contrast.
data("bioChemists", package = "pscl")
bio <- transform(
    bioChemists,
    fem = factor(fem, c("Men", "Women")),
    mar = factor(mar, c("Married", "Single"))
)
mod_zinb <- pscl::zeroinfl(
    art ~ phd + ment + fem + mar + kid5 | ment + kid5 + fem,
    data = bio,
    dist = "negbin"
)
# marginaleffects sorts terms alphabetically while Stata keeps the order given
# to dydx(), so the rows are aligned on the term name rather than on position.
zinb_specs <- list(
    zinb_response = list(type = "response", terms = c("phd", "ment", "fem", "mar", "kid5")),
    zinb_count_mean = list(type = "count", terms = c("phd", "ment", "fem", "mar", "kid5")),
    zinb_zero_probability = list(type = "zero", terms = c("ment", "fem", "kid5"))
)
for (fixture in names(zinb_specs)) {
    spec <- zinb_specs[[fixture]]
    x <- avg_slopes(mod_zinb, variables = spec$terms, type = spec$type)
    check_stata(fixture, x[match(spec$terms, x$term), ], 1e-4, 1e-3)
}

# Ordinal and multinomial outcome-specific margins. Expand frequency weights
# exactly as misc.ado does so likelihood and covariance calculations coincide.
housing <- MASS::housing
housing$Sat <- ordered(housing$Sat, c("Low", "Medium", "High"))
housing$Infl <- factor(housing$Infl, c("Low", "Medium", "High"))
housing$Type <- factor(housing$Type, c("Tower", "Apartment", "Atrium", "Terrace"))
housing$Cont <- factor(housing$Cont, c("Low", "High"))
housing <- housing[rep(seq_len(nrow(housing)), housing$Freq), ]

categorical_order <- function(x, multinomial = FALSE) {
    wanted <- if (multinomial) {
        c(
            "Infl|Medium - Low", "Infl|High - Low", "Cont|High - Low",
            "Type|Apartment - Tower", "Type|Atrium - Tower", "Type|Terrace - Tower"
        )
    } else {
        c(
            "Infl|Medium - Low", "Infl|High - Low",
            "Type|Apartment - Tower", "Type|Atrium - Tower", "Type|Terrace - Tower",
            "Cont|High - Low"
        )
    }
    key <- paste(x$term, x$contrast, sep = "|")
    x[match(wanted, key), ]
}

mod_ologit <- MASS::polr(
    Sat ~ Infl * Type + Cont,
    data = housing,
    Hess = TRUE,
    method = "logistic"
)
p <- avg_predictions(mod_ologit, type = "probs")
cmp <- avg_comparisons(
    mod_ologit,
    variables = c("Infl", "Type", "Cont"),
    type = "probs"
)
for (j in seq_along(levels(housing$Sat))) {
    group <- levels(housing$Sat)[j]
    check_stata(paste0("ologit_predictions_outcome", j), p[p$group == group, ], 1e-5, 1e-5)
    check_stata(
        paste0("ologit_comparisons_outcome", j),
        categorical_order(cmp[cmp$group == group, ]),
        1e-5,
        1e-5
    )
}

housing$Sat_nominal <- factor(housing$Sat, c("Low", "Medium", "High"))
mod_mlogit <- nnet::multinom(
    Sat_nominal ~ Infl * Cont + Type,
    data = housing,
    trace = FALSE,
    Hess = TRUE
)
p <- avg_predictions(mod_mlogit, type = "probs")
cmp <- avg_comparisons(
    mod_mlogit,
    variables = c("Infl", "Cont", "Type"),
    type = "probs"
)
for (j in seq_along(levels(housing$Sat_nominal))) {
    group <- levels(housing$Sat_nominal)[j]
    check_stata(paste0("mlogit_predictions_outcome", j), p[p$group == group, ], 1e-5, 1e-5)
    check_stata(
        paste0("mlogit_comparisons_outcome", j),
        categorical_order(cmp[cmp$group == group, ], multinomial = TRUE),
        1e-5,
        1e-5
    )
}

# Cox predictions use reference="zero" to match Stata's uncentered predict(hr).
kidtran <- read.csv("stata/databases/kidtran.csv")
mod_cox <- survival::coxph(
    survival::Surv(time, delta) ~ age + factor(gender) + factor(race),
    data = kidtran,
    ties = "efron"
)
x <- suppressWarnings(avg_slopes(
    mod_cox,
    variables = c("age", "gender", "race"),
    type = "lp"
))
check_stata("cox_lp", x, 1e-3, 1e-5)
x <- suppressWarnings(avg_slopes(
    mod_cox,
    variables = c("age", "gender", "race"),
    type = "risk",
    reference = "zero"
))
check_stata("cox_hazard_ratio", x, 1e-3, 1e-3)

# Binary changes, interactions, by-groups and weights on iris. Stata fits these
# with vce(robust); after a maximum-likelihood command that is the plain HC0
# sandwich times n / (n - 1).
stata_robust <- function(model) {
    n <- stats::nobs(model)
    sandwich::vcovHC(model, type = "HC0") * n / (n - 1)
}

# Replicates margins' at(): fix one or more variables at each grid value,
# leave the rest at their observed values, and average. expand.grid varies its
# first column fastest while Stata's _at index varies the last one fastest, so
# the grid is reordered before the rows are stacked.
counterfactual <- function(data, ...) {
    values <- list(...)
    grid <- expand.grid(values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    grid <- grid[do.call(order, rev(grid)), , drop = FALSE]
    do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
        d <- data
        for (v in names(grid)) d[[v]] <- grid[[v]][i]
        d
    }))
}

# `long_sepal` uses a cut point between two observed values. iris is imported
# into Stata as float and the literal in a comparison is promoted to double, so
# a threshold of 5.8 would put the rows holding exactly 5.8 on the greater-than
# side; misc.ado uses 5.85 for the same reason. `scount` uses floor(x + 0.5)
# because Stata's round() goes half away from zero while R's round() is
# half-to-even.
dat_iris <- transform(
    iris,
    wide = as.integer(Sepal.Width > 3),
    long_sepal = factor(as.integer(Sepal.Length > 5.85)),
    scount = floor(2 * Sepal.Width + 0.5),
    aw = seq_len(nrow(iris))
)

mod_iris <- glm(wide ~ Petal.Length * Petal.Width + long_sepal, family = binomial, data = dat_iris)
V_iris <- stata_robust(mod_iris)
check_stata(
    "logit_binary",
    avg_comparisons(mod_iris, variables = "long_sepal", vcov = V_iris),
    1e-6, 1e-6
)
newdata_long <- do.call(rbind, lapply(levels(dat_iris$long_sepal), function(k) {
    transform(dat_iris, long_sepal = factor(k, levels = levels(dat_iris$long_sepal)))
}))
check_stata(
    "logit_binary_by_species",
    predictions(mod_iris, newdata = newdata_long, by = c("Species", "long_sepal"), vcov = V_iris),
    1e-6, 1e-5
)
check_stata(
    "logit_interaction_at",
    avg_slopes(
        mod_iris,
        variables = "Petal.Length",
        newdata = counterfactual(dat_iris, Petal.Width = c(0.5, 1.5, 2.5)),
        by = "Petal.Width",
        vcov = V_iris
    ),
    1e-5, 1e-5
)

# pweights in Stata imply vce(robust), and the estimation weights are reused
# when the margins are averaged.
mod_iris_pois <- suppressWarnings(glm(
    scount ~ Sepal.Length * Petal.Length + long_sepal,
    family = poisson,
    data = dat_iris,
    weights = aw
))
V_iris_pois <- stata_robust(mod_iris_pois)
weighted_robust <- rbind(
    as.data.frame(avg_slopes(mod_iris_pois, variables = "Sepal.Length", wts = "aw", vcov = V_iris_pois))[, c("estimate", "std.error")],
    as.data.frame(avg_comparisons(mod_iris_pois, variables = "long_sepal", wts = "aw", vcov = V_iris_pois))[, c("estimate", "std.error")]
)
check_stata("poisson_weighted_robust", weighted_robust, 1e-6, 1e-4)

mod_iris_at <- glm(wide ~ Petal.Length * long_sepal + Petal.Width, family = binomial, data = dat_iris)
check_stata(
    "logit_predictions_at",
    predictions(
        mod_iris_at,
        newdata = counterfactual(
            dat_iris,
            Petal.Length = c(1.5, 4.5, 6.5),
            long_sepal = factor(0:1, levels = levels(dat_iris$long_sepal))
        ),
        by = c("Petal.Length", "long_sepal")
    ),
    1e-5, 1e-4
)
check_stata(
    "logit_comparisons_at",
    avg_comparisons(
        mod_iris_at,
        variables = "long_sepal",
        newdata = counterfactual(dat_iris, Petal.Length = c(1.5, 4.5, 6.5)),
        by = "Petal.Length"
    ),
    1e-5, 1e-4
)

# Survey-weighted margins over a subpopulation. The model is fit on the full
# design, exactly as `svy: logit` does, and only the averaging is restricted to
# the domain, which is what margins' subpop() means.
dat_svy <- transform(
    mtcars,
    vs = factor(vs),
    cyl = factor(cyl),
    psu = seq_len(nrow(mtcars)),
    strata = (seq_len(nrow(mtcars)) - 1L) %% 4L + 1L,
    pwt = mpg,
    domain = as.integer(cyl != 8)
)
design <- survey::svydesign(ids = ~psu, strata = ~strata, weights = ~pwt, data = dat_svy)
mod_svy <- survey::svyglm(am ~ hp + vs + wt, design = design, family = quasibinomial())
subpop <- dat_svy[dat_svy$domain == 1, , drop = FALSE]
survey_logit <- rbind(
    as.data.frame(avg_slopes(mod_svy, variables = "hp", newdata = subpop, wts = "pwt"))[, c("estimate", "std.error")],
    as.data.frame(avg_comparisons(mod_svy, variables = "vs", newdata = subpop, wts = "pwt"))[, c("estimate", "std.error")]
)
check_stata("survey_logit", survey_logit, 1e-5, 1e-4)
newdata_vs <- do.call(rbind, lapply(levels(dat_svy$vs), function(k) {
    transform(subpop, vs = factor(k, levels = levels(dat_svy$vs)))
}))
check_stata(
    "survey_logit_by_cyl",
    predictions(mod_svy, newdata = newdata_vs, by = c("cyl", "vs"), wts = "pwt"),
    1e-5, 1e-5
)

# Median survival time after a Weibull fit. Standard errors are not compared:
# Stata's margins excludes the ancillary log(scale) from the delta method while
# survreg's covariance matrix includes it, which is the same discrepancy the
# parametric-survival fixtures in test-stata-models.R document.
mod_weibull <- survival::survreg(
    survival::Surv(time, delta) ~ age + factor(gender) + factor(race),
    data = kidtran,
    dist = "weibull"
)
check_stata(
    "weibull_median_survival",
    predictions(
        mod_weibull,
        newdata = counterfactual(kidtran, age = c(30, 50, 70)),
        by = "age",
        type = "quantile",
        p = 0.5
    ),
    1e-5,
    NA_real_
)

# Every generated fixture must be asserted above or explicitly classified
# here. These are retained as exploratory Stata output but are not equivalent
# cross-software assertions on this branch.
excluded <- c(
    # Stata's `predict(yexpected)` after heckman is the unconditional mean,
    # which mixes the selection probability into the outcome equation.
    # marginaleffects exposes the selection and conditional-outcome parts but
    # not that product.
    "heckman_outcome_unconditional",
    # Generated by `tpoisson ... , ll(0)` on the positive counts, which is a
    # standalone truncated-Poisson likelihood rather than the count component
    # of a hurdle model. pscl::hurdle maximises a different objective, so the
    # two are not estimating the same parameters.
    "hurdle_positive_count",
    # streg's default metric is proportional hazards and predict(hr) is a
    # relative hazard. survreg() is accelerated-failure-time only and
    # marginaleffects offers no hazard-scale prediction for it.
    "weibull_hazard_ratio",
    # Stata's predict(mean time) integrates the survivor function.
    # marginaleffects exposes type = "quantile" (used for the median fixture
    # below) but no mean-survival-time prediction.
    "weibull_mean_survival"
)
expect_equal(
    sort(unique(golden$fixture)),
    sort(union(tested, excluded)),
    info = "every misc.csv fixture is tested or explicitly excluded"
)
