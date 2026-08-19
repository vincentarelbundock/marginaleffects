source("helpers.R")
using("marginaleffects")

requiet("MASS")
requiet("quantreg")
requiet("AER")
requiet("lme4")
requiet("geepack")
requiet("sandwich")
requiet("sampleSelection")
requiet("pscl")
requiet("nnet")
requiet("survival")

golden_path <- "stata/results/misc.csv"
if (!file.exists(golden_path)) exit_file(golden_path)
golden <- read.csv(golden_path, stringsAsFactors = FALSE)
tested <- character()

check_stata <- function(
    fixture,
    x,
    tolerance_estimate,
    tolerance_se,
    stata_order = NULL
) {
    s <- golden[golden$fixture == fixture, , drop = FALSE]
    if (!is.null(stata_order)) s <- s[stata_order, , drop = FALSE]
    tested <<- union(tested, fixture)
    expect_equal(nrow(x), nrow(s), info = paste(fixture, "nrow"))
    if (nrow(x) != nrow(s)) return(invisible(NULL))
    expect_equal(
        x$estimate,
        s$estimate,
        tolerance = tolerance_estimate,
        info = paste(fixture, "estimate")
    )
    if (!is.na(tolerance_se)) {
        expect_equal(
            x$std.error,
            s$std_error,
            tolerance = tolerance_se,
            info = paste(fixture, "std.error")
        )
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
# fractional_logit -- absorb a genuine cross-software disagreement in how the
# standard error itself is defined, not numerical noise. Analytic derivatives
# do not move them at all, and tightening them would only encode Stata's
# conventions as if they were ours.
tol <- list(
    lm = c(estimate = 1e-6, se = 1e-6),
    probit = c(estimate = 1e-4, se = 5e-2),
    cloglog = c(estimate = 1e-4, se = 1e-1),
    poisson = c(estimate = 1e-5, se = 1e-5),
    negative_binomial = c(estimate = 1e-4, se = 1e-4),
    gamma_log = c(estimate = 1e-4, se = 2e-2),
    quantile_median = c(estimate = 1e-4, se = 5e-1),
    tobit_left = c(estimate = 1e-4, se = 5e-4),
    fractional_logit = c(estimate = 1e-4, se = 5e-2),
    mixed_gaussian = c(estimate = 5e-3, se = 5e-4),
    # Stata and geepack use different scale/covariance conventions here. Point
    # estimates are comparable; GEE standard errors are deliberately omitted.
    gee_poisson = c(estimate = 1e-3, se = NA_real_)
)

for (model_name in names(models)) {
    V <- TRUE
    if (model_name == "fractional_logit") {
        V <- sandwich::vcovHC(models[[model_name]], type = "HC0")
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
data("bioChemists", package = "pscl")
bioChemists$fem <- factor(bioChemists$fem, c("Men", "Women"))
bioChemists$mar <- factor(bioChemists$mar, c("Married", "Single"))
mod_zinb <- pscl::zeroinfl(
    art ~ phd + ment + fem + mar + kid5 | ment + kid5 + fem,
    data = bioChemists,
    dist = "negbin"
)
zinb_specs <- list(
    response = c("phd", "ment", "fem", "mar", "kid5"),
    count = c("phd", "ment", "fem", "mar", "kid5"),
    zero = c("ment", "fem", "kid5")
)
zinb_fixture <- c(
    response = "zinb_response",
    count = "zinb_count_mean",
    zero = "zinb_zero_probability"
)
# The three component results agree numerically when aligned manually, but the
# equation-prefixed Stata labels do not yet provide stable semantic keys for an
# automated join. Keep them in the explicit exclusion audit below.

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

# Every generated fixture must be asserted above or explicitly classified
# here. These are retained as exploratory Stata output but are not equivalent
# cross-software assertions on this branch.
excluded <- c(
    "heckman_outcome_unconditional",
    "zinb_response",
    "zinb_count_mean",
    "zinb_zero_probability",
    "hurdle_positive_count",
    "logit_binary",
    "logit_binary_by_species",
    "logit_interaction_at",
    "poisson_weighted_robust",
    "logit_predictions_at",
    "logit_comparisons_at",
    "weibull_hazard_ratio",
    "weibull_mean_survival",
    "weibull_median_survival",
    "survey_logit",
    "survey_logit_by_cyl"
)
expect_equal(
    sort(unique(golden$fixture)),
    sort(union(tested, excluded)),
    info = "every misc.csv fixture is tested or explicitly excluded"
)
