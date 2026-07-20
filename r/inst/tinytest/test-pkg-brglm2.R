source("helpers.R")
using("marginaleffects")

requiet("brglm2")
requiet("margins")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- glm(am ~ mpg + wt, data = mtcars, family = binomial(), method = "brglm_fit")
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# brglmFit is produced by glm(method = brglm2::brglmFit) and inherits
# predict.glm(). Validate a noncanonical inverse-link derivative after weighted
# grouping and a linear hypothesis.
mod_analytic <- glm(
    am ~ hp * wt + factor(cyl),
    data = mtcars,
    family = binomial("probit"),
    method = brglm2::brglmFit
)
newdata <- mtcars[1:12, , drop = FALSE]
X <- marginaleffects:::get_model_matrix(mod_analytic, newdata)
expect_equivalent(
    drop(X %*% coef(mod_analytic)),
    as.numeric(predict(mod_analytic, newdata = newdata, type = "link"))
)
pred_analytic <- predictions(mod_analytic, newdata = newdata, type = "response")
old_option <- options(marginaleffects_analytic_jacobian = FALSE)
pred_fallback <- predictions(mod_analytic, newdata = newdata, type = "response")
options(old_option)
expect_equivalent(pred_analytic$estimate, pred_fallback$estimate)
expect_equivalent(
    components(pred_analytic, "jacobian"),
    components(pred_fallback, "jacobian"),
    tolerance = 1e-5
)

cmp_analytic <- avg_comparisons(
    mod_analytic,
    variables = "hp",
    by = "cyl",
    wts = "wt",
    hypothesis = matrix(c(1, -1, 0), ncol = 1),
    type = "response"
)
old_option <- options(marginaleffects_analytic_jacobian = FALSE)
cmp_fallback <- avg_comparisons(
    mod_analytic,
    variables = "hp",
    by = "cyl",
    wts = "wt",
    hypothesis = matrix(c(1, -1, 0), ncol = 1),
    type = "response"
)
options(old_option)
expect_equivalent(cmp_analytic$estimate, cmp_fallback$estimate)
expect_equivalent(
    components(cmp_analytic, "jacobian"),
    components(cmp_fallback, "jacobian"),
    tolerance = 1e-5
)

assign("brglm_fd_calls", 0L, envir = .GlobalEnv)
suppressMessages(invisible(utils::capture.output(
    trace(
        "get_jacobian_fdforward",
        where = asNamespace("marginaleffects"),
        tracer = quote({
            .GlobalEnv$brglm_fd_calls <- .GlobalEnv$brglm_fd_calls + 1L
        }),
        print = FALSE
    )
)))
tryCatch(
    {
        avg_comparisons(mod_analytic, variables = "hp", type = "response")
        expect_equal(.GlobalEnv$brglm_fd_calls, 0L)

        old_option <- options(marginaleffects_analytic_jacobian = FALSE)
        tryCatch(
            avg_comparisons(mod_analytic, variables = "hp", type = "response"),
            finally = options(old_option)
        )
        expect_equal(.GlobalEnv$brglm_fd_calls, 1L)
    },
    finally = {
        suppressMessages(invisible(utils::capture.output(
            untrace(
                "get_jacobian_fdforward",
                where = asNamespace("marginaleffects")
            )
        )))
        rm("brglm_fd_calls", envir = .GlobalEnv)
    }
)

# brglm2::brglm_fit vs. margins vs. emtrends
data("endometrial", package = "brglm2", envir = environment())
dat <<- endometrial
model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = dat)
model <- update(model, method = "brglm_fit") # probably breaks get_data from environemnt


# margins
mar <- margins(model)
mfx <- slopes(model, newdata = dat)
expect_slopes(model, newdata = dat)
expect_margins(mar, mfx)
# emtrends
em <- emtrends(model, ~PI, "PI", at = list(PI = 15, EH = 2, NV = 0))
em <- tidy(em)
mfx <- slopes(
    model,
    variables = "PI",
    newdata = datagrid(PI = 15, EH = 2, NV = 0),
    type = "link"
)
expect_equivalent(mfx$estimate, em$PI.trend)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)


# predictions: brglm2::brglm_fit: no validity
data("endometrial", package = "brglm2", envir = environment())
dat <- endometrial
model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = dat)
model <- update(model, method = "brglm_fit")
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(endometrial))
expect_predictions(model, n_row = nrow(endometrial))
expect_predictions(model, newdata = head(endometrial), n_row = 6)


# brmultinom: no validity
data("housing", package = "MASS")
dat <<- housing
mod <- brmultinom(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, type = "ML", ref = 1)
expect_slopes(mod, type = "probs")
expect_predictions(mod, type = "probs")


# bracl: no validity
data("stemcell", package = "brglm2")
dat <- stemcell
dat$religion <- as.numeric(dat$religion)
mod <- bracl(
    research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = dat,
    type = "ML"
)
expect_predictions(mod, type = "probs")
expect_slopes(mod, type = "probs")


# brglm2::brglm_fit vs. margins
tmp <<- data.frame(
    freq = c(15, 16, 16, 27, 33, 20, 21, 18, 26, 41, 38, 27, 29, 21, 33, 60, 41, 42),
    dose = rep(c(0, 10, 33, 100, 333, 1000), 3),
    observation = rep(1:3, each = 6)
)
model <- brnb(
    freq ~ dose + log(dose + 10),
    data = tmp,
    link = "log",
    transformation = "inverse",
    type = "ML"
)
expect_slopes(model, n_unique = 6, newdata = tmp)
mfx <- suppressWarnings(slopes(model))
mar <- suppressWarnings(margins(model))
expect_margins(mar, mfx)
