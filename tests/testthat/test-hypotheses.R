

# When `FUN` and `hypotheses` are `NULL`, `hypotheses()` returns a data.frame of parameters
dat <- mtcars
mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)
dmm <- hypotheses(mod)
expect_s3_class(dmm, "data.frame")

# Test of equality between coefficients
if (requireNamespace("car", quietly = TRUE)) {
    dmm <- hypotheses(mod, "hp = wt")
    dmc <- car::linearHypothesis(mod, hypothesis = "hp = wt")
    expect_equal(dmm$estimate, attr(dmc, "value")[[1]], ignore_attr = TRUE)
    expect_equal(dmm$std.error, sqrt(attr(dmc, "vcov")[[1]]), tolerance = 1e-5, ignore_attr = TRUE)
}

# Non-linear function
dmm <- hypotheses(mod, "exp(hp + wt) = 0.1")
expect_s3_class(dmm, "data.frame")

# Robust standard errors
dmm <- hypotheses(mod, "hp = wt", vcov = "HC3")
expect_s3_class(dmm, "data.frame")

# b1, b2, ... shortcuts can be used to identify rows in the output of FUN
dmm <- hypotheses(mod, "b2 = b3") |> suppressWarnings() # first time calling issues a warning
expect_s3_class(dmm, "data.frame")

# term names with special characters have to be enclosed in backticks
dmm <- hypotheses(mod, "`factor(cyl)6` = `factor(cyl)8`")
expect_s3_class(dmm, "data.frame")

# The `FUN` argument can be used to compute standard errors for fitted values
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

f <- function(x) predict(x, type = "link", newdata = mtcars)
p <- hypotheses(mod, hypothesis = f)
expect_s3_class(p, "data.frame")
expect_true(all(p$std.error > 0))

f <- function(x) predict(x, type = "response", newdata = mtcars)
p <- hypotheses(mod, hypothesis = f)
expect_s3_class(p, "data.frame")
expect_true(all(p$std.error > 0))

# equality between predictions: 1 and 2 equal, 2 and 3 different
fun <- function(x) {
    p <- predict(x, type = "link", newdata = mtcars)
    data.frame(term = "b1 = b2", estimate = p[1] - p[2])
}
dmm <- hypotheses(mod, hypothesis = fun)
expect_equal(dmm$estimate, 0, ignore_attr = TRUE)
fun <- function(x) {
    p <- predict(x, type = "link", newdata = mtcars)
    data.frame(term = "b3 = b2", estimate = p[3] - p[2])
}
dmm <- hypotheses(mod, hypothesis = fun)
expect_equal(dmm$estimate, 1.33154848763268, ignore_attr = TRUE)

# named matrix
mod <- lm(mpg ~ factor(cyl), data = mtcars)
hyp <- matrix(
    c(0, -1, 1, 1 / 3, 1 / 3, 1 / 3),
    ncol = 2,
    dimnames = list(NULL, c("H1", "H2"))
)
del <- hypotheses(mod, hypothesis = hyp)
expect_equal(del$term, c("H1", "H2"), ignore_attr = TRUE)


# two-step to check code where `hypotheses(model)` model is an object not a call
mod <- lm(mpg ~ factor(cyl), data = mtcars)
cmp <- avg_comparisons(mod)
hyp <- hypotheses(cmp, equivalence = c(-10, -5))
expect_s3_class(hyp, "hypotheses")


# Issue #656
if (requireNamespace("purrr", quietly = TRUE)) {
    reg_list <- list()
    reg_list[[1]] <- lm(mpg ~ wt + hp, data = mtcars)
    reg_list[[2]] <- lm(mpg ~ wt + hp + factor(vs), data = mtcars)
    expect_s3_class(hypotheses(reg_list[[1]]), "hypotheses")
    expect_s3_class(hypotheses(reg_list[[2]]), "hypotheses")
    h <- lapply(reg_list, hypotheses)
    expect_type(h, "list")
    expect_equal(length(h), 2, ignore_attr = TRUE)
    h <- purrr::map(reg_list, hypotheses)
    expect_type(h, "list")
    expect_equal(length(h), 2, ignore_attr = TRUE)

    cmp = lapply(reg_list, comparisons)
    tmp <- lapply(cmp, function(x) hypotheses(x, "b1 = b2"))
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "hypotheses")
    expect_s3_class(tmp[[2]], "hypotheses")
    tmp <- purrr::map(cmp, hypotheses, hypothesis = "b1 = b2")
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "hypotheses")
    expect_s3_class(tmp[[2]], "hypotheses")

    mfx = lapply(reg_list, avg_slopes)
    tmp <- lapply(mfx, function(x) hypotheses(x, "b1 = b2"))
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "hypotheses")
    expect_s3_class(tmp[[2]], "hypotheses")
    tmp <- purrr::map(mfx, hypotheses, hypothesis = "b1 = b2")
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "hypotheses")
    expect_s3_class(tmp[[2]], "hypotheses")

    pre = lapply(reg_list, predictions)
    tmp <- lapply(pre, function(x) hypotheses(x, "b1 = b2"))
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "hypotheses")
    expect_s3_class(tmp[[2]], "hypotheses")
    tmp <- purrr::map(pre, hypotheses, hypothesis = "b1 = b2")
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "hypotheses")
    expect_s3_class(tmp[[2]], "hypotheses")
    tmp <- purrr::map(reg_list, ~ hypotheses(.) |> tidy()) # error in Github version; works in CRAN version
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "tbl_df")
    expect_s3_class(tmp[[2]], "tbl_df")
    tmp <- purrr::map(reg_list, function(reg) {
        reg |>
            hypotheses("wt = 0") |>
            broom::tidy()
    })
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "tbl_df")
    expect_s3_class(tmp[[2]], "tbl_df")
    tmp <- purrr::map(reg_list, function(reg) tidy(hypotheses(reg, "wt = 0")))
    expect_type(tmp, "list")
    expect_s3_class(tmp[[1]], "tbl_df")
    expect_s3_class(tmp[[2]], "tbl_df")
}


# Issue #776: sort before hypothesis
load(test_path("../../inst/tinytest/modelarchive/data/gusto.rda"))
mod = glm(
    day30 ~ tx * sex + age,
    family = "binomial",
    data = gusto
)
cmp = avg_comparisons(
    mod,
    type = "link",
    variables = list("tx" = "pairwise"),
    by = "sex"
)
x <- hypotheses(cmp, hypothesis = "b4 - b3 = 0")
y <- cmp$estimate[4] - cmp$estimate[3]
z <- avg_comparisons(
    mod,
    type = "link",
    variables = list("tx" = "pairwise"),
    by = "sex",
    hypothesis = "b4 - b3 = 0"
) |> suppressWarnings()
expect_equal(x$estimate, y, ignore_attr = TRUE)
expect_equal(z$estimate, y, ignore_attr = TRUE)


# labels
dat <- mtcars
mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)
hyp <- hypotheses(mod, hypothesis = "b* = b2")
known <- c("b1=b2", "b2=b2", "b3=b2", "b4=b2", "b5=b2")
expect_true(all(hyp$hypothesis %in% known))

# Custom labels
hyp <- hypotheses(mod, hypothesis = c("equal" = "b1 = b2", "sums to zero" = "wt + hp = 0"))
expect_equal(hyp$hypothesis, c("equal", "sums to zero"), ignore_attr = TRUE)

# Does not mess up *
hyp <- hypotheses(mod, hypothesis = c("equal" = "b*=0"))
expect_equal(hyp$hypothesis, sprintf("b%s=0", 1:5), ignore_attr = TRUE)


# Issue #960
if (requireNamespace("nlme", quietly = TRUE)) {
    options(marginaleffects_safe = TRUE)
    fm1 <- nlme::lme(distance ~ age + Sex, data = nlme::Orthodont)
    expect_warning(hypotheses(fm1, hypothesis = c(0, 0), joint = c("SexFemale", "age")))
    # no warning generated
    h <- hypotheses(fm1, hypothesis = c(0, 0), df = c(1, 3), joint = c("SexFemale", "age"))
    options(marginaleffects_safe = FALSE)
}


# Issue #1344
mod <- lm(mpg ~ cyl * am * hp, mtcars)
helmert <<- function(x) {
    w <- contr.helmert(length(x))
    setNames(
        as.vector(x %*% w),
        nm = paste0("h-", seq_len(ncol(w)))
    )
}
h <- avg_slopes(mod, variables = c("hp"), by = c("cyl", "am"), newdata = "balanced")
h <- hypotheses(h, hypothesis = ~ I(helmert(x)) | am)
expect_true("am" %in% colnames(h))
expect_s3_class(h, "hypotheses")


# Sequential calls
mod <- lm(mpg ~ factor(cyl), mtcars)
p1 <- predictions(mod, by = "cyl", hypothesis = c("(b3 - b2) - (b2 - b1) = 0"))
p2 <- predictions(mod, by = "cyl", hypothesis = c("b2 - b1 = 0", "b3 - b2 = 0"))
p2 <- hypotheses(p2, "b2 - b1 = 0")
p3 <- predictions(mod, by = "cyl")
p3 <- hypotheses(p3, hypothesis = c("b2 - b1 = 0", "b3 - b2 = 0"))
p3 <- hypotheses(p3, "b2 - b1 = 0")
expect_equal(p1$estimate, p2$estimate, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(p1$estimate, p3$estimate, tolerance = 1e-6, ignore_attr = TRUE)
expect_equal(p1$std.error, p2$std.error, ignore_attr = TRUE)
expect_equal(p1$std.error, p3$std.error, tolerance = 1e-5, ignore_attr = TRUE)


# vignette: raw data.frame
draw <- function(i) {
    x <- rnorm(n = 10000, mean = 0, sd = 1)
    out <- data.frame(median = median(x), mean = mean(x))
    return(out)
}
sims <- do.call("rbind", lapply(1:25, draw))
coeftable <- data.frame(
    term = c("median", "mean"),
    estimate = c(mean(sims$median), mean(sims$mean))
)
vcov <- cov(sims)
h <- hypotheses(
    coeftable,
    vcov = vcov,
    hypothesis = "median = mean"
)
expect_s3_class(h, "hypotheses")


# Issue #1624: one-sided p-values should be consistent when testing hypotheses
# together vs separately
mod <- lm(Infant.Mortality ~ ., data = swiss)
h1 <- hypotheses(mod, "b1<=0", df = mod$df)
h2 <- hypotheses(mod, c("b1<=0", "b2<=0"), df = mod$df)
h3 <- hypotheses(mod, "b2<=0", df = mod$df)
# p-value for b1<=0 should be the same whether tested alone or with b2
expect_equal(h1$p.value[1], h2$p.value[1], tolerance = 1e-6)
# p-value for b2<=0 should be the same whether tested alone or with b1
expect_equal(h3$p.value[1], h2$p.value[2], tolerance = 1e-6)
# Test mixed operators work correctly
h4 <- hypotheses(mod, c("b1<=0", "b2>=0"), df = mod$df)
expect_equal(h1$p.value[1], h4$p.value[1], tolerance = 1e-6)
# b2>=0 should give the complement p-value of b2<=0
expect_equal(h3$p.value[1], 1 - h4$p.value[2], tolerance = 1e-6)
