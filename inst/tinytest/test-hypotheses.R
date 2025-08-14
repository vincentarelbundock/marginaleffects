source("helpers.R")
using("marginaleffects")
requiet("car")


# When `FUN` and `hypotheses` are `NULL`, `hypotheses()` returns a data.frame of parameters
dat <- mtcars
mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)
dmm <- hypotheses(mod)
expect_inherits(dmm, "data.frame")

# Test of equality between coefficients
dmm <- hypotheses(mod, "hp = wt")
dmc <- car::linearHypothesis(mod, hypothesis = "hp = wt")
expect_equivalent(dmm$estimate, attr(dmc, "value")[[1]])
expect_equivalent(dmm$std.error, sqrt(attr(dmc, "vcov")[[1]]))

# Non-linear function
dmm <- hypotheses(mod, "exp(hp + wt) = 0.1")
expect_inherits(dmm, "data.frame")

# Robust standard errors
dmm <- hypotheses(mod, "hp = wt", vcov = "HC3")
expect_inherits(dmm, "data.frame")

# b1, b2, ... shortcuts can be used to identify rows in the output of FUN
dmm <- hypotheses(mod, "b2 = b3") # first time calling issues a warning
expect_inherits(dmm, "data.frame")

# term names with special characters have to be enclosed in backticks
dmm <- hypotheses(mod, "`factor(cyl)6` = `factor(cyl)8`")
expect_inherits(dmm, "data.frame")

# The `FUN` argument can be used to compute standard errors for fitted values
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

f <- function(x) predict(x, type = "link", newdata = mtcars)
p <- hypotheses(mod, hypothesis = f)
expect_inherits(p, "data.frame")
expect_true(all(p$std.error > 0))

f <- function(x) predict(x, type = "response", newdata = mtcars)
p <- hypotheses(mod, hypothesis = f)
expect_inherits(p, "data.frame")
expect_true(all(p$std.error > 0))

# equality between predictions: 1 and 2 equal, 2 and 3 different
fun <- function(x) {
    p <- predict(x, type = "link", newdata = mtcars)
    data.frame(term = "b1 = b2", estimate = p[1] - p[2])
}
dmm <- hypotheses(mod, hypothesis = fun)
expect_equivalent(dmm$estimate, 0)
fun <- function(x) {
    p <- predict(x, type = "link", newdata = mtcars)
    data.frame(term = "b3 = b2", estimate = p[3] - p[2])
}
dmm <- hypotheses(mod, hypothesis = fun)
expect_equivalent(dmm$estimate, 1.33154848763268)

# named matrix
mod <- lm(mpg ~ factor(cyl), data = mtcars)
hyp <- matrix(
    c(0, -1, 1, 1 / 3, 1 / 3, 1 / 3),
    ncol = 2,
    dimnames = list(NULL, c("H1", "H2"))
)
del <- hypotheses(mod, hypothesis = hyp)
expect_equivalent(del$term, c("H1", "H2"))


# two-step to check code where `hypotheses(model)` model is an object not a call
mod <- lm(mpg ~ factor(cyl), data = mtcars)
cmp <- avg_comparisons(mod)
hyp <- hypotheses(cmp, equivalence = c(-10, -5))
expect_inherits(hyp, "hypotheses")


# Issue #656
requiet("purrr")
reg_list <- list()
reg_list[[1]] <- lm(mpg ~ wt + hp, data = mtcars)
reg_list[[2]] <- lm(mpg ~ wt + hp + factor(vs), data = mtcars)
expect_inherits(hypotheses(reg_list[[1]]), "hypotheses")
expect_inherits(hypotheses(reg_list[[2]]), "hypotheses")
h <- lapply(reg_list, hypotheses)
expect_inherits(h, "list")
expect_equivalent(length(h), 2)
h <- purrr::map(reg_list, hypotheses)
expect_inherits(h, "list")
expect_equivalent(length(h), 2)

cmp = lapply(reg_list, comparisons)
tmp <- lapply(cmp, function(x) hypotheses(x, "b1 = b2"))
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "hypotheses")
expect_inherits(tmp[[2]], "hypotheses")
tmp <- purrr::map(cmp, hypotheses, hypothesis = "b1 = b2")
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "hypotheses")
expect_inherits(tmp[[2]], "hypotheses")

mfx = lapply(reg_list, avg_slopes)
tmp <- lapply(mfx, function(x) hypotheses(x, "b1 = b2"))
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "hypotheses")
expect_inherits(tmp[[2]], "hypotheses")
tmp <- purrr::map(mfx, hypotheses, hypothesis = "b1 = b2")
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "hypotheses")
expect_inherits(tmp[[2]], "hypotheses")

pre = lapply(reg_list, predictions)
tmp <- lapply(pre, function(x) hypotheses(x, "b1 = b2"))
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "hypotheses")
expect_inherits(tmp[[2]], "hypotheses")
tmp <- purrr::map(pre, hypotheses, hypothesis = "b1 = b2")
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "hypotheses")
expect_inherits(tmp[[2]], "hypotheses")
tmp <- purrr::map(reg_list, ~ hypotheses(.) |> tidy()) # error in Github version; works in CRAN version
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "tbl_df")
expect_inherits(tmp[[2]], "tbl_df")
tmp <- purrr::map(reg_list, function(reg) {
    reg |>
        hypotheses("wt = 0") |>
        broom::tidy()
})
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "tbl_df")
expect_inherits(tmp[[2]], "tbl_df")
tmp <- purrr::map(reg_list, function(reg) tidy(hypotheses(reg, "wt = 0")))
expect_inherits(tmp, "list")
expect_inherits(tmp[[1]], "tbl_df")
expect_inherits(tmp[[2]], "tbl_df")


# Issue #776: sort before hypothesis
load(testing_path("modelarchive/data/gusto.rda"))
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
)
expect_equivalent(x$estimate, y)
expect_equivalent(z$estimate, y)


# labels
dat <- mtcars
mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)
hyp <- hypotheses(mod, hypothesis = "b* = b2")
known <- c("b1=b2", "b2=b2", "b3=b2", "b4=b2", "b5=b2")
expect_true(all(hyp$hypothesis %in% known))

# Custom labels
hyp <- hypotheses(mod, hypothesis = c("equal" = "b1 = b2", "sums to zero" = "wt + hp = 0"))
expect_equivalent(hyp$hypothesis, c("equal", "sums to zero"))

# Does not mess up *
hyp <- hypotheses(mod, hypothesis = c("equal" = "b*=0"))
expect_equivalent(hyp$hypothesis, sprintf("b%s=0", 1:5))

# hypotheses() applied to {marginaleffects} package objects
# commented out because doesn't work in environments because of match.call()
# mod <- glm(vs ~ hp + am, data = mtcars, family = binomial)

# cmp <- comparisons(mod, by = "am")
# dm <- hypotheses(cmp, hypothesis = "b1 = b3")
# expect_true("b1=b3" %in% dm$term)
# expect_equivalent(nrow(dm), 1)

# mfx <- slopes(mod)
# dm <- hypotheses(mfx, hypothesis = "b1 = b3")
# expect_true("b1=b3" %in% dm$term)
# expect_equivalent(nrow(dm), 1)

# pre <- predictions(mod, newdata = datagrid())
# dm <- hypotheses(pre, hypothesis = "b1 = 0.05")
# expect_true("b1=0.05" %in% dm$term)
# expect_equivalent(nrow(dm), 1)

# Issue #960
requiet("nlme")
fm1 <- lme(distance ~ age + Sex, data = Orthodont)
options(marginaleffects_safe = TRUE)
expect_warning(hypotheses(fm1, hypothesis = c(0, 0), joint = c("SexFemale", "age")))
options(marginaleffects_safe = FALSE)
# no warning generated
h <- hypotheses(fm1, hypothesis = c(0, 0), df = c(1, 3), joint = c("SexFemale", "age"))


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
expect_inherits(h, "hypotheses")


# Sequential calls
mod <- lm(mpg ~ factor(cyl), mtcars)
p1 <- predictions(mod, by = "cyl", hypothesis = c("(b3 - b2) - (b2 - b1) = 0"))
p2 <- predictions(mod, by = "cyl", hypothesis = c("b2 - b1 = 0", "b3 - b2 = 0"))
p2 <- hypotheses(p2, "b2 - b1 = 0")
p3 <- predictions(mod, by = "cyl")
p3 <- hypotheses(p3, hypothesis = c("b2 - b1 = 0", "b3 - b2 = 0"))
p3 <- hypotheses(p3, "b2 - b1 = 0")
expect_equivalent(p1$estimate, p2$estimate)
expect_equivalent(p1$estimate, p3$estimate)
expect_equivalent(p1$std.error, p2$std.error)
expect_equivalent(p1$std.error, p3$std.error)
