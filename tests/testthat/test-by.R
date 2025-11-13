# Load optional packages quietly
suppressPackageStartupMessages({
    library(marginaleffects)
    if (requireNamespace("margins", quietly = TRUE)) library(margins, quietly = TRUE)
    if (requireNamespace("nnet", quietly = TRUE)) library(nnet, quietly = TRUE)
})

tol <- 1e-4
tol_se <- 1e-2


mod1 <- glm(gear ~ cyl + am, family = poisson, data = mtcars)
mod2 <- lm(gear ~ cyl + am, data = mtcars)
p1 <- predictions(mod1, by = "am")
p2 <- predictions(mod2, by = "am")
p3 <- predictions(mod2, by = "am", wts = mtcars$wt)
expect_true("conf.low" %in% colnames(p1))
expect_true("conf.low" %in% colnames(p2))
expect_equal(nrow(p1), nrow(p2), ignore_attr = TRUE)
expect_equal(nrow(p1), 2, ignore_attr = TRUE)


# use comparison to collapse into averages
mod <- glm(gear ~ cyl + am, family = poisson, data = mtcars)
x <- avg_comparisons(mod, comparison = "dydx")
y <- comparisons(mod, comparison = "dydxavg")
expect_equal(x$estimate, y$estimate, ignore_attr = TRUE)
expect_equal(x$std.error, y$std.error, tolerance = 1e-5, ignore_attr = TRUE)

x <- avg_comparisons(mod, comparison = "eyex")
y <- comparisons(mod, comparison = "eyexavg")
expect_equal(x$estimate, y$estimate, ignore_attr = TRUE)
expect_equal(x$std.error, y$std.error, tolerance = 1e-5, ignore_attr = TRUE)

x <- avg_comparisons(mod, comparison = "eydx")
y <- comparisons(mod, comparison = "eydxavg")
expect_equal(x$estimate, y$estimate, ignore_attr = TRUE)
expect_equal(x$std.error, y$std.error, tolerance = 1e-5, ignore_attr = TRUE)

x <- avg_comparisons(mod, comparison = "dyex")
y <- comparisons(mod, comparison = "dyexavg")
expect_equal(x$estimate, y$estimate, ignore_attr = TRUE)
expect_equal(x$std.error, y$std.error, tolerance = 1e-5, ignore_attr = TRUE)

x <- avg_slopes(mod, slope = "dyex")
y <- slopes(mod, slope = "dyexavg")
expect_equal(x$estimate, y$estimate, ignore_attr = TRUE)
expect_equal(x$std.error, y$std.error, tolerance = 1e-5, ignore_attr = TRUE)

# input sanity check
expect_error(slopes(mod, slope = "bad"), regexp = "eyexavg")

##### aggregate() refactor makes this possible again
# by is deprecated in `summary()` and `tidy()`
# expect_error(summary(comparisons(mod), by = "am"), regexp = "instead")
# expect_error(tidy(comparisons(mod), by = "am"), regexp = "instead")

# by argument
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
cmp <- comparisons(mod, by = "am", comparison = "lnor")
expect_equal(nrow(cmp), 4)


# counterfactual margins at()
if (requireNamespace("margins", quietly = TRUE)) {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    mod <- lm(mpg ~ factor(cyl) * hp + wt, data = dat)
    mar <- margins(mod, at = list(cyl = unique(dat$cyl)))
    mar <- data.frame(summary(mar))
    mfx <- slopes(
        mod,
        by = "cyl",
        newdata = datagrid(cyl = c(4, 6, 8), grid_type = "counterfactual")
    )
    mfx <- mfx[order(mfx$term, mfx$contrast), ]
    expect_equal(mfx$estimate, mar$AME, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mar$SE, tolerance = 1e6, ignore_attr = TRUE)
}


# issue #434 by with character precitors
dat <- get_dataset("Affairs", "AER")
mod <- glm(
    affairs ~ children + gender + yearsmarried,
    family = poisson,
    data = dat
)
p <- predictions(mod, by = "children")
expect_equal(nrow(p), 2, ignore_attr = TRUE)
expect_false(anyNA(p$estimate))


# Issue #445: by data frame to collapse response levels
if (requireNamespace("nnet", quietly = TRUE)) {
    mod <- nnet::multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)

    expect_error(predictions(mod, type = "probs", by = "response"), regexp = "Character vector")
    expect_error(predictions(mod, type = "probs", by = mtcars), regexp = "Character vector")

    p <- predictions(mod, type = "probs", by = "group")
    expect_equal(nrow(p), 3, ignore_attr = TRUE)
    cmp <- comparisons(mod, type = "probs", by = "group")
    expect_equal(nrow(cmp), 9, ignore_attr = TRUE)

    # Important: we cannot deprecate `by` as data.frame because `group`
    # is not present in `newdata`, so we can't always merge beforehand.
    by <- data.frame(
        group = c("3", "4", "5"),
        by = c("(3,4)", "(3,4)", "(5)")
    )
    p1 <- predictions(mod, type = "probs")
    p2 <- predictions(mod, type = "probs", by = by)
    p3 <- predictions(mod, type = "probs", by = by, hypothesis = ~sequential)
    p4 <- predictions(mod, type = "probs", by = by, hypothesis = ~reference)
    p5 <- predictions(mod, type = "probs", by = c("am", "vs", "group"))
    expect_equal(mean(subset(p1, group == "5")$estimate), p2$estimate[2], ignore_attr = TRUE)
    expect_equal(p3$estimate, diff(p2$estimate), ignore_attr = TRUE)
    expect_equal(nrow(p4), 1, ignore_attr = TRUE)
    expect_equal(nrow(p5), 12, ignore_attr = TRUE)

    cmp <- comparisons(mod, type = "probs", by = "am")
    expect_equal(nrow(cmp), 18, ignore_attr = TRUE)

    cmp <- comparisons(
        mod,
        variables = "am",
        by = by,
        type = "probs"
    )
    expect_equal(nrow(cmp), 2, ignore_attr = TRUE)

    cmp <- comparisons(
        mod,
        variables = "am",
        by = by,
        hypothesis = ~sequential,
        type = "probs"
    )
    expect_equal(nrow(cmp), 1, ignore_attr = TRUE)
}


# Issue #481: warning on missing by categories
if (requireNamespace("nnet", quietly = TRUE)) {
    options(marginaleffects_safe = TRUE)
    mod <- nnet::multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
    by <- data.frame(
        by = c("4", "5"),
        group = 4:5
    )
    expect_warning(comparisons(mod, variables = "mpg", newdata = "mean", by = by))
    expect_warning(predictions(mod, newdata = "mean", by = by))
    options(marginaleffects_safe = FALSE)
}


# Issue #589: easy marginalization
mod <- lm(mpg ~ factor(gear) + am, mtcars)
cmp1 <- comparisons(mod, by = TRUE)
cmp2 <- comparisons(mod, by = FALSE)
expect_equal(nrow(cmp1), 3, ignore_attr = TRUE)
expect_equal(nrow(cmp2), 96, ignore_attr = TRUE)

pre1 <- predictions(mod, by = TRUE)
pre2 <- predictions(mod, by = FALSE)
expect_equal(nrow(pre1), 1, ignore_attr = TRUE)
expect_equal(nrow(pre2), 32, ignore_attr = TRUE)

pre1 <- slopes(mod, by = TRUE)
pre2 <- slopes(mod, by = FALSE)
expect_equal(nrow(pre1), 3, ignore_attr = TRUE)
expect_equal(nrow(pre2), 96, ignore_attr = TRUE)


# marginaleffects poisson vs. margins
if (requireNamespace("margins", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)) {
    dat <- mtcars
    mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
    mfx <- avg_slopes(
        mod,
        by = c("cyl", "am"),
        newdata = datagrid(
            cyl = unique,
            am = unique,
            grid_type = "counterfactual"
        )
    ) |>
        dplyr::arrange(term, cyl, am)
    mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
    mar <- summary(mar)
    # margins doesn't treat the binary am as binary automatically
    expect_equal(mfx$estimate[7:12], mar$AME[7:12], tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx$std.error[7:12], mar$SE[7:12], tolerance = tol_se, ignore_attr = TRUE)
}


# comparisons poisson vs. margins
if (requireNamespace("margins", quietly = TRUE)) {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
    mfx <- comparisons(
        mod,
        by = c("cyl", "am"),
        newdata = datagrid(
            cyl = unique,
            am = unique,
            grid_type = "counterfactual"
        )
    )

    mfx <- tidy(mfx)

    mfx <- mfx[order(mfx$term, mfx$contrast, mfx$cyl, mfx$am), ]
    mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
    mar <- summary(mar)
    expect_equal(mfx$estimate, mar$AME, tolerance = tol, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mar$SE, tolerance = tol_se, ignore_attr = TRUE)
}


# Issue #715: incorrect grouping with custom `comparison` function
if (requireNamespace("dplyr", quietly = TRUE)) {
    dat <- transform(mtcars, vs = vs, am = as.factor(am), cyl = as.factor(cyl))
    mod <- lm(mpg ~ qsec + am + cyl, dat)
    fun <- \(hi, lo) mean(hi) / mean(lo)
    cmp1 <- comparisons(mod, variables = "cyl", comparison = fun, by = "am") |>
        dplyr::arrange(am, contrast)
    cmp2 <- comparisons(mod, variables = "cyl", comparison = "ratioavg", by = "am") |>
        dplyr::arrange(am, contrast)
    expect_equal(cmp1$estimate, cmp2$estimate, ignore_attr = TRUE)
    expect_equal(cmp1$std.error, cmp2$std.error, ignore_attr = TRUE)
    expect_equal(nrow(cmp1), 4)
}


# https://stackoverflow.com/questions/75858227/in-rs-marginaleffects-package-why-do-these-two-methods-shows-different-results
if (requireNamespace("dplyr", quietly = TRUE)) {
    tmp <- mtcars %>% transform(am = factor(am), cyl = factor(cyl), mpg = mpg)
    mod <- lm(mpg ~ am * cyl, data = tmp)
    cmp1 <- avg_comparisons(mod, variables = "am", by = "cyl") |>
        dplyr::arrange(cyl)
    cmp2 <- comparisons(mod, variables = "am") %>%
        dplyr::group_by(cyl) %>%
        dplyr::summarize(estimate = mean(estimate), .groups = "keep") |>
        dplyr::ungroup()
    cmp3 <- predictions(mod) |>
        aggregate(estimate ~ am + cyl, FUN = mean) |>
        aggregate(estimate ~ cyl, FUN = diff)
    expect_equal(cmp1$estimate, cmp2$estimate, tolerance = tol, ignore_attr = TRUE)
    expect_equal(cmp1$estimate, cmp3$estimate, tolerance = tol, ignore_attr = TRUE)
}


# Issue #1058
tmp <- mtcars
tmp <- tmp[c("mpg", "cyl", "hp")]
tmp$cyl <- as.factor(tmp$cyl) # 3 levels
tmp$hp <- as.factor(tmp$hp)
bygrid <- datagrid(newdata = tmp, by = "cyl", hp = unique)
expect_equal(nrow(bygrid), 23, ignore_attr = TRUE)
