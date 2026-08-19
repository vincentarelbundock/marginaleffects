source("helpers.R")
using("marginaleffects")

# Every entry in `comparison_function_dict` is exercised here, so the dictionary
# stays the single source of truth: a shorthand added there without a test shows
# up as a failure in the coverage assertions at the bottom of this file.
dict <- marginaleffects:::comparison_function_dict

# The `*avgwts` entries are internal. sanitize_comparison() rejects them as user
# input; they are reached by passing the matching `*avg` shorthand together with
# `wts`, which is what the `weighted = TRUE` half of the grid below does.
internal <- grep("avgwts$", names(dict), value = TRUE)
public <- setdiff(names(dict), internal)

# A logit keeps every prediction strictly inside (0, 1), which lnor and lnratio
# need, and non-constant weights make the weighted branch differ from the
# unweighted one.
set.seed(1024)
dat <- transform(mtcars, w = runif(32, 0.5, 2))
mod <- glm(am ~ hp + wt, data = dat, family = binomial)

covered <- character()

for (shorthand in public) {
    for (weighted in c(FALSE, TRUE)) {
        args <- list(mod, variables = "hp", comparison = shorthand, newdata = dat)
        if (isTRUE(weighted)) args$wts <- "w"
        info <- sprintf("%s (wts = %s)", shorthand, weighted)

        cmp <- do.call(avg_comparisons, args)
        expect_inherits(cmp, "comparisons")
        expect_equal(nrow(cmp), 1L, info = paste(info, "nrow"))
        expect_true(is.finite(cmp$estimate), info = paste(info, "estimate is finite"))
        expect_true(is.finite(cmp$std.error), info = paste(info, "std.error is finite"))
        expect_true(cmp$std.error > 0, info = paste(info, "std.error is positive"))

        # The analytic Jacobian must reproduce the numeric one. The bound is
        # loose because the reference is itself a finite difference: the spread
        # across this grid is ~1e-4, while a wrong Jacobian is off by orders of
        # magnitude rather than by a few ulps.
        old_option <- options(marginaleffects_analytic_jacobian = FALSE)
        numeric_se <- do.call(avg_comparisons, args)$std.error
        options(old_option)
        expect_equal(
            cmp$std.error,
            numeric_se,
            tolerance = 1e-3,
            scale = numeric_se,
            info = paste(info, "analytic vs numeric jacobian")
        )

        covered <- union(covered, shorthand)
    }
}

# Weighted averaging identity. For the shorthands that average a row-wise
# quantity, the weighted estimate must equal the weighted mean of the unweighted
# row-wise estimates. This is what caught the elasticities: `eyex`, `eydx` and
# `dyex` returned NA under `wts` because prepare_elasticities() did not
# recognise the `*avgwts` names, so the response and elasticity columns their
# comparison functions require were never built.
#
# The identity does not extend to ratio/lnratio/lnor/lift, which are ratios of
# weighted means rather than means of row-wise ratios.
rowwise <- c("difference", "dydx", "eyex", "eydx", "dyex", "expdydx")
for (shorthand in rowwise) {
    wtd <- avg_comparisons(
        mod, variables = "hp", comparison = shorthand, newdata = dat, wts = "w")
    rows <- comparisons(
        mod, variables = "hp", comparison = shorthand, newdata = dat)
    expect_equal(nrow(rows), nrow(dat), info = paste(shorthand, "row-wise nrow"))
    expect_equal(
        wtd$estimate,
        weighted.mean(rows$estimate, dat$w),
        info = paste(shorthand, "weighted mean identity")
    )
}

# Coverage. The loop only names the public shorthands, so assert that running it
# with `wts` really does reach every internal entry, rather than assuming it.
expect_equal(
    sort(paste0(grep("avg$", public, value = TRUE), "wts")),
    sort(internal),
    info = "every internal *avgwts entry is reachable from a tested *avg shorthand"
)
expect_equal(
    sort(covered),
    sort(public),
    info = "every public comparison shorthand is tested"
)
