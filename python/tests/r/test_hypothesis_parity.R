source(here::here("tests/r/load.R"))

# R reference for every kind of `hypothesis`: formula shortcuts (grouped and
# not), row labels, equation strings, weights, functions, null values, and the
# inputs R rejects. Read by tests/test_hypothesis_parity.py, which runs the
# same cases with the same IDs on the same vendored CSV.
dat <- fread(here("tests/data/mtcars.csv"), na.strings = c("NA", ""))
m1 <- lm(mpg ~ hp + wt + am, data = dat)
m2 <- glm(am ~ hp + wt, data = dat, family = binomial)

rhs_dr <- c("reference", "revreference", "sequential", "revsequential", "pairwise",
            "revpairwise", "meandev", "meanotherdev", "trt_vs_ctrl")
cases <- list()
for (r in rhs_dr) for (l in c("difference", "ratio")) {
    f <- as.formula(sprintf("%s ~ %s", l, r))
    g <- as.formula(sprintf("%s ~ %s | am", l, r))
    cases[[sprintf("F/%s~%s", l, r)]] <- local({f <- f; function() avg_predictions(m1, by = "carb", hypothesis = f)})
    cases[[sprintf("G/%s~%s|am", l, r)]] <- local({g <- g; function() avg_predictions(m1, by = c("am", "carb"), hypothesis = g)})
}
for (r in c("poly", "helmert")) {
    cases[[sprintf("F/~%s", r)]] <- local({r <- r; function() avg_predictions(m1, by = "carb", hypothesis = as.formula(paste("~", r)))})
    cases[[sprintf("G/~%s|am", r)]] <- local({r <- r; function() avg_predictions(m1, by = c("am", "carb"), hypothesis = as.formula(paste("~", r, "| am")))})
}
cases[["L/multicol"]] <- function() avg_predictions(m1, by = c("am", "gear"), hypothesis = ~reference)
cases[["L/nolabel"]] <- function() comparisons(m1, variables = "hp", newdata = datagrid(am = c(0, 1), wt = c(2, 3)), hypothesis = ~reference)
cases[["L/comparisons_by"]] <- function() avg_comparisons(m1, variables = c("hp", "wt"), by = "am", hypothesis = ~reference)
cases[["L/slopes_pairwise"]] <- function() avg_slopes(m1, variables = "hp", by = "gear", hypothesis = ~pairwise)
cases[["L/bare_string"]] <- function() avg_predictions(m1, by = "carb", hypothesis = "pairwise")
cases[["S/terms"]] <- function() hypotheses(m1, hypothesis = "hp = wt")
cases[["S/positional"]] <- function() hypotheses(m1, hypothesis = "b2 - b3 = 0")
cases[["S/nonlinear"]] <- function() hypotheses(m1, hypothesis = "exp(b2) = 1")
cases[["S/wildcard"]] <- function() hypotheses(m1, hypothesis = "b* = 0")
cases[["S/multiple"]] <- function() hypotheses(m1, hypothesis = c("b2 = 0", "b3 = b4"))
cases[["S/preds_positional"]] <- function() avg_predictions(m1, by = "carb", hypothesis = "b1 = b2")
cases[["S/glm"]] <- function() avg_comparisons(m2, variables = "hp", hypothesis = "b1 = 0")
cases[["M/vector"]] <- function() hypotheses(m1, hypothesis = c(0, 1, -1, 0))
cases[["M/matrix_nonames"]] <- function() avg_predictions(m1, by = "gear", hypothesis = cbind(c(1, -1, 0), c(1, 0, -1)))
cases[["M/matrix_names"]] <- function() {
    H <- cbind(a = c(1, -1, 0), b = c(1, 0, -1))
    avg_predictions(m1, by = "gear", hypothesis = H)
}
cases[["N/null"]] <- function() avg_predictions(m1, by = "gear", hypothesis = 20)
cases[["X/function"]] <- function() {
    avg_predictions(m1, by = "gear", hypothesis = function(x) {
        data.frame(hypothesis = "max - min", estimate = max(x$estimate) - min(x$estimate))
    })
}

rows <- list()
meta <- list()
for (id in names(cases)) {
    res <- tryCatch(cases[[id]](), error = function(e) e)
    if (inherits(res, "error")) {
        meta[[id]] <- data.table(case = id, columns = "", error = TRUE)
        next
    }
    res <- as.data.table(res)
    lab <- if ("hypothesis" %in% names(res)) res$hypothesis else if ("term" %in% names(res)) as.character(res$term) else NA_character_
    grp <- if ("am" %in% names(res)) as.character(res$am) else NA_character_
    rows[[id]] <- data.table(case = id, i = seq_len(nrow(res)), group = grp, label = lab,
                             estimate = res$estimate,
                             std.error = if ("std.error" %in% names(res)) res$std.error else NA_real_)
    meta[[id]] <- data.table(case = id, columns = paste(names(res), collapse = "|"), error = FALSE)
}
fwrite(rbindlist(rows), here("tests/r/test_hypothesis_parity_rows.csv"))
fwrite(rbindlist(meta), here("tests/r/test_hypothesis_parity_meta.csv"))
