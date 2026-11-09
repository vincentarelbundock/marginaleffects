# Extract the coefficient vector of a strictly linear hypothesis expression,
# by structural recursion on its syntax tree.
#
# This is a proof, not a probe: a parse tree built only from estimate
# references, numeric constants, sums and differences, products with a
# constant side, and division by a constant denotes a linear map as a matter
# of syntax, wherever it is evaluated. Anything else -- unknown symbols,
# function calls, a product of two estimate terms -- yields NULL. No finite
# set of numeric evaluations can establish this property for arbitrary code,
# because a function can be built to agree with any fixed evaluation points
# and disagree elsewhere; the syntactic argument has no such gap. And the
# same structural walk which proves the map linear also reads off its
# coefficients, so the contrast matrix comes from the proof itself rather
# than from re-evaluating the closure against basis vectors.
#
# Returns a list with a `coef` vector of length `n_estimates` aligned to
# estimate positions and the additive `const`, or NULL when the expression is
# not a valid combination. An affine map is promoted as a matrix plus an
# offset: the estimates are crossprod(H, x) + offset, and the derivative is
# H alone, independent of the constant. Differentiating the constant
# numerically instead is catastrophic -- a large offset cancels the probe
# step and reports a zero standard error with full confidence.
#
# `labels` and `idx` are parallel: `labels[k]` is the symbol which stands for
# estimate position `idx[k]`.
hypothesis_expression_coefficients <- function(expr, labels, idx, n_estimates) {
    position <- stats::setNames(as.integer(idx), labels)

    const_value <- function(e) {
        v <- tryCatch(eval(e, baseenv()), error = function(err) NULL)
        if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
            return(v)
        }
        NULL
    }

    walk <- function(e) {
        if (is.numeric(e) && length(e) == 1L && is.finite(e)) {
            return(list(const = as.numeric(e), coef = numeric(n_estimates)))
        }
        if (is.symbol(e)) {
            pos <- position[[as.character(e)]]
            if (is.null(pos) || is.na(pos)) {
                return(NULL)
            }
            coef <- numeric(n_estimates)
            coef[pos] <- 1
            return(list(const = 0, coef = coef))
        }
        if (!is.call(e) || length(e) < 2L || !is.symbol(e[[1L]])) {
            return(NULL)
        }
        op <- as.character(e[[1L]])
        if (op == "(" && length(e) == 2L) {
            return(walk(e[[2L]]))
        }
        if (op %in% c("+", "-") && length(e) == 2L) {
            a <- walk(e[[2L]])
            if (is.null(a)) {
                return(NULL)
            }
            if (op == "-") {
                a$const <- -a$const
                a$coef <- -a$coef
            }
            return(a)
        }
        if (op %in% c("+", "-") && length(e) == 3L) {
            a <- walk(e[[2L]])
            b <- walk(e[[3L]])
            if (is.null(a) || is.null(b)) {
                return(NULL)
            }
            s <- if (op == "+") 1 else -1
            return(list(
                const = a$const + s * b$const,
                coef = a$coef + s * b$coef
            ))
        }
        if (op == "*" && length(e) == 3L) {
            a <- walk(e[[2L]])
            b <- walk(e[[3L]])
            if (is.null(a) || is.null(b)) {
                return(NULL)
            }
            a_const <- all(a$coef == 0)
            b_const <- all(b$coef == 0)
            if (a_const && b_const) {
                return(list(const = a$const * b$const, coef = a$coef))
            }
            if (a_const) {
                return(list(const = a$const * b$const, coef = a$const * b$coef))
            }
            if (b_const) {
                return(list(const = a$const * b$const, coef = b$const * a$coef))
            }
            return(NULL)
        }
        if (op == "/" && length(e) == 3L) {
            a <- walk(e[[2L]])
            b <- walk(e[[3L]])
            if (is.null(a) || is.null(b) || !all(b$coef == 0) || b$const == 0) {
                return(NULL)
            }
            return(list(const = a$const / b$const, coef = a$coef / b$const))
        }
        if (op == "^" && length(e) == 3L) {
            a <- walk(e[[2L]])
            b <- walk(e[[3L]])
            if (
                is.null(a) || is.null(b) ||
                    !all(a$coef == 0) || !all(b$coef == 0)
            ) {
                return(NULL)
            }
            v <- a$const^b$const
            if (!is.finite(v)) {
                return(NULL)
            }
            return(list(const = v, coef = a$coef))
        }
        NULL
    }

    exprs <- as.list(expr)
    if (length(exprs) != 1L) {
        return(NULL)
    }
    e <- exprs[[1L]]
    # Top-level "lhs = rhs" is the null-hypothesis form: the closure
    # computing the estimates evaluates "lhs - (rhs)", so a constant rhs
    # folds into the affine constant.
    rhs <- 0
    if (
        is.call(e) && length(e) == 3L && is.symbol(e[[1L]]) &&
            identical(as.character(e[[1L]]), "=")
    ) {
        rhs <- const_value(e[[3L]])
        if (is.null(rhs)) {
            return(NULL)
        }
        e <- e[[2L]]
    }
    out <- walk(e)
    if (is.null(out) || !all(is.finite(out$coef)) || !is.finite(out$const)) {
        return(NULL)
    }
    out$const <- out$const - rhs
    if (!is.finite(out$const)) {
        return(NULL)
    }
    out
}



# ---------------------------------------------------------------------------
# Compile a `hypothesis` once, never interpret it
# ---------------------------------------------------------------------------
#
# Every kind of `hypothesis` (numeric vector or matrix, string, formula,
# function) is compiled exactly once, against the estimates it reads, into a
# single stage object:
#
#   kind      "matrix" when the stage carries an exact contrast matrix `H`
#             (explicit matrices, affine strings, linear formula shortcuts);
#             otherwise "string", "formula", or "function".
#   source    the kind of `hypothesis` the user supplied.
#   template  the non-estimate columns of the output (`term`, `hypothesis`,
#             group columns...), in display order, with an `estimate`
#             placeholder.
#   apply     maps the estimates to the hypothesis estimates. It accepts a
#             numeric vector, or a matrix with one row per estimate and one
#             column per posterior draw (all kinds except "function").
#   H, offset optional: apply(est) == crossprod(H, est) + offset, so the
#             derivative of the stage is exactly H.
#   pullback  optional: structured exact pullback for dense linear shortcuts.
#
# The displayed estimates are `apply()` of the estimates the stage was
# compiled against (see `hypothesis_frame()`), the plan replay calls the same
# `apply()` to differentiate, and the Bayesian path calls it on the draws
# matrix. There is no second implementation for the results to drift from.
hypothesis_compile <- function(
    hypothesis,
    cmp_skeleton,
    by = NULL,
    newdata = NULL,
    mfx = NULL,
    draws = NULL) {
    hypothesis_check_deprecated(hypothesis)

    if (is.null(hypothesis)) {
        return(list(cmp = cmp_skeleton, hyp = NULL))
    }

    vec <- isTRUE(checkmate::check_atomic_vector(hypothesis)) &&
        isTRUE(checkmate::check_numeric(hypothesis))
    mat <- isTRUE(checkmate::check_matrix(hypothesis))

    if (isTRUE(checkmate::check_formula(hypothesis))) {
        hyp <- hypothesis_compile_formula(hypothesis, cmp_skeleton, by = by, newdata = newdata, mfx = mfx)
    } else if (isTRUE(checkmate::check_function(hypothesis))) {
        hyp <- hypothesis_compile_function(hypothesis, cmp_skeleton, by = by, newdata = newdata, draws = draws)
    } else if (vec || mat) {
        hyp <- hypothesis_compile_matrix(hypothesis, cmp_skeleton)
    } else if (is.character(hypothesis)) {
        hyp <- hypothesis_compile_string(hypothesis, cmp_skeleton)
    } else {
        stop(
            "`hypothesis` is broken. Please report this bug with a reproducible example: https://github.com/vincentarelbundock/marginaleffects/issues.",
            call. = FALSE
        )
    }

    if (identical(hyp$source, "function")) {
        # The user function already ran once to produce the labels; its output
        # is the displayed result.
        cmp <- hyp$template
    } else {
        cmp <- hypothesis_frame(hyp, hyp$apply(cmp_skeleton[["estimate"]]))
    }
    list(cmp = cmp, hyp = hyp)
}


hypothesis_check_deprecated <- function(hypothesis) {
    deprecated <- c(
        "pairwise",
        "revpairwise",
        "sequential",
        "revsequential",
        "reference",
        "meandev",
        "meanotherdev",
        "revreference"
    )
    if (isTRUE(checkmate::check_choice(hypothesis, deprecated))) {
        msg <- "This string is deprecated for use in the `hypothesis` argument. Use the formula interface instead. Ex: `hypothesis=~reference`"
        stop(msg, call. = FALSE)
    }
    invisible(NULL)
}


# The output rows of a compiled stage for a given vector of hypothesis
# estimates: the label template with the estimates filled in.
hypothesis_frame <- function(hyp, estimate) {
    out <- data.table::copy(hyp$template)
    out[["estimate"]] <- estimate
    out
}


# Push posterior draws (one row per pre-hypothesis estimate, one column per
# draw) through a compiled stage and attach them to the output frame.
hypothesis_frame_draws <- function(hyp, out, draws) {
    if (identical(hyp$source, "function")) {
        msg <- "The `hypothesis` argument does not support function for models with draws. You can use `get_draws()` to extract draws and manipulate them directly instead."
        stop_sprintf(msg)
    }
    draws <- as.matrix(hyp$apply(draws))
    if (hyp$source %in% c("matrix", "string")) {
        out[["estimate"]] <- posterior_draws_center(draws, stats::median)
    } else if ("hypothesis" %in% colnames(out) && nrow(draws) == nrow(out)) {
        row.names(draws) <- out[["hypothesis"]]
    }
    attr(out, "posterior_draws") <- draws
    out
}


# Numeric vector or matrix hypothesis: the contrast matrix is given.
hypothesis_compile_matrix <- function(hypothesis, cmp_skeleton) {
    if (isTRUE(checkmate::check_atomic_vector(hypothesis))) {
        hypothesis <- matrix(hypothesis, ncol = 1)
    }
    checkmate::assert_matrix(hypothesis, nrows = nrow(cmp_skeleton))
    if (is.null(colnames(hypothesis))) {
        colnames(hypothesis) <- rep("custom", ncol(hypothesis))
    }
    keep <- colnames(hypothesis) != "1 - 1"
    H <- hypothesis[, keep, drop = FALSE]
    apply_fn <- function(est) {
        if (is.matrix(est)) {
            return(crossprod(H, est))
        }
        as.vector(est %*% H)
    }
    list(
        kind = "matrix",
        source = "matrix",
        template = data.table::data.table(term = colnames(H), estimate = NA_real_),
        apply = apply_fn,
        H = H
    )
}


# Validate one string hypothesis against the estimates it reads and compile
# its expression. Positional (`b1`, `b2`) and term-name references are
# resolved here, once.
hypothesis_compile_string_one <- function(x, hypothesis, lab) {
    # row indices: `hypotheses` includes them, but `term` does not
    if (
        isTRUE(grepl("\\bb\\d+\\b", hypothesis)) &&
            !any(grepl("\\bb\\d+\\b", x[["term"]]))
    ) {
        msg <- "
It is essential to check the order of estimates when specifying hypothesis tests using positional indices like b1, b2, etc. The indices of estimates can change depending on the order of rows in the original dataset, user-supplied arguments, model-fitting package, and version of `marginaleffects`.

It is also good practice to use assertions that ensure the order of estimates is consistent across different runs of the same code. Example:

```r
mod <- lm(mpg ~ am * carb, data = mtcars)

# assertion for safety
p <- avg_predictions(mod, by = 'carb')
stopifnot(p$carb[1] == 1, p$carb[2] == 2)

# hypothesis test
avg_predictions(mod, by = 'carb', hypothesis = 'b1 - b2 = 0')
```

Disable this warning with: `options(marginaleffects_safe = FALSE)`
"
        if (isTRUE(getOption("marginaleffects_safe", default = TRUE))) {
            warn_once(msg, "hypothesis_positional_indices_are_dangerous")
        }
        bmax <- regmatches(lab, gregexpr("\\bb\\d+\\b", lab))[[1]]
        bmax <- tryCatch(
            max(as.numeric(gsub("b", "", bmax))),
            error = function(e) 0
        )
        if (bmax > nrow(x)) {
            msg <- "%s cannot be used in `hypothesis` because the call produced just %s estimate(s). Try executing the exact same command without the `hypothesis` argument to see which estimates are available for hypothesis testing."
            msg <- sprintf(msg, paste0("b", bmax), nrow(x))
            stop_sprintf(msg)
        }
        positional <- TRUE
        rowlabels <- NULL

        # term names
    } else {
        if (!"term" %in% colnames(x) || anyDuplicated(x$term) > 0) {
            msg <- c(
                'To use term names in a `hypothesis` string, the same function call without `hypothesis` argument must produce a `term` column with unique row identifiers. You can use `b1`, `b2`, etc. indices instead of term names in the `hypotheses` string Ex: "b1 + b2 = 0" Alternatively, you can use the `newdata`, `variables`, or `by` arguments:',
                "",
                "mod <- lm(mpg ~ am * vs + cyl, data = mtcars)",
                'comparisons(mod, newdata = "mean", hypothesis = "b1 = b2")',
                'comparisons(mod, newdata = "mean", hypothesis = "am = vs")',
                'comparisons(mod, variables = "am", by = "cyl", hypothesis = ~pairwise)'
            )
            stop_sprintf(msg)
        }
        positional <- FALSE
        rowlabels <- x$term
    }

    # Compile the expression, binding only the estimates it references.
    # Dynamic lookups (`get()`, `eval()`, ...) cannot be resolved
    # syntactically, so they bind every estimate.
    n_estimates <- nrow(x)
    if (isTRUE(positional)) {
        hypothesis <- gsub(
            "\\bb([0-9]+)\\b",
            "marginaleffects__\\1",
            hypothesis,
            perl = TRUE
        )
    }
    expr <- parse(text = hypothesis)
    dynamic_functions <- c(
        "assign", "do.call", "eval", "eval.parent", "eval_tidy", "evalq",
        "exists", "get", "get0", "local", "ls", "match.fun", "mget",
        "objects", "parse", "substitute", "with", "within"
    )
    dynamic <- any(all.names(expr, functions = TRUE) %in% dynamic_functions)
    if (isTRUE(positional)) {
        if (isTRUE(dynamic)) {
            idx <- seq_len(n_estimates)
            labels <- paste0("marginaleffects__", idx)
        } else {
            labels <- grep("^marginaleffects__[0-9]+$", all.vars(expr), value = TRUE)
            idx <- as.integer(sub("^marginaleffects__", "", labels))
            keep <- !is.na(idx) & idx >= 1L & idx <= n_estimates
            idx <- idx[keep]
            labels <- labels[keep]
        }
    } else if (isTRUE(dynamic)) {
        idx <- seq_along(rowlabels)
        labels <- rowlabels
    } else {
        labels <- intersect(all.vars(expr), rowlabels)
        idx <- match(labels, rowlabels)
    }
    list(expr = expr, idx = idx, labels = labels)
}


# Evaluate one compiled string expression at a vector of estimates.
hypothesis_compile_string_eval <- function(est, cc) {
    envir <- new.env(parent = .GlobalEnv)
    for (j in seq_along(cc$idx)) {
        assign(cc$labels[[j]], est[[cc$idx[[j]]]], envir = envir)
    }
    eval(cc$expr, envir = envir)
}


expand_wildcard <- function(hyp, bmax, lab) {
    # Find all occurrences of b*
    bstar_indices <- gregexpr("b\\*", hyp)[[1]]
    if (bstar_indices[1] == -1) {
        return(list(hyp, lab))
    }
    bstar_count <- length(bstar_indices)
    if (bstar_count > 1) {
        stop_sprintf("Error: More than one 'b*' substring found.")
    }

    # Replace b* with b1, b2, b3, ..., bmax
    labs <- character(bmax)
    result <- character(bmax)
    for (i in 1:bmax) {
        result[i] <- sub("b\\*", paste0("b", i), hyp)
        labs[i] <- sub("b\\*", paste0("b", i), lab)
    }

    return(list(result, labs))
}


hypothesis_compile_string <- function(hypothesis, cmp_skeleton) {
    lab <- attr(hypothesis, "label")
    if (is.null(lab)) {
        lab <- hypothesis
    }
    expanded <- expand_wildcard(hypothesis, nrow(cmp_skeleton), lab)
    hyps <- expanded[[1]]
    labs <- expanded[[2]]

    compiled <- vector("list", length(hyps))
    rowlab <- character(length(hyps))
    for (i in seq_along(hyps)) {
        compiled[[i]] <- hypothesis_compile_string_one(cmp_skeleton, hyps[i], labs[i])
        rowlab[i] <- if (!is.null(attr(labs[i], "names"))) {
            attr(labs[i], "names")
        } else {
            gsub("\\s+", "", labs[i])
        }
    }

    apply_vec <- function(est) {
        unlist(lapply(compiled, function(cc) {
            hypothesis_compile_string_eval(est, cc)
        }), use.names = FALSE)
    }
    apply_fn <- function(est) {
        if (is.matrix(est)) {
            out <- base::apply(est, MARGIN = 2, FUN = apply_vec)
            return(matrix(out, ncol = ncol(est)))
        }
        apply_vec(est)
    }

    # One evaluation at the compile point fixes the number of rows each
    # expression contributes, which the label template needs.
    est0 <- cmp_skeleton[["estimate"]]
    values <- lapply(compiled, function(cc) hypothesis_compile_string_eval(est0, cc))
    template <- data.table::data.table(
        hypothesis = rep(rowlab, lengths(values)),
        estimate = NA_real_
    )

    hyp <- list(
        kind = "string",
        source = "string",
        template = template,
        apply = apply_fn
    )

    # A syntactic proof of shape for every expression, never a numeric probe:
    # the walk which proves the map affine also reads off the contrast matrix
    # column by column, and the additive constant. Any expression outside the
    # affine fragment (a product of estimates, a function call) yields NULL
    # and leaves the hypothesis as a closure. Missing estimates keep the
    # closure too, so their standard errors stay missing.
    if (
        isTRUE(getOption("marginaleffects_hypothesis_promote", default = TRUE)) &&
            all(lengths(values) == 1L) &&
            !anyNA(unlist(values))
    ) {
        n_estimates <- nrow(cmp_skeleton)
        columns <- lapply(compiled, function(cc) {
            hypothesis_expression_coefficients(cc$expr, cc$labels, cc$idx, n_estimates)
        })
        if (length(columns) > 0L && !any(vapply(columns, is.null, logical(1)))) {
            hyp$kind <- "matrix"
            hyp$H <- do.call(cbind, lapply(columns, `[[`, "coef"))
            # The estimates are crossprod(H, x) + offset; the derivative is H
            # alone, never differentiated through the constant.
            hyp$offset <- vapply(columns, `[[`, numeric(1), "const")
        }
    }
    hyp
}


# Formula hypotheses: `~pairwise`, `ratio ~ reference | group`,
# `~ I(fun(x))`, etc. The estimates are split into groups (one for the
# whole vector without `| group`), and the shortcut is applied within each.
hypothesis_compile_formula <- function(hypothesis, cmp_skeleton, by = NULL, newdata = NULL, mfx = NULL) {
    form <- sanitize_hypothesis_formula(hypothesis)
    group <- form$group
    x <- data.table::as.data.table(cmp_skeleton)

    if (isTRUE(checkmate::check_character(by))) {
        bycols <- setdiff(by, group)
    } else {
        bycols <- by
    }
    if (!isTRUE(checkmate::check_character(bycols)) && !is.null(mfx) && length(mfx@variable_names_datagrid) > 0) {
        bycols <- intersect(mfx@variable_names_datagrid, colnames(x))
    }
    labels <- get_labels(x, by = bycols, hypothesis_by = group)
    if (is.null(labels)) {
        labels <- paste("Row", seq_len(nrow(x)))
    }

    if (isTRUE(form$lhs == "arbitrary_function")) {
        fun_comparison <- sprintf("function(x) %s", form$rhs)
        fun_label <- sprintf("function(x) suppressWarnings(names(%s))", form$rhs)
        fun_comparison <- eval(parse(text = fun_comparison), envir = environment(hypothesis))
        fun_label <- eval(parse(text = fun_label), envir = environment(hypothesis))
    } else {
        fun_label <- hypothesis_formula_list[[form$rhs]][[form$lhs]]$label
        fun_comparison <- hypothesis_formula_list[[form$rhs]][[form$lhs]]$comparison
    }

    # Groups, in sorted key order, and the key values of each group.
    groupval <- hypothesis_formula_groups(x, newdata, group)
    if (is.null(groupval)) {
        groups <- list(seq_len(nrow(x)))
        keys <- NULL
    } else {
        combined <- data.table::data.table(
            marginaleffects_formula_idx = seq_len(nrow(x))
        )
        combined <- cbind(combined, groupval)
        data.table::setDT(combined)
        keys <- combined[
            ,
            .(marginaleffects_formula_idx = list(marginaleffects_formula_idx)),
            keyby = groupval
        ]
        groups <- keys[["marginaleffects_formula_idx"]]
        keys[, marginaleffects_formula_idx := NULL]
    }
    expand_keys <- function(n) {
        if (is.null(keys)) {
            return(NULL)
        }
        keys[rep(seq_len(nrow(keys)), n)]
    }

    # One evaluation at the compile point fixes the number of rows each
    # group contributes, any names the shortcut attaches to its output, and
    # runs the shortcut's own guards (e.g. the many-pairs safety check).
    est0 <- x[["estimate"]]
    values <- lapply(groups, function(idx) fun_comparison(est0[idx]))
    if (!isTRUE(checkmate::check_numeric(unlist(values)))) {
        msg <- "The `hypothesis` argument must produce numeric estimates."
        stop(msg, call. = FALSE)
    }
    lens <- lengths(values)

    template <- data.table::data.table(estimate = rep(NA_real_, sum(lens)))
    gk <- expand_keys(lens)
    if (!is.null(gk)) {
        template <- cbind(gk, template)
    }
    names_out <- lapply(values, function(v) suppressWarnings(names(v)))
    n_names <- sum(lengths(names_out))
    if (n_names > 0) {
        # The shortcut names its own output (e.g. helmert, or a custom
        # function returning a named vector).
        if (n_names == nrow(template)) {
            template <- cbind(
                data.table::data.table(hypothesis = unlist(names_out, use.names = FALSE)),
                template
            )
        }
    } else {
        hyplab <- tryCatch(
            lapply(groups, function(idx) fun_label(labels[idx])),
            error = function(e) NULL
        )
        if (!is.null(hyplab) && sum(lengths(hyplab)) == nrow(template)) {
            lk <- expand_keys(lengths(hyplab))
            hl <- data.table::data.table(hypothesis = unlist(hyplab, use.names = FALSE))
            if (!is.null(lk)) {
                hl <- cbind(lk, hl)
            }
            template <- cbind(hl, template[, "estimate"])
        }
    }
    attr(template, "hypothesis_function_by") <- group

    apply_closure <- function(est) {
        if (is.matrix(est)) {
            out <- lapply(groups, function(idx) {
                r <- base::apply(est[idx, , drop = FALSE], MARGIN = 2, FUN = fun_comparison)
                matrix(r, ncol = ncol(est))
            })
            return(do.call(rbind, out))
        }
        unlist(
            lapply(groups, function(idx) fun_comparison(est[idx])),
            use.names = FALSE
        )
    }

    hyp <- list(
        kind = "formula",
        source = "formula",
        template = template,
        apply = apply_closure
    )

    promote <- isTRUE(getOption("marginaleffects_hypothesis_promote", default = TRUE))
    if (!promote) {
        return(hyp)
    }

    # Differences and dot products are linear: the shortcut block of each
    # group is assembled into one sparse contrast matrix, and the stage
    # applies it directly. Missing estimates keep the closure, so their
    # standard errors stay missing. Ratios and custom functions have no
    # proven linear structure and stay closures.
    H <- hypothesis_compile_formula_matrix(form, groups)
    if (!is.null(H) && ncol(H) == sum(lens) && !anyNA(unlist(values))) {
        hyp$kind <- "matrix"
        hyp$H <- H
        hyp$apply <- function(est) {
            out <- Matrix::crossprod(H, est)
            if (is.matrix(est)) as.matrix(out) else as.vector(out)
        }
    } else if (isTRUE(form$lhs %in% c("difference", "dotproduct"))) {
        # The centering shortcuts are linear but dense: meandev is
        # x - mean(x), with matrix I - 11'/n, and meanotherdev is
        # (n/(n-1)) x - sum(x)/(n-1). Materializing them costs O(n^2) memory
        # for a rank-one update, so they get a structured exact pullback
        # instead, applied group by group in O(np). Both operators are
        # symmetric, so the pullback applies H itself.
        shortcut <- form$rhs
        if (
            shortcut %in% c("meandev", "meanotherdev") &&
                !(identical(shortcut, "meanotherdev") && any(lengths(groups) < 2L))
        ) {
            hyp$pullback <- function(J) {
                J <- as.matrix(J)
                out <- vector("list", length(groups))
                for (k in seq_along(groups)) {
                    B <- J[groups[[k]], , drop = FALSE]
                    n <- nrow(B)
                    if (identical(shortcut, "meandev")) {
                        out[[k]] <- B - matrix(colMeans(B), n, ncol(B), byrow = TRUE)
                    } else {
                        out[[k]] <- (n / (n - 1)) * B -
                            matrix(colSums(B) / (n - 1), n, ncol(B), byrow = TRUE)
                    }
                }
                do.call(rbind, out)
            }
        }
    }
    hyp
}


# User-supplied function hypotheses are arbitrary code: they produce their
# own labels, so the stage re-runs the function on substituted estimates.
# Never promoted, because linearity of arbitrary code cannot be proven.
hypothesis_compile_function <- function(hypothesis, cmp_skeleton, by = NULL, newdata = NULL, draws = NULL) {
    skeleton <- data.table::copy(cmp_skeleton)
    if (!is.null(draws)) {
        attr(skeleton, "posterior_draws") <- draws
    }
    run <- function(est) {
        x <- data.table::copy(skeleton)
        x[["estimate"]] <- est
        hypothesis_function(x, newdata = newdata, hypothesis = hypothesis, by = by)
    }
    template <- run(skeleton[["estimate"]])
    if (!isTRUE(checkmate::check_numeric(template[["estimate"]]))) {
        msg <- "The `hypothesis` argument must produce numeric estimates."
        stop(msg, call. = FALSE)
    }
    list(
        kind = "function",
        source = "function",
        template = template,
        apply = function(est) run(est)[["estimate"]]
    )
}

# A compiled stage carries an exact derivative when it has a finite contrast
# matrix of the right width or a structured pullback. Callers which must decide
# whether to recover the pre-hypothesis estimates -- an expensive replay, and
# useless when nothing will probe them -- ask this before the Jacobian exists.
hypothesis_stage_exact <- function(hyp, n_post = NULL) {
    if (is.null(hyp)) {
        return(FALSE)
    }
    if (identical(hyp$kind, "matrix") && !is.null(hyp$H)) {
        H <- as.matrix(hyp$H)
        if (
            isTRUE(checkmate::check_matrix(H, mode = "numeric")) &&
                all(is.finite(H)) &&
                (is.null(n_post) || ncol(H) == n_post)
        ) {
            return(TRUE)
        }
    }
    is.function(hyp$pullback)
}


# Pull a Jacobian back through a compiled hypothesis stage. `J` has one row per
# pre-hypothesis estimate and the result has one row per post-hypothesis
# estimate. `at` is the pre-hypothesis estimate vector, consulted only when the
# stage has to be probed numerically. Returns NULL when the stage cannot be
# differentiated here: the contract is fail-closed, so callers fall back to
# differentiating the whole pipeline rather than compose something wrong.
hypothesis_stage_pullback <- function(hyp, J, at = NULL) {
    if (is.null(hyp)) {
        return(NULL)
    }

    # Affine hypotheses map estimates with crossprod(H, estimate) + offset, so H
    # maps every coefficient column of the Jacobian at once; the offset has zero
    # derivative and never enters. Probing it numerically instead cancels
    # catastrophically once the constant is large relative to the estimates.
    if (identical(hyp$kind, "matrix") && !is.null(hyp$H)) {
        H <- as.matrix(hyp$H)
        if (
            nrow(H) == nrow(J) &&
                isTRUE(checkmate::check_matrix(H, mode = "numeric")) &&
                all(is.finite(H))
        ) {
            out <- tryCatch(
                as.matrix(Matrix::crossprod(hyp$H, J)),
                error = function(e) NULL
            )
            if (!isTRUE(checkmate::check_matrix(out, mode = "numeric"))) {
                return(NULL)
            }
            return(list(jacobian = out, exact = TRUE))
        }
    }

    # Centering shortcuts carry a structured pullback: their operators are dense
    # as matrices, and the pullback applies t(H) in O(np) instead. It is exact,
    # so no numeric-stage provenance attaches.
    if (is.function(hyp$pullback)) {
        out <- tryCatch(as.matrix(hyp$pullback(J)), error = function(e) NULL)
        if (!isTRUE(checkmate::check_matrix(out, mode = "numeric"))) {
            return(NULL)
        }
        return(list(jacobian = out, exact = TRUE))
    }

    # A hypothesis which is not linear, or whose linearity could not be proved,
    # is still only a map from a handful of estimates to a handful of tested
    # quantities. Differentiating that map costs nothing next to a model
    # evaluation, so the exact Jacobian of everything upstream is composed with
    # a probe of the hypothesis rather than being discarded.
    if (!is.function(hyp$apply) || !is.numeric(at) || length(at) != nrow(J)) {
        return(NULL)
    }
    G <- stage_jacobian_dense(hyp$apply, at)
    if (is.null(G) || ncol(G) != nrow(J)) {
        return(NULL)
    }
    list(jacobian = as.matrix(G %*% J), exact = FALSE)
}


hypothesis_compile_formula_matrix <- function(form, groups) {
    if (!isTRUE(form$lhs %in% c("difference", "dotproduct"))) {
        return(NULL)
    }

    group_sizes <- lengths(groups)
    unique_sizes <- unique(group_sizes)
    block_cache <- tryCatch(
        lapply(unique_sizes, function(n) {
            hypothesis_compile_formula_matrix_block(form$rhs, n)
        }),
        error = function(e) NULL
    )
    if (is.null(block_cache) || any(vapply(block_cache, is.null, logical(1)))) {
        return(NULL)
    }

    blocks <- block_cache[match(group_sizes, unique_sizes)]
    H <- Matrix::bdiag(blocks)
    row_order <- unlist(groups, use.names = FALSE)
    H[match(seq_along(row_order), row_order), , drop = FALSE]
}

hypothesis_compile_formula_matrix_block <- function(shortcut, n) {
    if (shortcut %in% c("reference", "revreference")) {
        if (n < 2L) {
            return(NULL)
        }
        s <- if (shortcut == "reference") 1 else -1
        return(Matrix::sparseMatrix(
            i = c(seq.int(2L, n), rep.int(1L, n - 1L)),
            j = rep.int(seq_len(n - 1L), 2L),
            x = c(rep.int(s, n - 1L), rep.int(-s, n - 1L)),
            dims = c(n, n - 1L)
        ))
    }

    if (shortcut == "sequential") {
        if (n < 2L) {
            return(NULL)
        }
        return(Matrix::sparseMatrix(
            i = c(seq.int(2L, n), seq_len(n - 1L)),
            j = rep.int(seq_len(n - 1L), 2L),
            x = rep(c(1, -1), each = n - 1L),
            dims = c(n, n - 1L)
        ))
    }

    if (shortcut %in% c("pairwise", "revpairwise")) {
        if (shortcut == "pairwise") {
            pairs <- which(lower.tri(matrix(FALSE, n, n)), arr.ind = TRUE)
        } else {
            pairs <- which(upper.tri(matrix(FALSE, n, n)), arr.ind = TRUE)
        }
        npairs <- nrow(pairs)
        return(Matrix::sparseMatrix(
            i = c(pairs[, 1L], pairs[, 2L]),
            j = rep.int(seq_len(npairs), 2L),
            x = rep(c(1, -1), each = npairs),
            dims = c(n, npairs)
        ))
    }

    if (shortcut == "trt_vs_ctrl") {
        if (n < 2L) {
            return(NULL)
        }
        return(Matrix::sparseMatrix(
            i = seq_len(n),
            j = rep.int(1L, n),
            x = c(-1, rep.int(1 / (n - 1L), n - 1L)),
            dims = c(n, 1L)
        ))
    }

    # meandev and meanotherdev get a structured pullback in
    # hypothesis_compile_formula():
    # their operators are dense -- every entry of the n x n matrix is nonzero
    # -- so materializing them costs O(n^2) memory for what is a rank-one
    # update applied in O(np).

    if (shortcut == "poly") {
        H <- stats::contr.poly(n)
        H <- H[, seq_len(min(5L, ncol(H))), drop = FALSE]
        return(Matrix::Matrix(H, sparse = TRUE))
    }

    if (shortcut == "helmert") {
        return(Matrix::Matrix(stats::contr.helmert(n), sparse = TRUE))
    }

    NULL
}
