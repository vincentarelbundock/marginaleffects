hypothesis_string_compile_expression <- function(
    hypothesis,
    rowlabels = NULL,
    n_estimates = NULL,
    positional = FALSE,
    eval_parent = .GlobalEnv) {
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
            labels <- grep(
                "^marginaleffects__[0-9]+$",
                all.vars(expr),
                value = TRUE
            )
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

    list(
        expr = expr,
        idx = idx,
        labels = labels,
        eval_parent = eval_parent
    )
}


hypothesis_string_eval_compiled <- function(vec, compiled) {
    envir <- new.env(parent = compiled$eval_parent)
    for (j in seq_along(compiled$idx)) {
        assign(
            compiled$labels[[j]],
            vec[[compiled$idx[[j]]]],
            envir = envir
        )
    }
    eval(compiled$expr, envir = envir)
}


hypothesis_string <- function(x, hypothesis) {
    out_list <- draws_list <- list()
    lab <- attr(hypothesis, "label")
    tmp <- expand_wildcard(hypothesis, nrow(x), lab)
    hypothesis <- tmp[[1]]
    labs <- tmp[[2]]
    for (i in seq_along(hypothesis)) {
        out_list[[i]] <- eval_string_hypothesis(x, hypothesis[i], labs[i])
        draws_list[[i]] <- attr(out_list[[i]], "posterior_draws")
    }
    out <- do.call(rbind, out_list)
    attr(out, "posterior_draws") <- do.call(rbind, draws_list)
    attr(out, "label") <- if (!is.null(attr(labs, "names"))) {
        attr(labs, "names")
    } else {
        labs
    }
    return(out)
}


eval_string_hypothesis <- function(x, hypothesis, lab) {
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

    compiled <- hypothesis_string_compile_expression(
        hypothesis,
        rowlabels = rowlabels,
        n_estimates = nrow(x),
        positional = positional,
        eval_parent = environment()
    )

    if (!is.null(attr(lab, "names"))) {
        lab <- attr(lab, "names")
    } else {
        lab <- gsub("\\s+", "", lab)
    }

    draws <- attr(x, "posterior_draws")
    if (!is.null(draws)) {
        insight::check_if_installed("collapse", minimum_version = "1.9.0")
        tmp <- apply(
            draws,
            MARGIN = 2,
            FUN = hypothesis_string_eval_compiled,
            compiled = compiled
        )
        draws <- matrix(tmp, ncol = ncol(draws))
        out <- data.table(
            hypothesis = lab,
            tmp = collapse::dapply(draws, MARGIN = 1, FUN = collapse::fmedian)
        )
    } else {
        out <- hypothesis_string_eval_compiled(
            x[["estimate"]],
            compiled = compiled
        )
        out <- data.table(
            hypothesis = lab,
            tmp = out
        )
    }

    setnames(out, old = "tmp", new = "estimate")
    attr(out, "posterior_draws") <- draws
    return(out)
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
