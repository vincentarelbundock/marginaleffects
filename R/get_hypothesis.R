get_hypothesis <- function(
    x,
    hypothesis,
    by = NULL,
    newdata = NULL,
    draws = NULL) {

    deprecated <- c("pairwise", "revpairwise", "sequential", "revsequential", "reference", "meandev", "meanotherdev", "revreference")
    if (isTRUE(checkmate::check_choice(hypothesis, deprecated))) {
        msg <- "This string is deprecated for use in the `hypothesis` argument. Use the formula interface instead. Ex: `hypothesis=~reference`"
        stop(msg, call. = FALSE)
    }

    if (is.null(hypothesis)) return(x)

    labels <- get_hypothesis_row_labels(x, by = by)

    if (isTRUE(checkmate::check_formula(hypothesis))) {
        tmp <- sanitize_hypothesis_formula(hypothesis)
        list2env(tmp, environment())
    } else {
        comparison <- "difference"
        hypothesis_by <- NULL
    }

    valid <- c("reference", "sequential", "pairwise", "meandev", "meanotherdev", "poly", "trt_vs_ctrl", "arbitrary_function")
    if (isTRUE(checkmate::check_choice(hypothesis, choices = valid))) {
        if (hypothesis == "arbitrary_function") {
            fun_cmp <- sprintf("function(x) %s", comparison)
            fun_lab <- sprintf("function(x) suppressWarnings(names(%s))", comparison)
            out <- hypothesis_apply(x,
                labels = labels,
                hypothesis_by = hypothesis_by,
                fun_comparison = eval(parse(text = fun_cmp)),
                fun_label = eval(parse(text = fun_lab)),
                newdata = newdata,
                arbitrary = TRUE)
        } else {
            tmp <- hypothesis_functions[[hypothesis]][[comparison]]
            out <- hypothesis_apply(x,
                labels = labels,
                hypothesis_by = hypothesis_by,
                fun_comparison = tmp$comparison,
                fun_label = tmp$label,
                newdata = newdata)
        }
        return(out)
    }

    if (is.function(hypothesis)) {
        if (!is.null(draws)) {
            msg <- "The `hypothesis` argument does not support function for models with draws. You can use `get_draws()` to extract draws and manipulate them directly instead."
            insight::format_error(msg)
        }
        if ("rowid" %in% colnames(x) && "rowid" %in% colnames(newdata)) {
            x <- merge(x, newdata, all.x = TRUE, by = intersect(colnames(x), colnames(newdata)))
        } else if (isTRUE(nrow(x) == nrow(newdata))) {
            x <- cbind(x, newdata)
        }

        attr(x, "variables_datagrid") <- attr(newdata, "variables_datagrid")
        attr(x, "by") <- if (is.character(by)) by else names(by)

        argnames <- names(formals(hypothesis))
        if (!"x" %in% argnames) insight::format_error("The `hypothesis` function must accept an `x` argument.")
        if (!all(argnames %in% c("x", "draws"))) {
            msg <- "The allowable arguments for the `hypothesis` function are: `x` and `draws`"
            insight::format_error(msg)
        }
        args <- list(x = x, newdata = newdata, by = by, draws = draws)
        args <- args[names(args) %in% argnames]
        out <- do.call(hypothesis, args)
        at <- attr(out, "hypothesis_function_by")

        # sanity
        msg <- "The `hypothesis` argument function must return a data frame with `term` (or `hypothesis`) and `estimate` columns."
        if (inherits(out, "data.frame")) {
            if (!all(c("term", "estimate") %in% colnames(out)) && !all(c("hypothesis", "estimate") %in% colnames(out))) {
                insight::format_error(msg)
            }
        } else if (isTRUE(checkmate::check_numeric(out))) {
            if (isTRUE(checkmate::check_data_frame(x, nrows = length(out))) && "term" %in% colnames(out)) {
                out <- data.frame(term = out$term, estimate = out)
            } else {
                out <- data.frame(term = seq_along(out), estimate = out)
            }
        } else {
            insight::format_error(msg)
        }

        attr(out, "hypothesis_function_by") <- at
        return(out)
    }

    lincom <- NULL

    # lincom: numeric vector or matrix
    if (isTRUE(checkmate::check_numeric(hypothesis))) {
     if (isTRUE(checkmate::check_atomic_vector(hypothesis))) {
        checkmate::assert_numeric(hypothesis, len = nrow(x))
        lincom <- as.matrix(hypothesis)
     } else if (isTRUE(checkmate::check_matrix(hypothesis))) {
        lincom <- hypothesis
     }
    }

    lincom <- sanitize_lincom(lincom, x)

    # matrix hypothesis
    if (isTRUE(checkmate::check_matrix(lincom))) {
        out <- lincom_multiply(x, lincom)
        return(out)

    # string hypothesis
    } else if (is.character(hypothesis)) {
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

    insight::format_error("`hypotheses` is broken. Please report this bug: https://github.com/vincentarelbundock/marginaleffects/issues.")
}


get_hypothesis_row_labels <- function(x, by = NULL) {
    lab <- grep("^term$|^by$|^group$|^value$|^contrast$|^contrast_", colnames(x), value = TRUE)
    lab <- Filter(function(z) length(unique(x[[z]])) > 1, lab)
    if (isTRUE(checkmate::check_character(by))) {
        lab <- unique(c(lab, by))
    }
    if (length(lab) == 0) {
        lab <- NULL
    } else {
        lab_df <- data.frame(x)[, lab, drop = FALSE]
        idx <- vapply(lab_df, FUN = function(x) length(unique(x)) > 1, FUN.VALUE = logical(1))
        if (sum(idx) > 0) {
            lab <- apply(lab_df[, idx, drop = FALSE], 1, paste, collapse = ", ")
        } else {
            lab <- apply(lab_df, 1, paste, collapse = ", ")
        }
    }

    # wrap in parentheses to avoid a-b-c-d != (a-b)-(c-d)
    if (any(grepl("-", lab))) {
        lab <- sprintf("(%s)", lab)
    }

    return(lab)
}


sanitize_lincom <- function(lincom, x) {
    if (isTRUE(checkmate::check_matrix(lincom))) {
        checkmate::assert_matrix(lincom, nrows = nrow(x))
        if (is.null(colnames(lincom))) {
            colnames(lincom) <- rep("custom", ncol(lincom))
        }
    }
    return(lincom)
}



lincom_multiply <- function(x, lincom) {
    # bayesian
    draws <- attr(x, "posterior_draws")
    if (!is.null(draws)) {
        draws <- t(as.matrix(lincom)) %*% draws
        out <- data.table(
            term = colnames(lincom),
            tmp = apply(draws, 1, stats::median))
        setnames(out, old = "tmp", new = "estimate")
        attr(out, "posterior_draws") <- draws

        # frequentist
    } else {
        out <- data.table(
            term = colnames(lincom),
            tmp = as.vector(x[["estimate"]] %*% lincom))
        setnames(out, old = "tmp", new = "estimate")
    }

    out <- out[out$term != "1 - 1", , drop = FALSE]
    return(out)
}


eval_string_hypothesis <- function(x, hypothesis, lab) {
    # row indices: `hypotheses` includes them, but `term` does not
    if (isTRUE(grepl("\\bb\\d+\\b", hypothesis)) && !any(grepl("\\bb\\d+\\b", x[["term"]]))) {

        msg <- "
It is essential to check the order of estimates when specifying hypothesis tests using positional indices like b1, b2, etc. The indices of estimates can change depending on the order of rows in the original dataset, user-supplied arguments, model-fitting package, and version of `marginaleffects`. 

It is also good practice to use assertions that ensure the order of estimates is consistent across different runs of the same code. Example:

```r
mod <- lm(mpg ~ am * carb, data = mtcars)

# assertion for safety
p <- avg_predictions(mod, by = 'carb')
stopifnot(p$carb[1] != 1 || p$carb[2] != 2)

# hypothesis test
avg_predictions(mod, by = 'carb', hypothesis = 'b1 - b2 = 0')
```

Disable this warning with: `options(marginaleffects_safe = FALSE)`
"
        if (isTRUE(getOption("marginaleffects_safe", default = TRUE))) {
            warn_once(msg, "hypothesis_positional_indices_are_dangerous")
        }


        bmax <- regmatches(lab, gregexpr("\\bb\\d+\\b", lab))[[1]]
        bmax <- tryCatch(max(as.numeric(gsub("b", "", bmax))), error = function(e) 0)
        if (bmax > nrow(x)) {
            msg <- "%s cannot be used in `hypothesis` because the call produced just %s estimate(s). Try executing the exact same command without the `hypothesis` argument to see which estimates are available for hypothesis testing."
            msg <- sprintf(msg, paste0("b", bmax), nrow(x))
            insight::format_error(msg)
        }
        for (i in seq_len(nrow(x))) {
            tmp <- paste0("marginaleffects__", i)
            hypothesis <- gsub(paste0("b", i), tmp, hypothesis)
        }
        rowlabels <- paste0("marginaleffects__", seq_len(nrow(x)))

    # term names
    } else {
        if (!"term" %in% colnames(x) || anyDuplicated(x$term) > 0) {
            msg <- c(
                'To use term names in a `hypothesis` string, the same function call without `hypothesis` argument must produce a `term` column with unique row identifiers. You can use `b1`, `b2`, etc. indices instead of term names in the `hypotheses` string Ex: "b1 + b2 = 0" Alternatively, you can use the `newdata`, `variables`, or `by` arguments:',
                "",
                "mod <- lm(mpg ~ am * vs + cyl, data = mtcars)",
                'comparisons(mod, newdata = "mean", hypothesis = "b1 = b2")',
                'comparisons(mod, newdata = "mean", hypothesis = "am = vs")',
                'comparisons(mod, variables = "am", by = "cyl", hypothesis = ~pairwise)')
            insight::format_error(msg)
        }
        rowlabels <- x$term
    }

    eval_string_function <- function(vec, hypothesis, rowlabels) {
        envir <- parent.frame()
        void <- sapply(
            seq_along(vec), function(i) {
                assign(rowlabels[i], vec[i], envir = envir)
            })
        out <- eval(parse(text = hypothesis), envir = envir)
        return(out)
    }

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
            FUN = eval_string_function,
            hypothesis = hypothesis,
            rowlabels = rowlabels)
        draws <- matrix(tmp, ncol = ncol(draws))
        out <- data.table(
            term = lab,
            tmp = collapse::dapply(draws, MARGIN = 1, FUN = collapse::fmedian))
    } else {
        out <- eval_string_function(
            x[["estimate"]],
            hypothesis = hypothesis,
            rowlabels = rowlabels)
        out <- data.table(
            term = lab,
            tmp = out)
    }

    setnames(out, old = "tmp", new = "estimate")
    attr(out, "posterior_draws") <- draws
    return(out)
}


expand_wildcard <- function(hyp, bmax, lab) {
  # Find all occurrences of b*
  bstar_indices <- gregexpr("b\\*", hyp)[[1]]
  if (bstar_indices[1] == -1) return(list(hyp, lab))
  bstar_count <- length(bstar_indices)
  if (bstar_count > 1) {
    insight::format_error("Error: More than one 'b*' substring found.")
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
