get_hypotheses <- function(model_perturbed, hypothesis, hypothesis_is_formula, newparams = NULL, ...) {
    if (isTRUE(checkmate::check_numeric(model_perturbed))) {
        out <- data.frame(term = seq_along(model_perturbed), estimate = model_perturbed)
    } else if (inherits(model_perturbed, "data.frame")) {
        out <- model_perturbed
        if (!"estimate" %in% colnames(out)) {
            msg <- "`hypothesis` function must return a data.frame with a column named `estimate`."
            insight::format_error(msg)
        }
        if (!"term" %in% colnames(out)) {
            n <- tryCatch(names(stats::coef(model_perturbed)), error = function(e) NULL)
            if (is.null(n)) {
                n <- paste0("b", seq_len(nrow(out)))
            }
            out$term <- n
        }
        if (!all(c("term", "estimate") %in% colnames(out))) {
            msg <- "`hypothesis` function must return a data.frame with two columns named `term` and `estimate`."
            insight::format_error(msg)
        }

        # unknown model
    } else if (!is.function(hypothesis)) {
        out <- insight::get_parameters(model_perturbed, ...)
        if ("Component" %in% colnames(out) && !anyNA(out$Component)) {
            out$Parameter <- sprintf("%s_%s", out$Component, out$Parameter)
        } else if ("Response" %in% colnames(out) && !anyNA(out$Response)) {
            out$Parameter <- sprintf("%s_%s", out$Response, out$Parameter)
        }
        idx <- intersect(colnames(model_perturbed), c("term", "group", "estimate"))
        colnames(out)[1:2] <- c("term", "estimate")

        # glmmTMB
        if (!is.null(newparams)) {
            out$estimate <- newparams
        }
    } else if (hypothesis_is_formula) {
        beta <- get_coef(model_perturbed)
        out <- data.table::data.table(estimate = beta, term = names(beta))

        # unknown model but user-supplied hypothesis function
    } else {
        out <- model_perturbed
    }

    tmp <- get_hypothesis(out, hypothesis = hypothesis)
    out <- tmp$estimate
    hypothesis_function_by <- attr(tmp, "hypothesis_function_by")

    # labels
    lab <- c("hypothesis", "term", hypothesis_function_by)
    lab <- intersect(lab, colnames(tmp))
    if (length(lab) > 0) {
        lab <- tmp[, ..lab]
        attr(out, "label") <- lab
    }

    if ("group" %in% colnames(tmp)) {
        attr(out, "grouplab") <- tmp[["group"]]
    }

    attr(out, "hypothesis_function_by") <- hypothesis_function_by
    return(out)
}