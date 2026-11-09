# The estimates on which the hypothesis is evaluated, before the hypothesis is
# applied. Callers which need to differentiate the hypothesis exactly must
# compile it against these estimates, so the extraction is factored out here
# rather than duplicated.
get_hypotheses_skeleton <- function(model_perturbed, hypothesis, hypothesis_is_formula, newparams = NULL, ...) {
    if (isTRUE(checkmate::check_numeric(model_perturbed))) {
        out <- data.frame(term = seq_along(model_perturbed), estimate = model_perturbed)
    } else if (inherits(model_perturbed, "data.frame")) {
        out <- model_perturbed
        if (!"estimate" %in% colnames(out)) {
            msg <- "`hypothesis` function must return a data.frame with a column named `estimate`."
            stop_sprintf(msg)
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
            stop_sprintf(msg)
        }

        # unknown model
    } else if (!is.function(hypothesis)) {
        out <- insight::get_parameters(model_perturbed, ...)
        if ("Component" %in% colnames(out) && !anyNA(out$Component)) {
            out$Parameter <- sprintf("%s_%s", out$Component, out$Parameter)
        } else if ("Response" %in% colnames(out) && !anyNA(out$Response)) {
            out$Parameter <- sprintf("%s_%s", out$Response, out$Parameter)
        }
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

    return(out)
}


# Compile the hypothesis once against the estimates of the unperturbed model,
# so that every perturbation in the delta method only re-extracts the
# estimates and applies the compiled stage. User-supplied functions receive
# the model itself on some inputs, so they are not compiled and are re-run on
# each perturbation instead.
get_hypotheses_compile <- function(model, hypothesis, hypothesis_is_formula, ...) {
    if (is.null(hypothesis) || is.function(hypothesis)) {
        return(NULL)
    }
    skeleton <- get_hypotheses_skeleton(
        model_perturbed = model,
        hypothesis = hypothesis,
        hypothesis_is_formula = hypothesis_is_formula,
        ...
    )
    out <- hypothesis_compile(hypothesis, skeleton)
    out$estimate_pre <- skeleton[["estimate"]]
    out
}


# `compiled`: a stage from `get_hypotheses_compile()`. When supplied, only the
# estimates are returned, without labels: this is the function the delta
# method differentiates.
get_hypotheses <- function(model_perturbed, hypothesis, hypothesis_is_formula, newparams = NULL, compiled = NULL, ...) {
    out <- get_hypotheses_skeleton(
        model_perturbed = model_perturbed,
        hypothesis = hypothesis,
        hypothesis_is_formula = hypothesis_is_formula,
        newparams = newparams,
        ...
    )

    if (!is.null(compiled)) {
        return(compiled$apply(out[["estimate"]]))
    }

    if (isTRUE(checkmate::check_function(hypothesis))) {
        tmp <- hypothesis_function(
            data.table::copy(out),
            newdata = NULL,
            hypothesis = hypothesis,
            by = NULL
        )
    } else {
        tmp <- hypothesis_compile(hypothesis, out)$cmp
    }

    get_hypotheses_label(tmp)
}


# Labelled estimate vector from a hypothesis output table.
get_hypotheses_label <- function(tmp) {
    hypothesis_function_by <- attr(tmp, "hypothesis_function_by")

    # must be a data.table
    tmp <- data.table::as.data.table(tmp)

    out <- tmp$estimate

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
