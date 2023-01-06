recall <- function(x, hypothesis = NULL, by = NULL, vcov = NULL, conf_level = NULL) {

    # add `hypotheses` and `by` to the original call and re-evaluate
    if (!inherits(x, c("comparisons", "slopes", "predictions", "marginalmeans"))) {
        return(NULL)
    }

    if (is.null(hypothesis) && is.null(by) && is.null(vcov)) {
        return(NULL)
    }

    mc <- attr(x, "call")
    if (!is.call(mc)) {
        msg <- sprintf("Call could not be retrieved from object of class %s.", class(model)[1])
        insight::format_error(msg)
    }

    if (!is.null(by)) {
        if ("by" %in% names(mc)) {
            insight::format_error("The `by` argument cannot be applied twice.")
        } else {
            mc[["by"]] <- by
        }
    }

    if (!is.null(hypothesis)) {
        if ("hypothesis" %in% names(mc)) {
            insight::format_error("The `by` argument cannot be applied twice.")
        } else {
            mc[["hypothesis"]] <- hypothesis
        }
    }

    if (!is.null(vcov)) {
        if (!isTRUE(checkmate::check_flag(vcov, null.ok = TRUE))) {
            mc[["vcov"]] <- vcov
        }
    }

    if (!is.null(conf_level)) {
        if (!isTRUE(checkmate::check_flag(conf_level, null.ok = TRUE))) {
            mc[["conf_level"]] <- conf_level
        }
    }

    out <- eval(mc)

    return(out)
}
