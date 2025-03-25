# fancy way to catch the call so that get_averages(slopes()) does not evaluate twice
# and is fast
recall <- function(x, ...) {
    funs <- c("comparisons", "slopes", "predictions", "hypotheses", "avg_predictions", "avg_comparisons", "avg_slopes")
    funs <- c(funs, paste0("marginaleffects::", funs))

    # 2-step estimation with already evaluated & assigned call
    if (!is.call(x)) {
        # unsupported evaluated object: return `NULL`
        if (!inherits(x, funs)) {
            return(NULL)
        }

        # retrieve call
        mc <- attr(x, "call")
        if (!is.call(mc)) {
            msg <- sprintf("Call could not be retrieved from object of class %s.", class(x)[1])
            insight::format_error(msg)
        }

    # unsupported call: return `NULL`
    } else {
        if (!as.character(x[1]) %in% funs) {
            return(NULL)
        }
        mc <- x
    }

    dots <- list(...)

    # don't overwrite certain arguments
    if ("hypothesis" %in% names(mc) && "hypothesis" %in% names(dots)) {
        if (is.null(dots[["hypothesis"]])) {
            dots[["hypothesis"]] <- NULL
        }
    }

    # safe to work with original objects when available
    objs <- c("newdata", "model")
    for (obj in objs) {
        if (!is.null(attr(x, "call")[[obj]])) {
            dots[[obj]] <- attr(x, "call")[[obj]]
        }
    }

    # overwrite previous arguments
    for (n in names(dots)) {
        # named NULL should not remove the corresponding argument from the call
        if (is.null(dots[[n]])) {
            mc[n] <- list(NULL)
        } else {
            mc[[n]] <- dots[[n]]
        }
    }

    out <- eval(mc)

    return(out)
}
