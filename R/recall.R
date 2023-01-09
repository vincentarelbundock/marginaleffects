recall <- function(x, ...) {

    if (!inherits(x, c("comparisons", "slopes", "predictions", "marginalmeans", "hypotheses"))) {
        return(NULL)
    }

    mc <- attr(x, "call")
    if (!is.call(mc)) {
        msg <- sprintf("Call could not be retrieved from object of class %s.", class(x)[1])
        insight::format_error(msg)
    }

    dots <- list(...)

    # save newdata=datagrid() for use in recall()
    if (!is.null(attr(x, "newdata"))) {
        mc[["newdata"]] <- attr(x, "newdata")
    }

    mc[["model"]] <- attr(x, "model")

    # no need to compute again if nothing changed
    nochange <- TRUE

    # overwrite previous arguments
    for (n in names(dots)) {
        if (!identical(mc[[n]], dots[[n]])) {
            mc[[n]] <- dots[[n]]
            nochange <- FALSE
        }
    }

    if (isTRUE(nochange)) {
        out <- x
    } else {
        out <- eval(mc)
    }

    return(out)
}
