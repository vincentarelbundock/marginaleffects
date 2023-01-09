recall <- function(x, ...) {

    # fancy way to catch the call so that averages(slopes()) does not evaluate twice and is fast
    funs <- c("comparisons", "slopes", "predictions", "marginalmeans", "hypotheses")
    if (is.call(x)) {
        fun <- as.character(x)[1]
        if (!fun %in% funs) {
            return(NULL)
        }
        mc <- x
    } else {
        if (!inherits(x, funs)) {
            return(NULL)
        }
        mc <- attr(x, "call")
        if (!is.call(mc)) {
            msg <- sprintf("Call could not be retrieved from object of class %s.", class(x)[1])
            insight::format_error(msg)
        }
    }

    dots <- list(...)

    # save newdata=datagrid() for use in recall()
    if (!is.null(attr(x, "newdata"))) {
        dots[["newdata"]] <- attr(x, "newdata")
    }

    if (!is.null(attr(x, "model"))) {
        dots[["model"]] <- attr(x, "model")
    }

    # overwrite previous arguments
    # for (n in names(dots)) {
    #     mc[[n]] <- dots[[n]]
    # }
    FUN <- rlang::call_modify
    args <- c(list(".call" = quote(mc)), dots)
    mc <- do.call("FUN", args)

    out <- eval(mc)

    return(out)
}
