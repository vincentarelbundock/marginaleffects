backtransform <- function(x, transform) {
    # transform can be a function or a named list of length 1 with a function, but could be NULL
    if (!is.function(transform)) {
        if (is.null(transform[[1]])) {
            return(x)
        }

        transform <- transform[[1]]
    }

    checkmate::assert_data_frame(x)
    checkmate::assert_function(transform)
    cols <- intersect(colnames(x), c("estimate", "conf.low", "conf.high"))
    draws <- attr(x, "posterior_draws")

    if (!is.null(draws)) {
        dim_pre <- dim(draws)
        draws <- transform(draws)
        dim_post <- dim(draws)
        if (!identical(dim_pre, dim_post)) {
            insight::format_error(
                "The `transform` function must return an object of the same class as its input: a matrix input must return a matrix of the same size, and a vector input must return a vector of the same length."
            )
        }
    }

    for (col in cols) {
        x[[col]] <- transform(x[[col]])
    }

    # Issue #1204: Some inverse link functions swap the order of low and high
    if (all(c("conf.low", "conf.high") %in% colnames(x))) {
        if (all(x$conf.high < x$conf.low)) {
            lo <- x[["conf.low"]]
            hi <- x[["conf.high"]]
            x[["conf.low"]] <- hi
            x[["conf.high"]] <- lo
        }
    }

    for (col in c("std.error", "statistic")) {
        x[[col]] <- NULL
    }

    attr(x, "posterior_draws") <- draws

    return(x)
}
