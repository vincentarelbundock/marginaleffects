backtransform <- function(x, transform) {

    # transform can be a function or a named list of length 1 with a function, but could be NULL
    if (!is.function(transform)) {
        if (is.null(transform[[1]])) {
            return(x)
        } else {
            transform <- transform[[1]]
        }
    }
   

    checkmate::assert_data_frame(x)
    checkmate::assert_function(transform)
    cols <- intersect(colnames(x), c("estimate", "conf.low", "conf.high"))
    draws <- attr(x, "posterior_draws")

    if (!is.null(draws)) {
        draws <- transform(draws)
    }

    for (col in cols) {
        x[[col]] <- transform(x[[col]])
    }
    for (col in c("std.error", "statistic")) {
        x[[col]] <- NULL
    }

    attr(x, "posterior_draws") <- draws

    return(x)
}
