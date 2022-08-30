backtransform <- function(x, transform_post) {
    checkmate::assert_data_frame(x)
    checkmate::assert_function(transform_post)
    cols <- intersect(colnames(x), c("comparison", "marginalmean", "predicted", "estimate", "conf.low", "conf.high"))
    draws <- attr(x, "posterior_draws")

    if (!is.null(draws)) {
        draws <- transform_post(draws)
    }

    for (col in cols) {
        x[[col]] <- transform_post(x[[col]])
    }
    for (col in c("std.error", "statistic")) {
        x[[col]] <- NULL
    }

    attr(x, "posterior_draws") <- draws

    return(x)
}
