backtransform <- function(x, transform_post) {
    checkmate::assert_data_frame(x)
    checkmate::assert_function(transform_post)
    cols <- intersect(colnames(x), c("comparison", "marginalmean", "estimate", "conf.low", "conf.high"))
    for (col in cols) {
        x[[col]] <- transform_post(x[[col]])
    }
    for (col in c("std.error", "statistic")) {
        x[[col]] <- NULL
    }
    return(x)
}
