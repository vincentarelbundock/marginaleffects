backtransform <- function(x, transformation) {
    checkmate::assert_data_frame(x)
    checkmate::assert_function(transformation)
    cols <- intersect(colnames(x), c("comparison", "estimate", "conf.low", "conf.high"))
    for (col in cols) {
        x[[col]] <- transformation(x[[col]])
    }
    for (col in c("std.error", "statistic")) {
        x[[col]] <- NULL
    }
    return(x)
}
