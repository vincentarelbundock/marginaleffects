backtransform <- function(x, transformation) {
    checkmate::assert_data_frame(x)
    checkmate::assert_function(transformation)
    for (col in c("estimate", "conf.low", "conf.high")) {
        x[[col]] <- transformation(x[[col]])
    }
    for (col in c("std.error", "statistic")) {
        x[[col]] <- NULL
    }
    return(x)
}
