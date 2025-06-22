sanitize_estimator <- function(x, estimator, method) {
    if (is.null(estimator)) {
        cl <- c("data.frame", "data.table", "tbl_df", "tbl")
        if (any(cl %in% class(x)[1])) {
            msg <- "`x` can only be a data.frame when supplying a function to the `estimator` argument."
            stop_sprintf(msg)
        }
    } else {
        checkmate::assert_function(estimator)
        checkmate::assert_data_frame(x)
        cl <- c("data.frame", "data.table", "tbl_df", "tbl")
        if (!any(cl %in% class(x)[1])) {
            msg <- "The `x` argument must be a raw data frame when using the `estimator` argument."
            stop_sprintf(msg)
        }
        if (!isTRUE(checkmate::check_choice(method, c("rsample", "boot")))) {
            stop_sprintf("The `estimator` argument is only supported when `method` is \"rsample\" or \"boot\".")
        }
        x <- estimator(x)
        cl <- c("predictions", "comparisons", "slopes", "hypotheses")
        if (!any(cl %in% class(x))) {
            msg <- sprintf("The `estimator` function must return a `marginaleffects` object.")
            stop_springf(msg)
        }
    }
    return(x)
}