sanitize_eps <- function(eps, model, variables) {
    checkmate::assert_number(eps, null.ok = TRUE)
    if (is.null(eps)) {
        out <- list(default_eps = 1e-4)
        # we don't use `newdata`, because that may already be averaged or subsetted
        dat <- insight::get_data(model)
        for (v in unlist(variables)) {
            if (isTRUE(is.numeric(dat[[v]]))) {
                out[[v]] <- 1e-4 * diff(range(dat[[v]], na.rm = TRUE))
            }
        }
    } else {
        out <- list(default_eps = eps)
    }
    return(out)
}
