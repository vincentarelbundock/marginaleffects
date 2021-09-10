#' Generate "typical" datasets for use in `marginaleffects`'s `newdata` argument
#' 
#' @param model Model object
#' @param data data.frame (one and only one of the `model` and `data` arguments
#' must be true).
#' @param at Named list of typical values to consider.
#' @return A `data.frame` with the values specified in the `at` list, and where each of the other variables is set at its median or mode.
#' @export
#' @examples
#' # Notice that the dataset now only has 2 rows, and that all variables except
#' # `hp` are at their median or mode.
#' td <- typical(data = mtcars, at = list("hp" = c(100, 110)))
#' td
#'
typical <- function(model = NULL, data = NULL, at = NULL) {
    checkmate::assert_list(at, 
                           null.ok = TRUE, 
                           min.len = 1,
                           names = "unique")

    if (!is.null(model) & !is.null(data)) {
        stop("One of the `model` or `data` arguments must be `NULL`.")
    }
    if (is.null(model) & is.null(data)) {
        stop("One of the `model` or `data` arguments must not be `NULL`.")
    }

    if (!is.null(model)) {
        dat <- insight::get_data(model)
        v_all <- insight::find_variables(model)$conditional
    } else {
        dat <- data
        v_all <- colnames(dat)
    }

    v_manual <- names(at)
    v_automatic <- setdiff(v_all, v_manual)

    # check `at` names
    v_bad <- setdiff(names(at), v_all)
    if (length(v_bad) > 0) {
        warning(sprintf("Variables in `at` are missing from the model data: %s", paste(v_bad, collapse = ", ")))
    }

    # check `at` elements and convert them to factor as needed
    for (n in names(at)) {
        if (is.factor(dat[[n]])) {
            at[[n]] <- as.character(at[[n]])
            if (!all(at[[n]] %in% levels(dat[[n]]))) {
                msg <- sprintf('The "%s" element of the `at` list corresponds to a factor variable. The values entered in the `at` list must be one of the factor levels: "%s".', n, paste(levels(dat[[n]]), collapse = '", "'))
                stop(msg)
            } else {
                at[[n]] <- factor(at[[n]], levels = levels(dat[[n]]))
            }
        }
    }

    if (length(v_automatic) > 0) {
        dat_automatic <- dat[, v_automatic, drop = FALSE]
        dat_automatic <- stats::na.omit(dat_automatic)
        out <- median_or_mode(dat_automatic)
    } else {
        out <- list()
    }

    if (!is.null(at)) {
        for (n in names(at)) {
            out[n] <- at[n]
        }
    }

    out <- expand.grid(out)
    return(out)
}
