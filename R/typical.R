#' @title Generate "typical" datasets based on a model
#' @export
typical <- function(model = NULL, data = NULL, at = NULL) {
    checkmate::assert_list(at, 
                           null.ok = TRUE, 
                           min.len = 1,
                           names = "unique")

    v_all <- insight::find_variables(model)$conditional
    v_manual <- names(at)
    v_automatic <- setdiff(v_all, v_manual)

    if (!is.null(model) && is.null(data)) {
        dat <- insight::get_data(model)
    } else if (is.null(model) && !is.null(data)) {
        dat <- data
    } else {
        stop("One (and only one) of the `model` and `data` arguments can be `NULL`.")
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
