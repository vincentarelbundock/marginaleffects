#' @title Generate "counterfactual" datasets based on a model
#' @export
counterfactual <- function(model = NULL, data = NULL, at = NULL) {
    checkmate::assert_list(at, 
                           null.ok = TRUE, 
                           min.len = 1,
                           names = "unique")

    if (!is.null(model) && is.null(data)) {
        dat <- insight::get_data(model)
        v_all <- insight::find_variables(model)$conditional
    } else if (is.null(model) && !is.null(data)) {
        dat <- data
        v_all <- colnames(dat)
    } else {
        stop("One (and only one) of the `model` and `data` arguments can be `NULL`.")
    }

    v_manual <- names(at)
    v_automatic <- setdiff(v_all, v_manual)

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

    # `at` -> `data.frame`
    at <- expand.grid(at)

    if (length(v_automatic) > 0) {
        dat_automatic <- dat[, v_automatic, drop = FALSE]
        dat_automatic <- cbind(data.frame(rowid = 1:nrow(dat_automatic)), dat_automatic)
        out <- merge(dat_automatic, at, all = TRUE)
    }  else {
        out <- at
    }

    return(out)
}
