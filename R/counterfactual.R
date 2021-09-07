counterfactual <- function(model, at = NULL) {
    checkmate::assert_list(at, 
                           null.ok = TRUE, 
                           min.len = 1,
                           names = "unique")

    v_all <- insight::find_variables(model)$conditional
    v_manual <- names(at)
    v_automatic <- setdiff(v_all, v_manual)

    if (length(v_automatic) > 0) {
        dat <- insight::get_data(model)
        dat_automatic <- dat[, v_automatic]
        dat_automatic <- na.omit(dat_automatic)

        # check `at` elements and convert them to factor as needed
        for (n in names(at)) {
            if (is.factor(dat[[n]])) {
                if (!all(at[[n]] %in% levels(dat[[n]]))) {
                    msg <- sprintf('The "%s" element of the `at` list corresponds to a factor variable. The values entered in the `at` list must be one of the factor levels: "%s".', n, paste(levels(dat[[n]]), collapse = '", "'))
                    stop(msg)
                } else {
                    at[[n]] <- factor(at[[n]], levels = levels(dat[[n]]))
                }
            }
        }

        out <- median_or_mode(dat_automatic)

    } else {
        out <- list()
    }


    if (!is.null(at)) {
        for (n in names(at)) {
            out[[n]] <- at[[n]]
        }
    }
    out <- expand.grid(out)
    return(out)
}


####################################################################
#  The functions below were copied from the `prediction` package. ##
#  Copyright: Thomas J. Leeper 2016-2018                          ##
#  MIT License                                                    ##
####################################################################

#' find mode, preserve type, and pick an arbitrary value when multi-modal
#' https://stackoverflow.com/a/8189441/342331
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

median_or_mode <- function(x) {
    UseMethod("median_or_mode")
}

median_or_mode.default <- function(x) {
    median(x)
}

median_or_mode.factor <- function(x) {
    Mode(x)
}

median_or_mode.logical <- function(x) {
    Mode(x)
}

median_or_mode.data.frame <- function(x) {
    if (anyNA(x)) {
        stop("Please remove missing values before calling `median_or_mode`.")
    }
    setNames(lapply(x, median_or_mode), names(x))
}
