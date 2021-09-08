test_against_margins <- function(results, 
                                 margins_object, 
                                 se = TRUE, 
                                 tolerance = 1e-5,
                                 verbose = FALSE) {

    is_equal <- function(x, y) {
        all(abs((x - y) / x) < tolerance)
    }

    margins_object <- data.frame(margins_object)
    term_names <- unique(results$term)

    flag <- TRUE

    # dydx
    for (tn in term_names) {
        unknown <- results[results$term == tn, "dydx"]
        known <- as.numeric(margins_object[, paste0("dydx_", tn)])
        tmp <- is_equal(known, unknown)
        if (isFALSE(tmp)) {
            flag <- FALSE
            if (isTRUE(verbose)) print(sprintf("dydx: %s", tn))
        }

    }

    # std.error
    if ("std.error" %in% colnames(results)) {
        for (tn in term_names) {
            unknown <- results[results$term == tn, "std.error"]
            known <- as.numeric(margins_object[, paste0("SE_dydx_", tn)])
            tmp <- is_equal(known, unknown)
            if (isFALSE(tmp)) {
                flag <- FALSE
                if (isTRUE(verbose)) print(sprintf("se: %s", tn))
            }
        }
    }

    return(flag)
}
