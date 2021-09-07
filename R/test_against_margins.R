test_against_margins <- function(results, 
                               margins_object, 
                               se = TRUE, 
                               tolerance = 0.0001,
                               verbose = FALSE) {

    margins_object <- data.frame(margins_object)
    term_names <- unique(results$term)

    # dydx
    for (tn in term_names) {
        if (verbose) print(sprintf("dydx: %s", tn))
        unknown <- results[results$term == tn, "dydx"]
        known <- as.numeric(margins_object[, paste0("dydx_", tn)])
        testthat::expect_equal(known, unknown, tolerance = tolerance)
    }

    # std.error
    if ("std.error" %in% colnames(results)) {
        for (tn in term_names) {
            if (verbose) print(sprintf("std.error: %s", tn))
            unknown <- results[results$term == tn, "std.error"]
            known <- as.numeric(margins_object[, paste0("SE_dydx_", tn)])
            testthat::expect_equal(known, unknown, tolerance = tolerance)
        }
    }
}
