library(marginaleffects)

requiet <- function(package) {
    void <- capture.output(
        pkg_available <- tryCatch(suppressPackageStartupMessages(suppressWarnings(suppressMessages(tryCatch(
            isTRUE(require(package, warn.conflicts = FALSE, character.only = TRUE)),
            error = function(e) FALSE
        )))))
    )
    return(invisible(pkg_available))
}

testing_path <- function(x) {
    file.path(testthat::test_path("../../inst/tinytest"), x)
}

# Environment variables for test control
EXPENSIVE <- TRUE
AUTODIFF <- FALSE
ON_WINDOWS <- Sys.info()[["sysname"]] == "Windows"

marginaleffects::autodiff(FALSE)
options(
    marginaleffects_autodiff_message = TRUE,
    marginaleffects_numDeriv = NULL,
    marginaleffects_safe = FALSE
)

# Custom testthat expectations
expect_slopes2 <- function(
    object,
    n_unique = NULL,
    pct_na = 5,
    se = TRUE,
    ...) {

    act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
    act$val <- marginaleffects:::hush(slopes(act$val, ...))

    errors <- character()

    # class
    if (!inherits(act$val, "slopes") || !inherits(act$val, "marginaleffects")) {
        errors <- c(errors, sprintf("Wrong class: `%s`.", class(act$val)[1]))
    }

    # na
    if (isTRUE(marginaleffects:::hush(mean(is.na(act$val$estimate)) * 100 > pct_na))) {
        errors <- c(errors, sprintf("More than %s%% missing values.", pct_na))
    }

    # unique
    if (!is.null(n_unique) && isTRUE(marginaleffects:::hush(length(unique(act$val$estimate)) < n_unique - 1))) {
        errors <- c(errors, sprintf("Fewer than %s unique values.", n_unique))
    }

    # std.error
    if (isTRUE(se) && !"std.error" %in% colnames(act$val)) {
        errors <- c(errors, "No standard error column.")
    }

    testthat::expect(
        length(errors) == 0,
        sprintf(
            "%s failed:\n%s",
            act$lab,
            paste(errors, collapse = "\n")
        )
    )

    invisible(act$val)
}

expect_predictions2 <- function(object, se = TRUE, n_row = NULL, n_col = NULL, ...) {
    act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
    act$val <- marginaleffects:::hush(predictions(act$val, ...))

    errors <- character()

    # class
    if (!isTRUE(checkmate::check_class(act$val, "predictions"))) {
        errors <- c(errors, sprintf("Wrong class: `%s`.", class(act$val)[1]))
    }

    # std.error
    if (isTRUE(se) && !"std.error" %in% colnames(act$val)) {
        errors <- c(errors, "No standard error column.")
    }

    # rows
    if (!is.null(n_row) && isTRUE(n_row > nrow(act$val))) {
        errors <- c(errors, sprintf("Expected at least %s rows but got %s.", n_row, nrow(act$val)))
    }

    # cols
    if (!is.null(n_col) && isTRUE(n_col > ncol(act$val))) {
        errors <- c(errors, sprintf("Expected at least %s columns but got %s.", n_col, ncol(act$val)))
    }

    testthat::expect(
        length(errors) == 0,
        sprintf(
            "%s failed:\n%s",
            act$lab,
            paste(errors, collapse = "\n")
        )
    )

    invisible(act$val)
}

expect_comparisons2 <- function(object, se = TRUE, n_row = NULL, n_col = NULL, ...) {
    act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
    act$val <- marginaleffects:::hush(comparisons(act$val, ...))

    errors <- character()

    # class
    if (!isTRUE(checkmate::check_class(act$val, "comparisons"))) {
        errors <- c(errors, sprintf("Wrong class: `%s`.", class(act$val)[1]))
    }

    # std.error
    if (isTRUE(se) && !"std.error" %in% colnames(act$val)) {
        errors <- c(errors, "No standard error column.")
    }

    # rows
    if (!is.null(n_row) && isTRUE(n_row > nrow(act$val))) {
        errors <- c(errors, sprintf("Expected at least %s rows but got %s.", n_row, nrow(act$val)))
    }

    # cols
    if (!is.null(n_col) && isTRUE(n_col > ncol(act$val))) {
        errors <- c(errors, sprintf("Expected at least %s columns but got %s.", n_col, ncol(act$val)))
    }

    testthat::expect(
        length(errors) == 0,
        sprintf(
            "%s failed:\n%s",
            act$lab,
            paste(errors, collapse = "\n")
        )
    )

    invisible(act$val)
}

expect_hypotheses2 <- function(object, se = TRUE, n_row = NULL, n_col = NULL, ...) {
    act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
    act$val <- marginaleffects:::hush(hypotheses(act$val, ...))

    errors <- character()

    # class
    if (!isTRUE(checkmate::check_class(act$val, "hypotheses"))) {
        errors <- c(errors, sprintf("Wrong class: `%s`.", class(act$val)[1]))
    }

    # std.error
    if (isTRUE(se) && !"std.error" %in% colnames(act$val)) {
        errors <- c(errors, "No standard error column.")
    }

    # rows
    if (!is.null(n_row) && isTRUE(n_row > nrow(act$val))) {
        errors <- c(errors, sprintf("Expected at least %s rows but got %s.", n_row, nrow(act$val)))
    }

    # cols
    if (!is.null(n_col) && isTRUE(n_col > ncol(act$val))) {
        errors <- c(errors, sprintf("Expected at least %s columns but got %s.", n_col, ncol(act$val)))
    }

    testthat::expect(
        length(errors) == 0,
        sprintf(
            "%s failed:\n%s",
            act$lab,
            paste(errors, collapse = "\n")
        )
    )

    invisible(act$val)
}

expect_margins2 <- function(
    results,
    margins_object,
    se = TRUE,
    tolerance = 1e-5,
    verbose = FALSE) {

    act <- testthat::quasi_label(rlang::enquo(results), arg = "results")

    is_equal <- function(x, y) {
        all(abs((x - y) / x) < tolerance)
    }

    act$val$type <- NULL

    margins_object <- data.frame(margins_object)
    term_names <- unique(act$val$term)

    errors <- character()

    # dydx
    for (tn in term_names) {
        unknown <- act$val[act$val$term == tn, "estimate"]
        lab <- paste0("dydx_", tn)
        if (lab %in% colnames(margins_object)) {
            known <- as.numeric(margins_object[, lab])
            if (!is_equal(known, unknown)) {
                errors <- c(errors, sprintf("dydx mismatch for: %s", tn))
                if (isTRUE(verbose)) print(sprintf("dydx: %s", tn))
            }
        }
    }

    # std.error
    if (isTRUE(se) && "std.error" %in% colnames(act$val)) {
        for (tn in term_names) {
            lab_se <- paste0("SE_dydx_", tn)
            lab_var <- paste0("Var_dydx_", tn)
            if (lab_se %in% colnames(margins_object)) {
                unknown <- act$val[act$val$term == tn, "std.error"]
                known <- as.numeric(margins_object[, lab_se])
                if (!is_equal(known, unknown)) {
                    errors <- c(errors, sprintf("std.error mismatch for: %s", tn))
                    if (isTRUE(verbose)) print(sprintf("se: %s", tn))
                }
            } else if (lab_var %in% colnames(margins_object)) {
                unknown <- act$val[act$val$term == tn, "std.error"]
                known <- sqrt(as.numeric(margins_object[, lab_var]))
                if (!is_equal(known, unknown)) {
                    errors <- c(errors, sprintf("Var mismatch for: %s", tn))
                    if (isTRUE(verbose)) print(sprintf("Var: %s", tn))
                }
            } else {
                errors <- c(errors, sprintf("Missing column: %s or %s", lab_se, lab_var))
                if (isTRUE(verbose)) print(sprintf("missing column: %s", lab_se))
            }
        }
    }

    testthat::expect(
        length(errors) == 0,
        sprintf(
            "%s failed:\n%s",
            act$lab,
            paste(errors, collapse = "\n")
        )
    )

    invisible(act$val)
}
