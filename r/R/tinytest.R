#' `tinytest` helper
#'
#' @export
#' @keywords internal
expect_slopes <- function(
    object,
    n_unique = NULL,
    pct_na = 5,
    se = TRUE,
    ...) {
    insight::check_if_installed("tinytest")

    object <- hush(slopes(object, ...))

    diff <- ""

    # class
    fail_class <- !inherits(object, "slopes") || !inherits(object, "marginaleffects")
    if (fail_class) {
        msg <- sprintf("Wrong class: `%s`.", class(object)[1])
        diff <- c(diff, msg)
    }

    # na
    fail_na <- isTRUE(hush(mean(is.na(object$estimate)) * 100 > pct_na))
    if (fail_na) {
        msg <- sprintf("More than %s of missing values.", pct_na)
        diff <- c(diff, msg)
    }

    # unique
    fail_unique <- isTRUE(hush(length(unique(object$estimate)) < n_unique - 1))
    if (fail_unique) {
        msg <- sprintf("Fewer than %s unique values.", n_unique)
        diff <- c(diff, msg)
    }

    # unique
    if (isTRUE(se) && !"std.error" %in% colnames(object)) {
        msg <- "No standard error."
        diff <- c(diff, msg)
        fail_se <- TRUE
    } else {
        fail_se <- FALSE
    }

    # diff message
    diff <- paste(diff, collapse = "\n")

    # pass/fail
    fail <- fail_class || fail_na || fail_unique || fail_se

    # tinytest object
    out <- tinytest::tinytest(
        result = !fail,
        call = sys.call(sys.parent(1)),
        diff = diff
    )

    return(out)
}


#' `tinytest` helper
#'
#' @export
#' @keywords internal
expect_predictions <- function(object, se = TRUE, n_row = NULL, n_col = NULL, ...) {
    insight::check_if_installed("tinytest")
    call <- sys.call(sys.parent(1))
    object <- hush(predictions(object, ...))
    expect_marginaleffects_object(
        object,
        class = "predictions",
        se = se,
        n_row = n_row,
        n_col = n_col,
        call = call
    )
}


#' `tinytest` helper
#'
#' @export
#' @keywords internal
expect_margins <- function(
    results,
    margins_object,
    se = TRUE,
    tolerance = 1e-5,
    verbose = FALSE) {
    insight::check_if_installed("tinytest")

    is_equal <- function(x, y) {
        # `isTRUE()`: an `NA` in either vector must count as a mismatch, and
        # unequal lengths must not recycle silently.
        length(x) == length(y) && isTRUE(all(abs((x - y) / x) < tolerance))
    }

    results$type <- NULL

    margins_object <- data.frame(margins_object)
    term_names <- unique(results$term)

    flag <- TRUE
    diff <- ""

    # dydx
    for (tn in term_names) {
        unknown <- results[results$term == tn, "estimate"]
        lab <- paste0("dydx_", tn)
        if (lab %in% colnames(margins_object)) {
            known <- as.numeric(margins_object[, lab])
            tmp <- is_equal(known, unknown)
            if (isFALSE(tmp)) {
                flag <- FALSE
                if (isTRUE(verbose)) print(sprintf("dydx: %s", tn))
            }
        }
    }

    # std.error
    if (isTRUE(se) && "std.error" %in% colnames(results)) {
        for (tn in term_names) {
            lab_se <- paste0("SE_dydx_", tn)
            lab_var <- paste0("Var_dydx_", tn)
            if (lab_se %in% colnames(margins_object)) {
                unknown <- results[results$term == tn, "std.error"]
                known <- as.numeric(margins_object[, lab_se])
                tmp <- is_equal(known, unknown)
                if (isFALSE(tmp)) {
                    flag <- FALSE
                    if (isTRUE(verbose)) print(sprintf("se: %s", tn))
                }
            } else if (lab_var %in% colnames(margins_object)) {
                unknown <- results[results$term == tn, "std.error"]
                known <- sqrt(as.numeric(margins_object[, lab_var]))
                tmp <- is_equal(known, unknown)
                if (isFALSE(tmp)) {
                    flag <- FALSE
                    if (isTRUE(verbose)) print(sprintf("Var: %s", tn))
                }
            } else {
                flag <- FALSE
                msg <- sprintf("missing column: %s or %s", lab_se, lab_var)
                diff <- c(diff, msg)
                if (isTRUE(verbose)) print(msg)
            }
        }
    }

    diff <- paste(diff, collapse = "\n")

    # tinytest object
    out <- tinytest::tinytest(
        result = flag,
        call = sys.call(sys.parent(1)),
        diff = diff
    )

    return(out)
}


#' `tinytest` helper
#'
#' @export
#' @keywords internal
expect_hypotheses <- function(object, se = TRUE, n_row = NULL, n_col = NULL, ...) {
    insight::check_if_installed("tinytest")
    call <- sys.call(sys.parent(1))
    object <- hush(hypotheses(object, ...))
    expect_marginaleffects_object(
        object,
        class = "hypotheses",
        se = se,
        n_row = n_row,
        n_col = n_col,
        call = call
    )
}


#' `tinytest` helper
#'
#' @export
#' @keywords internal
expect_comparisons <- function(object, se = TRUE, n_row = NULL, n_col = NULL, ...) {
    insight::check_if_installed("tinytest")
    call <- sys.call(sys.parent(1))
    object <- hush(comparisons(object, ...))
    expect_marginaleffects_object(
        object,
        class = "comparisons",
        se = se,
        n_row = n_row,
        n_col = n_col,
        call = call
    )
}


# Shared checks for `expect_predictions()`, `expect_comparisons()`, and
# `expect_hypotheses()`. `call` is captured by the caller so the tinytest
# object reports the user's call.
expect_marginaleffects_object <- function(object, class, se, n_row, n_col, call) {
    diff <- ""

    # class
    fail_class <- !isTRUE(checkmate::check_class(object, class))
    if (fail_class) {
        diff <- c(diff, sprintf("Wrong class: `%s`.", class(object)[1]))
    }

    # std.error
    fail_se <- isTRUE(se) && !"std.error" %in% colnames(object)
    if (fail_se) {
        diff <- c(diff, "No standard error.")
    }

    # rows and cols
    fail_row <- isTRUE(n_row > nrow(object))
    if (fail_row) {
        diff <- c(diff, sprintf("Number of rows: %s", nrow(object)))
    }
    fail_col <- isTRUE(n_col > ncol(object))
    if (fail_col) {
        diff <- c(diff, sprintf("Number of columns: %s", ncol(object)))
    }

    fail <- fail_class || fail_se || fail_row || fail_col
    tinytest::tinytest(
        result = !fail,
        call = call,
        diff = paste(diff, collapse = "\n")
    )
}
