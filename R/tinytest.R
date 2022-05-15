testing_path <- function(x) {
    insight::check_if_installed("here")
    here::here("inst/tinytest/", x)
}

requiet <- function(package) {
  # skip_if_not_installed(package)
  suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )
}

download_model <- function(name) {
    tmp <- tempfile()
    url <- paste0("https://raw.github.com/vincentarelbundock/modelarchive/main/data/", name, ".rds")
    try(download.file(url, tmp, quiet = TRUE), silent = TRUE)
    out <- try(readRDS(tmp), silent = TRUE)
    return(out)
}

expect_predictions <- function(object,
                               se = TRUE,
                               n_col = NULL,
                               n_row = NULL) {
    expect_inherits(object, "predictions")
    expect_true("type" %in% colnames(object))
    expect_true("predicted" %in% colnames(object))
    if (isTRUE(se)) expect_true("std.error" %in% colnames(object))
    if (!is.null(n_col)) expect_true(ncol(object) >= n_col)
    if (!is.null(n_row)) expect_true(nrow(object) >= n_row)
}


expect_marginaleffects <- function(
    object,
    type = "response",
    n_unique = 10,
    pct_na = 5,
    se = TRUE) {


    # Compute
    mfx <- marginaleffects(object, type = type)
    tid <- tidy(mfx)

    # Check
    mfx_class <- class(mfx)[1]
    tid_class <- class(tid)[1]
    mfx_nrow <- nrow(mfx)
    tid_nrow <- nrow(tid)
    dydx_unique <- length(unique(round(mfx$dydx, 4))) /
                   length(unique(mfx$term))
    dydx_na <- sum(is.na(mfx$dydx)) / nrow(mfx) * 100
    if (isTRUE(se)) {
        std.error_unique <- length(unique(round(mfx$std.error, 4))) /
                            length(unique(mfx$term))
        std.error_na <- sum(is.na(mfx$std.error_na)) / nrow(mfx) * 100
    } else {
        std.error_unique <- NULL
        std.error_na <- NULL
    }

    expect_inherits(mfx, "marginaleffects")
    expect_inherits(tid, "data.frame")
    expect_true(nrow(mfx) > 0)
    expect_true(nrow(tid) > 0)
    expect_true(dydx_unique >= n_unique)
    expect_true(dydx_na <= pct_na)
    if (!is.null(std.error_na)) {
        expect_true(std.error_unique >= n_unique)
        expect_true(std.error_na <= pct_na)
    }
}


expect_marginalmeans <- function(object,
                                 se = TRUE,
                                 n_col = NULL,
                                 n_row = NULL) {
    expect_inherits(object, "marginalmeans")
    expect_true(nrow(object) >= n_row)
    expect_true(ncol(object) >= n_col)
    if (isTRUE(se)) expect_true("std.error" %in% colnames(object))
}


expect_margins <- function(results,
                           margins_object,
                           se = TRUE,
                           tolerance = 1e-5,
                           verbose = FALSE) {

    is_equal <- function(x, y) {
        all(abs((x - y) / x) < tolerance)
    }

    results$type <- NULL

    margins_object <- data.frame(margins_object)
    term_names <- unique(results$term)

    flag <- TRUE

    # dydx
    for (tn in term_names) {
        unknown <- results[results$term == tn, "dydx"]
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
                if (isTRUE(verbose)) print(sprintf("missing column: %s", lab))
            }
        }
    }

    expect_true(flag)
}

