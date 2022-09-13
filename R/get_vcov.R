#' Get a named variance-covariance matrix from a model object (internal function)
#'
#' @inheritParams marginaleffects
#' @return A named square matrix of variance and covariances. The names must match the coefficient names.
#' @rdname get_vcov
#' @keywords internal
#' @export
get_vcov <- function(model, ...) {
    UseMethod("get_vcov", model)
}


#' @rdname get_vcov
#' @export
get_vcov.default <- function(model,
                             vcov = NULL,
                             ...) {

    # user-level default can be boolean.
    if (isFALSE(vcov)) {
        return(FALSE)
    }

    # TRUE generates a warning in `insight::get_varcov` for some models
    if (isTRUE(vcov)) {
        vcov <- NULL
    }

    bad <- c("brmsfit", "stanreg")
    if (any(inherits(model, bad, which = TRUE) == 1)) {
        return(NULL)
    }

    # strings are only available with insight 0.17.1
    # strings should be case-insensitive
    vcov_strings <- c("stata", "robust", "HC", "HC0", "HC1", "HC2", "HC3",
                      "HC4", "HC4m", "HC5", "HAC", "NeweyWest", "kernHAC", "OPG",
                      "satterthwaite", "kenward-roger")
    if (isTRUE(checkmate::check_choice(tolower(vcov), choices = tolower(vcov_strings)))) {
        insight::check_if_installed("insight", minimum_version = "0.17.1")
        idx <- match(tolower(vcov), tolower(vcov_strings))
        vcov <- vcov_strings[idx]
    }

    # formulas are only available with insight 0.17.1
    if (isTRUE(checkmate::check_formula(vcov))) {
        insight::check_if_installed("insight", minimum_version = "0.17.1")
    }

    checkmate::assert(
        checkmate::check_null(vcov),
        checkmate::check_function(vcov),
        checkmate::check_matrix(vcov),
        checkmate::check_formula(vcov),
        checkmate::check_choice(vcov, choices = vcov_strings))

    out <- vcov

    if (isTRUE(checkmate::check_matrix(out))) {
        if (ncol(out) != nrow(out)) stop("The `vcov` matrix must be square.", call. = FALSE)
        return(out)
    }

    if (isTRUE(checkmate::check_function(out))) {
        return(out(model))
    }

    # {insight}
    args <- get_varcov_args(model, vcov)
    args[["x"]] <- model
    args[["component"]] <- "all"

    # 1st try: with arguments
    fun <- get("get_varcov", asNamespace("insight"))
    out <- myTryCatch(do.call("fun", args))

    # 2nd try: without arguments
    if (!isTRUE(checkmate::check_matrix(out$value, min.rows = 1))) {
        out <- myTryCatch(insight::get_varcov(model))
        if (isTRUE(checkmate::check_matrix(out$value, min.rows = 1))) {
            msg <- format_msg(
            "Unable to extract a variance-covariance matrix using this `vcov`
            argument. Standard errors are computed using the default variance
            instead. Perhaps the model or argument is not supported by the
            `sandwich` or `clubSandwich` packages. If you believe that the model
            is supported by one of these two packages, you can open a feature
            request on Github.")
            warning(msg, call. = FALSE)
        }
    }

    if (!isTRUE(checkmate::check_matrix(out$value, min.rows = 1))) {
        msg <- "Unable to extract a variance-covariance matrix from this model."
        warning(msg, call. = FALSE)
        return(NULL)

    # valid matrix with warning
    } else if (!is.null(out$warning)) {
        warning(out$warning$message, call. = FALSE)
    }

    out <- out[["value"]]

    # problem: no row.names
    if (is.null(row.names(out))) {
        coefs <- get_coef(model)
        if (ncol(out) == length(coefs)) {
            termnames <- names(stats::coef(model))
            if (length(termnames) == ncol(out)) {
                colnames(out) <- termnames
                row.names(out) <- termnames
            }
        } else {
            return(NULL)
        }
    }

    # problem: duplicate colnames
    if (anyDuplicated(names(out)) == 0) {
        coefs <- get_coef(model)
        # 1) Check above is needed for `AER::tobit` and others where `out`
        # includes Log(scale) but `coef` does not Dangerous for `oridinal::clm`
        # and others where there are important duplicate column names in
        # `out`, and selecting with [,] repeats the first instance.

        # 2) Sometimes out has more columns than coefs (e.g., betareg)
        if (all(names(coefs) %in% colnames(out))) {
            out <- out[names(coefs), names(coefs), drop = FALSE]
        }
    }

    return(out)

    # NOTES:
    # survival::coxph with 1 regressor produces a vector
}



#' Take a `modelsummary()` style `vcov` argument and convert it to
#' `insight::get_varcov()`
#'
#' @keywords internal
get_varcov_args <- function(model, vcov) {
    if (is.null(vcov)) {
        out <- list()
        return(out)
    }

    if (isTRUE(checkmate::check_formula(vcov))) {
        out <- list("vcov" = "vcovCL", "vcov_args" = list("cluster" = vcov))
        return(out)
    }

    if (isTRUE(vcov == "satterthwaite") || isTRUE(vcov == "kenward-roger")) {
        if (!isTRUE(inherits(model, "lmerMod")) && !isTRUE(inherits(model, "lmerModTest"))) {
            msg <- 'Satterthwaite and Kenward-Roger corrections are only available for linear mixed effects models.'
            stop(msg, call. = FALSE)
        }
        if (isTRUE(vcov == "satterthwaite")) {
            return(list())
        } else {
            return(list(vcov = "kenward-roger"))
        }
    }

    out <- switch(vcov,
        "stata" = list(vcov = "HC2"),
        "robust" = list(vcov = "HC3"),
        "bootstrap" = list(vcov = "BS"),
        "outer-product" = list(vcov = "OPG"),
        list(vcov = vcov))
    return(out)
}



get_vcov_label <- function(vcov) {
    if (is.null(vcov)) vcov <- ""
    if (!is.character(vcov)) return(NULL)

    out <- switch(vcov,
        "stata" = "Stata",
        "robust" = "Robust",
        "kenward-roger" = "Kenward-Roger",
        "satterthwaite" = "Satterthwaite",
        "HC" = ,
        "HC0" = ,
        "HC1" = ,
        "HC2" = ,
        "HC3" = ,
        "HC4" = ,
        "HC4m" = ,
        "HC5" = ,
        "HAC" = ,
        "OPG" = vcov,
        "NeweyWest" = "Newey-West",
        "kernHAC" = "Kernel HAC",
        vcov
    )
    return(out)
}


