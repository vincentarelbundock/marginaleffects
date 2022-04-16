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


    if (isFALSE(vcov)) {
        return(NULL)
    }

    # user-level default is TRUE, but this generates a warning in
    # `insight::get_varcov` for some models
    if (isTRUE(vcov)) {
        vcov <- NULL
    }

    out <- vcov

    # ignore `vcov` completely 
    bad <- c("brmsfit", "stanreg") 
    bad <- any(sapply(bad, function(x) inherits(model, x)))
    if (isTRUE(bad)) {
        return(NULL)
    }

    # classes for which `insight::get_varcov` produces unexpected results
    bad <- c("betareg", "plm", "geeglm", "glmmTMB", "ivreg", "lmerMod", "glmerMod", "scam")
    bad <- any(sapply(bad, function(x) inherits(model, x)))
    if (!isTRUE(checkmate::check_matrix(out)) && isTRUE(bad)) {
        if (!is.null(out) && !is.logical(out)) {
            stop("The `vcov` argument is not supported for objects of this class.",
                 call. = FALSE)
        }
        out <- try(stats::vcov(model), silent = TRUE)
    }

    if (!isTRUE(checkmate::check_matrix(out))) {
        out <- try(insight::get_varcov(model, vcov = vcov, component = "all"), silent = TRUE)
    }

    if (!isTRUE(checkmate::check_matrix(out))) {
        # suppress: "Re-fitting to get Hessian"
        out <- try(suppressMessages(stats::vcov(model)), silent = TRUE)
    }

    # lme4 produces a distinct matrix type
    if (inherits(out, "dpoMatrix")) {
        out <- as.matrix(out)
    }

    # give up: brms, loess, and other models without vcov
    if (!isTRUE(checkmate::check_matrix(out))) {
        msg <- sprintf("Unable to extract a variance-covariance matrix for model of class `%s`.", class(model)[1])
        warning(msg, .call = FALSE)
        return(NULL)
    }

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
