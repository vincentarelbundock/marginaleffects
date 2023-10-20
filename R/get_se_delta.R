get_se_delta_marginalmeans <- function(model,
                                       variables,
                                       newdata,
                                       type,
                                       cross = FALSE,
                                       ...) {
    get_marginalmeans(
        model = model,
        variables = variables,
        newdata = newdata,
        type = type,
        cross = cross,
        ...
    )$estimate
}


get_se_delta_contrasts <- function(model,
                                   variables,
                                   newdata,
                                   type,
                                   hypothesis,
                                   lo,
                                   hi,
                                   original,
                                   cross,
                                   ...) {
    get_contrasts(model,
        newdata = newdata,
        variables = variables,
        type = type,
        hypothesis = hypothesis,
        lo = lo,
        hi = hi,
        original = original,
        cross = cross,
        verbose = FALSE,
        deltamethod = TRUE,
        ...
    )$estimate
}



#' Compute standard errors using the delta method
#'
#' @inheritParams slopes
#' @param FUN a function which accepts a `model` and other inputs and returns a
#'   vector of estimates (marginal effects, marginal means, etc.)
#' @param index data.frame over which we aggregate J_mean (matches tidy() output)
#' @return vector of standard errors
#' @noRd
get_se_delta <- function(model,
                         vcov,
                         FUN,
                         type = NULL,
                         newdata = NULL,
                         index = NULL,
                         eps = NULL,
                         J = NULL,
                         hypothesis = NULL,
                         numderiv = NULL,
                         ...) {

    # delta method does not work for these models
    bad <- c("brmsfit", "stanreg", "bart")
    if (any(bad %in% class(model))) {
        return(NULL)
    }

    coefs <- get_coef(model, ...)

    # TODO: this is a terrible sanity check
    # some vcov methods return an unnamed matrix, some have duplicate names
    flag <- anyDuplicated(colnames(vcov)) == 0 || anyDuplicated(names(coefs))  == 0
    if (flag && !is.null(dimnames(vcov)) && all(names(coefs) %in% colnames(vcov))) {
        bnames <- intersect(names(coefs), colnames(vcov))
        vcov <- vcov[bnames, bnames, drop = FALSE]
        colnames(vcov) <- row.names(vcov) <- names(coefs)
        coefs <- coefs[bnames]
    }
    

    # input: named vector of coefficients
    # output: gradient
    inner <- function(x) {
        model_tmp <- set_coef(model, stats::setNames(x, names(coefs)) ,...)
        # do not pass NULL arguments. Important for `deltam` to allow users to supply FUN without ...
        args <- c(list(model = model_tmp, hypothesis = hypothesis), list(...))
        if (inherits(model, "gamlss")) args[["safe"]] <- FALSE
        if (!is.null(eps)) args[["eps"]] <- eps
        if (!is.null(type)) args[["type"]] <- type
        if (!is.null(newdata)) args[["newdata"]] <- newdata
        if (!is.null(J)) args[["J"]] <- J
        if (!is.null(eps)) args[["eps"]] <- eps
        g <- do.call("FUN", args)
        return(g)
    }

    if (is.null(J) || !is.null(hypothesis)) {
        args <- list(
            func = inner,
            x = coefs,
            numderiv = numderiv)
        J <- do.call("get_jacobian", args)
        colnames(J) <- names(get_coef(model, ...))
    }

    # align J and V: This might be a problematic hack, but I have not found examples yet.
    V <- vcov
    if (!isTRUE(ncol(J) == ncol(V))) {
        beta <- get_coef(model, ...)
        # Issue #718: ordinal::clm in test-pkg-ordinal.R
        if (anyNA(beta) && anyDuplicated(names(beta)) && ncol(J) > ncol(V) && ncol(J) == length(beta) && length(stats::na.omit(beta)) == ncol(V)) {
            J <- J[, !is.na(beta), drop = FALSE]
        } else {
            cols <- intersect(colnames(J), colnames(V))
            if (length(cols) == 0) {
                insight::format_error("The jacobian does not match the variance-covariance matrix.")
            }
            V <- V[cols, cols, drop = FALSE]
            J <- J[, cols, drop = FALSE]
        }
    }

    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    se <- sqrt(rowSums(tcrossprod(J, V) * J))
    se[se == 0] <- NA_real_
    attr(se, "jacobian") <- J

    return(se)
}
