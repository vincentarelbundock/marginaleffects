align_jacobian_vcov <- function(J, V, object, ...) {
    if (!isTRUE(ncol(J) == ncol(V))) {
        beta <- get_coef(object, ...)
        # Issue #718: ordinal::clm in test-pkg-ordinal.R
        if (
            anyNA(beta) &&
                anyDuplicated(names(beta)) &&
                ncol(J) > ncol(V) &&
                ncol(J) == length(beta) &&
                length(stats::na.omit(beta)) == ncol(V)
        ) {
            J <- J[, !is.na(beta), drop = FALSE]
        } else {
            cols <- intersect(colnames(J), colnames(V))
            if (length(cols) == 0) {
                insight::format_error(
                    "The jacobian does not match the variance-covariance matrix."
                )
            }
            V <- V[cols, cols, drop = FALSE]
            J <- J[, cols, drop = FALSE]
        }
    }
    return(list(J = J, V = V))
}


get_se_delta_contrasts <- function(
    mfx,
    model_perturbed,
    variables,
    newdata,
    type,
    hypothesis,
    lo,
    hi,
    original,
    cross,
    comparison,
    by,
    byfun,
    ...) {
    get_comparisons(
        model_perturbed = model_perturbed,
        mfx = mfx,
        variables = variables,
        type = type,
        hypothesis = hypothesis,
        lo = lo,
        hi = hi,
        original = original,
        cross = cross,
        verbose = FALSE,
        deltamethod = TRUE,
        by = by,
        byfun = byfun,
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
get_se_delta <- function(
    model_perturbed,
    vcov,
    FUN,
    mfx = NULL,
    type = NULL,
    newdata = NULL,
    index = NULL,
    eps = NULL,
    J = NULL,
    hypothesis = NULL,
    numderiv = NULL,
    calling_function = NULL,
    comparison = NULL,
    by = NULL,
    byfun = NULL,
    hi = NULL,
    lo = NULL,
    original = NULL,
    ...) {
    # delta method does not work for these models
    bad <- c("brmsfit", "stanreg", "bart")
    if (any(inherits(model_perturbed, bad))) {
        return(NULL)
    }

    coefs <- get_coef(model_perturbed, ...)

    # TODO: this is a terrible sanity check
    # some vcov methods return an unnamed matrix, some have duplicate names
    flag <- anyDuplicated(colnames(vcov)) == 0 ||
        anyDuplicated(names(coefs)) == 0
    if (
        flag &&
            !is.null(dimnames(vcov)) &&
            all(names(coefs) %in% colnames(vcov))
    ) {
        bnames <- intersect(names(coefs), colnames(vcov))
        vcov <- vcov[bnames, bnames, drop = FALSE]
        colnames(vcov) <- row.names(vcov) <- names(coefs)
        coefs <- coefs[bnames]
    }

    # user-supplied jacobian machine (e.g. JAX)
    if (is.null(J)) {
        fun <- getOption("marginaleffects_jacobian_function", default = function(...) NULL)
        if (!isTRUE(checkmate::check_function(fun))) {
            msg <- "The `marginaleffects_jacobian_function` option must be a function."
            stop_sprintf(msg)
        }
        if (!"..." %in% names(formals(fun))) {
            msg <- "The `marginaleffects_jacobian_function` option must accept the ... argument."
            stop_sprintf(msg)
        }
        J <- fun(
            coefs = coefs,
            mfx = mfx,
            newdata = newdata,
            model_perturbed = model_perturbed,
            hypothesis = hypothesis,
            type = type,
            by = by,
            byfun = byfun,
            hi = hi,
            lo = lo,
            original = original,
            comparison = comparison,
            calling_function = calling_function
        )
        checkmate::assert_matrix(J, mode = "numeric", ncols = length(coefs), null.ok = TRUE)
    }

    # input: named vector of coefficients
    # output: gradient
    inner <- function(x) {
        names(x) <- names(coefs)
        model_tmp <- set_coef(model_perturbed, x, ...)
        # do not pass NULL arguments. Important for `deltam` to allow users to supply FUN without ...
        args <- list(
            mfx = mfx,
            model_perturbed = model_tmp,
            hypothesis = hypothesis,
            type = type,
            hi = hi,
            lo = lo,
            original = original,
            by = by,
            byfun = byfun
        )
        args <- c(args, list(...))
        if (inherits(model_perturbed, "gamlss")) {
            args[["safe"]] <- FALSE
        }
        if (!is.null(eps)) {
            args[["eps"]] <- eps
        }
        if (!is.null(type)) {
            args[["type"]] <- type
        }
        if (!is.null(newdata)) {
            args[["newdata"]] <- newdata
        }
        if (!is.null(J)) {
            args[["J"]] <- J
        }
        if (!is.null(eps)) {
            args[["eps"]] <- eps
        }

        if (inherits(model_perturbed, "glmmTMB")) {
            args$newparams <- x
        }

        g <- do.call("FUN", args)

        return(g)
    }

    if (is.null(J) || !is.null(hypothesis)) {
        args <- list(
            func = inner,
            x = coefs,
            numderiv = numderiv
        )
        J <- do.call("get_jacobian", args)
        colnames(J) <- names(get_coef(model_perturbed, ...))
    }

    # align J and V: This might be a problematic hack, but I have not found examples yet.
    V <- vcov
    if (!isTRUE(ncol(J) == ncol(V))) {
        aligned <- align_jacobian_vcov(J, V, model_perturbed, ...)
        J <- aligned$J
        V <- aligned$V
    }

    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # algebra trick: https://stackoverflow.com/a/42569902/342331
    # keep old code for transparency and reference
    # se <- sqrt(rowSums(tcrossprod(J, V) * J))
    se <- eigen_J_V_SE(J, V)
    se[se == 0] <- NA_real_
    attr(se, "jacobian") <- J

    return(se)
}
