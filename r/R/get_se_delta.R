align_jacobian_vcov <- function(J, V, object, ...) {
    if (!isTRUE(ncol(J) == ncol(V))) {
        beta <- get_coef(object, ...)
        # Issue #718: ordinal::clm in test-pkg-ordinal.R
        if (
            anyNA(beta) &&
                ncol(J) > ncol(V) &&
                ncol(J) == length(beta) &&
                length(stats::na.omit(beta)) == ncol(V)
        ) {
            J <- J[, !is.na(beta), drop = FALSE]
        } else {
            cols <- intersect(colnames(J), colnames(V))
            if (length(cols) == 0) {
                stop_sprintf(
                    "The jacobian does not match the variance-covariance matrix."
                )
            }
            V <- V[cols, cols, drop = FALSE]
            J <- J[, cols, drop = FALSE]
        }
    }
    return(list(J = J, V = V))
}


std_error_from_jacobian <- function(J, V, object, ...) {
    # Covariance propagation is shared algebra. Eligibility for analytic and
    # autodiff Jacobians remains independent of this helper.
    jnames <- colnames(J)
    vnames <- colnames(V)
    if (
        !is.null(jnames) && !is.null(vnames) &&
            anyDuplicated(jnames) == 0L && anyDuplicated(vnames) == 0L &&
            setequal(jnames, vnames)
    ) {
        V <- V[jnames, jnames, drop = FALSE]
    }
    if (!isTRUE(ncol(J) == ncol(V))) {
        aligned <- align_jacobian_vcov(J, V, object, ...)
        J <- aligned$J
        V <- aligned$V
    }

    # Avoid constructing the full J V J' matrix when only its diagonal is used.
    se <- sqrt(rowSums(tcrossprod(J, V) * J))
    se[se == 0] <- NA_real_
    list(std.error = se, jacobian = J)
}


#' Compute standard errors using the delta method
#'
#' @inheritParams slopes
#' @param FUN a function which accepts a `model` and other inputs and returns a
#'   vector of estimates (marginal effects, marginal means, etc.)
#' @return vector of standard errors
#' @noRd
get_se_delta <- function(
    model_perturbed,
    vcov,
    FUN,
    mfx = NULL,
    type = NULL,
    newdata = NULL,
    eps = NULL,
    J = NULL,
    hypothesis = NULL,
    calling_function = NULL,
    comparison = NULL,
    by = NULL,
    hi = NULL,
    lo = NULL,
    original = NULL,
    estimates = NULL,
    ...) {
    # Use mfx slots when available
    if (!is.null(mfx)) {
        eps <- if (is.null(eps)) mfx@eps else eps
        calling_function <- if (is.null(calling_function)) mfx@calling_function else calling_function
        comparison <- if (is.null(comparison)) mfx@comparison else comparison
        by <- if (is.null(by)) mfx@by else by
    }
    # delta method does not work for these models
    bad <- c("brmsfit", "stanreg", "bart")
    if (any(inherits(model_perturbed, bad))) {
        return(NULL)
    }

    coefs <- get_coef(model_perturbed, ...)

    # some vcov methods return an unnamed matrix, some have duplicate names
    flag <- anyDuplicated(colnames(vcov)) == 0 &&
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
        fun <- settings_get("jacobian_function")
        if (is.null(fun)) {
            fun <- function(...) NULL
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
            hi = hi,
            lo = lo,
            original = original,
            estimates = estimates,
            comparison = comparison,
            calling_function = calling_function
        )
        checkmate::assert_matrix(J, mode = "numeric", ncols = length(coefs), null.ok = TRUE)

        # Unpad Jacobian for autodiff jax_jacobian if newdata was padded
        # JAX computes jacobian on padded newdata, but final results are unpadded
        if (!is.null(J) && !is.null(mfx) && "rowid" %in% colnames(mfx@newdata)) {
            idx <- mfx@newdata$rowid > 0
            if (!all(idx) && nrow(J) == nrow(mfx@newdata)) {
                J <- J[idx, , drop = FALSE]
            }
        }
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
            by = by
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

        g <- do_call(FUN, args)

        return(g)
    }

    if (is.null(J) || !is.null(hypothesis)) {
        args <- list(
            func = inner,
            x = coefs,
            numderiv = mfx@numderiv
        )
        J <- do.call("get_jacobian", args)
        colnames(J) <- names(coefs)
    }

    propagated <- std_error_from_jacobian(J, vcov, model_perturbed, ...)
    se <- propagated$std.error
    attr(se, "jacobian") <- propagated$jacobian

    return(se)
}
