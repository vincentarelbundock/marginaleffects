#' Get marginal effects and standard errors (internal function)
#'
#' @return A data.frame with term names, group names, marginal effects, and
#' standard errors.
#' @param variable A string to identify the variable whose marginal effect to compute.
#' @param fitfram A data.frame over which to compute marginal effects.
#' @param group_name String to identify the "group" or "level" of the terms to
#'   estimate. Groups are often used in models like multinomial logit where each
#'   level of the response variable is associated to its own set of coefficients.
#' @inheritParams marginaleffects
#' @rdname get_dydx_and_se
#' @keywords internal
#' @export
get_dydx_and_se <- function (model, ...) {
    UseMethod("get_dydx_and_se", model)
}

#' @rdname get_dydx_and_se
#' @export
get_dydx_and_se.default <- function(model, 
                                    variables, 
                                    fitfram = insight::get_data(model), 
                                    vcov = stats::vcov(model), 
                                    group_name = NULL,
                                    type = "response",
                                    numDeriv_method = "simple", 
                                    ...) {

    coefs <- get_coef(model)

    if (!is.null(vcov)) {
        vcov <- vcov[names(coefs), names(coefs)]
    }

    get_one_variable <- function(v) {
        if (is.factor(fitfram[[v]]) || is.logical(fitfram[[v]]) || is.character(fitfram[[v]])) {
            dydx_fun <- get_dydx_categorical
        } else {
            dydx_fun <- get_dydx_continuous
        }

        # Marginal effects
        g <- dydx_fun(model = model,
                      fitfram = fitfram,
                      variable = v,
                      group_name = group_name,
                      type = type,
                      numDeriv_method = numDeriv_method,
                      ...)

        one_variable <- list()
        one_variable[["g"]] <- g

        # Standard errors
        if (!is.null(vcov)) {
            inner <- function(x) {
                model_tmp <- set_coef(model, stats::setNames(x, names(coefs)))
                g <- dydx_fun(model = model_tmp,
                              fitfram = fitfram,
                              variable = v,
                              group_name = group_name,
                              type = type,
                              numDeriv_method = numDeriv_method)
                return(g$dydx)
            }
            J <- numDeriv::jacobian(func = inner, 
                                    x = coefs,
                                    method = numDeriv_method)
            colnames(J) <- names(get_coef(model))
            J_mean <- stats::aggregate(J, by = list(g$term), mean)
            row.names(J_mean) <- J_mean[[1]]
            J_mean[[1]] <- NULL
            one_variable[["J_mean"]] <- as.matrix(J_mean)
            one_variable[["J"]] <- J
        }
        return(one_variable)
    }

    g_list <- list()
    J_list <- list()
    J_mean_list <- list()
    for (v in variables) {
        tmp <- get_one_variable(v)
        if ("g" %in% names(tmp)) {
            g_list[[v]] <- tmp$g
        }
        if ("J" %in% names(tmp)) {
            J_list[[v]] <- tmp$J
            J_mean_list[[v]] <- tmp$J_mean
        } else {
            J_list[[v]] <- NULL
            J_mean_list[[v]] <- NULL
        }
    }

    out <- do.call("rbind", g_list)
    row.names(out) <- NULL

    if (!is.null(vcov)) {
        # Keep row.names for J and J_mean matrices
        J <- do.call("rbind", J_list)
        J_mean <- do.call("rbind", J_mean_list)
        attr(out, "J") <- J
        attr(out, "J_mean") <- J_mean

        # Unit-level standard errors are much slower to compute (are they, though?)
        # Var(dydx) = J Var(beta) J'
        # computing the full matrix is memory-expensive, and we only need the diagonal
        # algebra trick: https://stackoverflow.com/a/42569902/342331
        V <- colSums(t(J %*% vcov) * t(J))
        out$std.error <- sqrt(V)

        # Standard errors at the mean gradient (this is what `Stata` and `margins` report)
        V <- colSums(t(J_mean %*% vcov) * t(J_mean))
        tmp <- data.frame("term" = names(V), "std.error" = sqrt(V))
        row.names(tmp) <- NULL
        attr(out, "se_at_mean_gradient") <- tmp
    }

    return(out)
}
