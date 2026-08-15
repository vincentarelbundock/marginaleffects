#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glm <- function(model, coefs, ...) {
    model[["coefficients"]] <- sub_named_vector(
        model[["coefficients"]],
        coefs
    )
    model
}


#' @rdname set_coef
#' @export
set_coef.lm <- function(model, coefs, ...) {
    model[["coefficients"]] <- sub_named_vector(
        model[["coefficients"]],
        coefs
    )
    model
}


#' @rdname get_predict
#' @export
get_predict.lm <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    ...) {
    MM <- attr(newdata, "marginaleffects_model_matrix")
    beta <- get_coef(model)
    if (!isTRUE(checkmate::check_matrix(MM)) || ncol(MM) != length(beta)) {
        out <- get_predict.default(
            model = model,
            newdata = newdata,
            type = type,
            ...
        )
        return(out)
    }
    p <- model$rank
    p1 <- seq_len(p)
    piv <- if (p) qr(model)$pivot[p1]
    if (!all(seq_len(ncol(MM)) %in% piv)) {
        MM <- MM[, piv, drop = FALSE]
        beta <- beta[piv]
    }
    pred <- drop(MM %*% beta)

    # `pred` is a secret argument which re-uses the default get_predict to format a vector a data frame using correct `rowid`
    out <- get_predict.default(
        model = model,
        newdata = newdata,
        type = type,
        pred = pred,
        ...
    )
    # Analytic derivatives can reuse this exact linear predictor because the
    # prediction above was computed from the cached model matrix.
    attr(out, "marginaleffects_linear_predictor") <- pred
    attr(out, "marginaleffects_model_matrix_used") <- TRUE
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.glm <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    ...) {
    out <- NULL
    MM <- attr(newdata, "marginaleffects_model_matrix")
    if (isTRUE(checkmate::check_matrix(MM))) {
        if (isTRUE(checkmate::check_choice(type, c("link")))) {
            out <- get_predict.lm(model = model, newdata = newdata, ...)
        } else if (isTRUE(checkmate::check_choice(type, "response")) || is.null(type)) {
            out <- get_predict.lm(model = model, newdata = newdata, ...)
            out$estimate <- stats::family(model)$linkinv(out$estimate)
        }
    }

    if (is.null(out)) {
        out <- get_predict.default(
            model = model,
            newdata = newdata,
            type = type,
            ...
        )
    }

    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.nls <- function(model, coefs, ...) {
    out <- model
    out$m$setPars(coefs)
    return(out)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.nls <- function(model, ...) {
    model$m$getPars()
}


#' @keywords internal
#' @export
get_autodiff_args.lm <- function(model, mfx, type) {
    # no inheritance! Important to avoid breaking other models
    if (!class(model)[1] %in% c("lm", "ivreg")) {
        return(NULL)
    }

    if (!is.null(model$offset)) {
        return("models with offsets")
    }

    if ("weights" %in% names(model$call)) {
        return("models with weights")
    }

    # Check type support
    if (!type %in% c("response", "link", "invlink(link)")) {
        return(sprintf("`type='%s'`", type))
    }

    # If all checks pass, return supported arguments
    out <- list(model_type = "linear", family = NULL, link = NULL)
    return(out)
}


#' @keywords internal
#' @export
get_autodiff_args.glm <- function(model, mfx, type) {
    # no inheritance! Important to avoid breaking other models
    if (!class(model)[1] == "glm") {
        return(NULL)
    }

    if (!is.null(model$offset)) {
        return("models with offsets")
    }

    if ("weights" %in% names(model$call)) {
        return("models with weights")
    }

    # Check type support
    if (!type %in% c("response", "link", "invlink(link)")) {
        return(sprintf("`type='%s'`", type))
    }

    if (type %in% c("link", "invlink(link)")) {
        return(list(model_type = "linear", family = NULL, link = NULL))
    }

    # For GLM, check if family/link combination is supported
    family_name <- model$family$family
    link_name <- model$family$link

    supported_families <- c("gaussian", "binomial", "poisson", "Gamma")
    if (!family_name %in% supported_families) {
        return("unsupported GLM family/link combinations")
    }

    supported_links <- list(
        gaussian = c("identity", "log", "inverse"),
        binomial = c("logit", "probit", "cloglog", "log"),
        poisson = c("log", "identity", "sqrt"),
        Gamma = c("inverse", "identity", "log")
    )
    if (!link_name %in% supported_links[[family_name]]) {
        return("unsupported GLM family/link combinations")
    }

    # If all checks pass, return supported arguments
    out <- list(
        model_type = "glm",
        family = family_name,
        link = link_name
    )
    return(out)
}
