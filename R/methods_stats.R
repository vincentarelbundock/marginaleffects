#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glm <- function(model, coefs, ...) {
    model[["coefficients"]] <- sub_named_vector(
        model[["coefficients"]],
        coefs
    )
    ## But, there's an edge case!! When `predict(model, se.fit = TRUE)` is called without `newdata`, `predict.lm()` isn't called.
    ## Instead `model$linear.predictors` is returned directly if `type = "link"` and
    ## `model$fitted.values` is returned directly if `type = "response"`.
    ## `marginal_effects()` for "glm" is always called with `newdata`, so we won't hit this.
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
    newdata = insight::get_data(model),
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
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.glm <- function(
    model,
    newdata = insight::get_data(model),
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
get_autodiff_args.lm <- function(model, mfx) {
    if (!is.null(model$offset)) {
        autodiff_warning("models with offsets")
        return(NULL)
    }

    # Check type support
    if (!mfx@type %in% c("response", "link", "invlink(link)")) {
        autodiff_warning(sprintf("`type='%s'`", mfx@type))
        return(NULL)
    }

    # If all checks pass, return supported arguments
    out <- list(model_type = "linear")
    return(out)
}


#' @keywords internal
#' @export
get_autodiff_args.glm <- function(model, mfx) {
    if (!is.null(model$offset)) {
        autodiff_warning("models with offsets")
        return(NULL)
    }

    # Check type support
    if (!mfx@type %in% c("response", "link", "invlink(link)")) {
        autodiff_warning(sprintf("`type='%s'`", mfx@type))
        return(NULL)
    }

    # Check comparison type for comparisons function
    if (mfx@calling_function == "comparisons" && !mfx@comparison %in% c("difference", "ratio")) {
        autodiff_warning("other functions than `predictions()` or `comparisons()`, with `comparisons='difference'` or `'ratio'`")
        return(NULL)
    }

    # For GLM, check if family/link combination is supported
    family_name <- model$family$family
    link_name <- model$family$link

    # Supported family types
    supported_families <- c("gaussian", "binomial", "poisson", "Gamma")
    if (!family_name %in% supported_families) {
        autodiff_warning("unsupported GLM family/link combinations")
        return(NULL)
    }

    # Supported link types
    supported_links <- c("identity", "log", "logit", "probit", "inverse", "sqrt", "cloglog")
    if (!link_name %in% supported_links) {
        autodiff_warning("unsupported GLM family/link combinations")
        return(NULL)
    }

    # For link/invlink(link) type with GLM, use linear model approach
    model_type <- if (mfx@type %in% c("link", "invlink(link)")) "linear" else "glm"

    # Convert to Python enum values for GLM models
    family_type <- NULL
    link_type <- NULL
    if (model_type == "glm") {
        # Import Family and Link enums from Python
        Family <- mAD$glm$families$Family
        Link <- mAD$glm$families$Link

        # Family types using Python enum
        family_type <- switch(family_name,
            "gaussian" = Family$GAUSSIAN,
            "binomial" = Family$BINOMIAL,
            "poisson" = Family$POISSON,
            "Gamma" = Family$GAMMA,
            NULL
        )

        # Link types using Python enum
        link_type <- switch(link_name,
            "identity" = Link$IDENTITY,
            "log" = Link$LOG,
            "logit" = Link$LOGIT,
            "probit" = Link$PROBIT,
            "inverse" = Link$INVERSE,
            "sqrt" = Link$SQRT,
            "cloglog" = Link$CLOGLOG,
            NULL
        )
    }

    # If all checks pass, return supported arguments
    out <- list(
        model_type = model_type,
        family_type = family_type,
        link_type = link_type

    )
    return(out)
}
