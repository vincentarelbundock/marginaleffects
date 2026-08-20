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
        out <- tryCatch(
            get_predict.default(
                model = model,
                newdata = newdata,
                type = type,
                ...
            ),
            error = function(e) e
        )
        if (!inherits(out, "error")) {
            return(out)
        }
        # `stats::predict()` failed. Linear predictions only need the model
        # matrix and the coefficients, so retry that way. This rescues fit
        # objects stripped of components that `predict()` requires, such as
        # `qr$qr` deleted to save memory.
        newdata <- model_matrix_retry(model, newdata, beta, out)
        MM <- attr(newdata, "marginaleffects_model_matrix")
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
    link <- isTRUE(checkmate::check_choice(type, "link"))
    response <- isTRUE(checkmate::check_choice(type, "response")) || is.null(type)
    MM <- attr(newdata, "marginaleffects_model_matrix")
    if (isTRUE(checkmate::check_matrix(MM)) && (link || response)) {
        out <- get_predict.lm(model = model, newdata = newdata, ...)
        if (response) {
            out$estimate <- stats::family(model)$linkinv(out$estimate)
        }
    }

    if (is.null(out)) {
        out <- tryCatch(
            get_predict.default(
                model = model,
                newdata = newdata,
                type = type,
                ...
            ),
            error = function(e) e
        )
        if (inherits(out, "error")) {
            # See the comment in `get_predict.lm()`: retry through the model
            # matrix, which only makes sense on the link and response scales.
            if (!link && !response) {
                stop(out)
            }
            newdata <- model_matrix_retry(
                model,
                newdata,
                get_coef(model),
                out
            )
            out <- get_predict.lm(model = model, newdata = newdata, ...)
            if (response) {
                out$estimate <- stats::family(model)$linkinv(out$estimate)
            }
        }
    }

    return(out)
}

#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.nls <- function(model, coefs, ...) {
    out <- model
    # `model$m` is a list of closures which all share the environments built by
    # `stats:::nlsModel()`. Calling `setPars()` on it writes through to the
    # caller's model: the object the Jacobian loop perturbs is the same object
    # it perturbs *from*, which leaves the user's fitted model holding the last
    # perturbed parameters. Clone the environments before writing.
    out$m <- clone_nlsModel(model$m)
    out$m$setPars(coefs)
    return(out)
}


# Deep copy of the closure environments behind an `nlsModel` object. Two
# environments matter: the frame of `nlsModel()` itself (holding `setPars`, the
# QR decomposition, ...) and the `env` it created to hold the data and the
# current parameter values.
clone_nlsModel <- function(m) {
    e_model <- environment(m[["setPars"]])
    if (!is.environment(e_model)) {
        return(m)
    }
    clone_env <- function(e) {
        out <- new.env(parent = parent.env(e))
        for (nm in ls(e, all.names = TRUE)) {
            assign(nm, get(nm, envir = e, inherits = FALSE), envir = out)
        }
        out
    }
    new_model <- clone_env(e_model)
    e_data <- get0("env", envir = e_model, inherits = FALSE)
    if (is.environment(e_data)) {
        assign("env", clone_env(e_data), envir = new_model)
    }
    rebind <- function(f) {
        if (is.function(f) && identical(environment(f), e_model)) {
            environment(f) <- new_model
        }
        f
    }
    for (nm in ls(new_model, all.names = TRUE)) {
        assign(
            nm,
            rebind(get(nm, envir = new_model, inherits = FALSE)),
            envir = new_model
        )
    }
    out <- lapply(m, rebind)
    attributes(out) <- attributes(m)
    return(out)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.nls <- function(model, ...) {
    model$m$getPars()
}
