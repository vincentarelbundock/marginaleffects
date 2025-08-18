#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.multinom <- function(model, coefs, ...) {
    # internally, coefficients are held in the `wts` vector, with 0s
    # interspersed. When transforming that vector to a matrix, we see that the
    # first row and first column are all zeros.
    # NOTE: must use `newdata` in predict otherwise returns stored object.
    b_original <- get_coef(model)
    model$wts[match(b_original, model$wts)] <- coefs
    return(model)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.multinom <- function(model, ...) {
    out <- insight::get_parameters(model, ...)
    out <- stats::setNames(
        out$Estimate,
        sprintf("%s:%s", out$Response, out$Parameter)
    )
    return(out)
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.multinom <- function(model, ...) {
    resp <- insight::get_response(model)
    if (is.factor(resp)) {
        out <- levels(resp)
    } else {
        out <- unique(resp)
    }
    return(out[2:length(out)])
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.multinom <- function(
    model,
    newdata = insight::get_data(model),
    type = "probs",
    mfx = NULL,
    ...) {
    calling_function <- if (!is.null(mfx)) mfx@calling_function else "predictions"
    type <- sanitize_type(model, type, calling_function = calling_function)

    is_latent <- is_mclogit <- is_nnet <- FALSE
    if (isTRUE(type == "latent") && inherits(model, c("mblogit", "mclogit"))) {
        is_latent <- TRUE
        is_mclogit <- TRUE
        type <- "link"
    } else if (isTRUE(type == "latent") && inherits(model, "multinom")) {
        is_latent <- TRUE
        is_nnet <- TRUE
        type <- "probs"
    }

    # needed because `predict.multinom` uses `data` rather than `newdata`
    pred <- stats::predict(model, newdata = newdata, type = type, ...)

    # atomic vector means there is only one row in `newdata`
    # two levels DV returns a vector
    if (isTRUE(checkmate::check_atomic_vector(pred))) {
        y_original <- sort(unique(insight::get_response(model)))
        two_levels <- length(y_original) == 2
        if (isTRUE(two_levels)) {
            pred <- matrix(pred)
            colnames(pred) <- as.character(y_original[2])
        } else {
            pred <- matrix(pred, nrow = 1, dimnames = list(NULL, names(pred)))
        }
    }

    if (is_latent && is_mclogit) {
        missing_level <- as.character(unique(insight::get_response(model)))
        missing_level <- setdiff(missing_level, colnames(pred))
        if (length(missing_level) == 1) {
            pred <- cbind(0, pred)
            colnames(pred)[1] <- missing_level
            pred <- pred - rowMeans(pred)
        } else {
            stop_sprintf(
                "Unable to compute predictions on the latent scale."
            )
        }
    } else if (is_latent && is_nnet) {
        inverse_softMax <- function(mu) {
            log_mu <- log(mu)
            return(sweep(log_mu, 1, STATS = rowMeans(log_mu), FUN = "-"))
        }
        pred <- inverse_softMax(pred)
    }

    # matrix with outcome levels as columns
    out <- data.frame(
        group = rep(colnames(pred), each = nrow(pred)),
        estimate = c(pred)
    )
    out$group <- group_to_factor(out$group, model)
    out <- add_rowid(out, newdata)

    return(out)
}
