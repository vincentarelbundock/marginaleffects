#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.crch <- function(model, coefs, ...) {
    # in crch::crch, coefficients are held in:
    # model$coefficients$location
    # model$coefficients$scale
    out <- model
    # this is weird, I know
    out$coefficients["location"]$location <-
        coefs[1:length(out$coefficients["location"]$location)]
    out$coefficients["scale"]$scale <-
        coefs[(length(out$coefficients["location"]$location) + 1):length(coefs)]
    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.crch <- function(model,
                             newdata = NULL,
                             vcov = FALSE,
                             conf_level = 0.95,
                             type = "location", ...) {

    pred <- stats::predict(model, newdata = newdata, type = type)
    sanity_predict_vector(pred = pred, model = model, newdata = newdata, type = type)
    sanity_predict_numeric(pred = pred, model = model, newdata = newdata, type = type)
    out <- data.frame(
        rowid = 1:nrow(newdata),
        predicted = pred)
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.hlxr <- function(model, coefs, ...) {
    # in crch::crch, coefficients are held in:
    # model$coefficients$location
    # model$coefficients$scale
    out <- model

    idx_int <- length(model$coefficients$intercept)
    idx_loc <- length(model$coefficients$location)

    # this is weird, I know
    out$coefficients["intercept"]$intercept <- coefs[1:idx_int]
    out$coefficients["location"]$location <- coefs[(idx_int + 1):(idx_int + 1 + idx_loc)]
    out$coefficients["scale"]$scale <- coefs[(idx_int + idx_loc + 1):length(coefs)]

    return(out)
}


get_predict.hlxr <- get_predict.crch
