#' @rdname get_coef
#' @export
get_coef.brmultinom <- function(model, ...) {
    out <- insight::get_parameters(model)
    out <- stats::setNames(out$Estimate,
                           sprintf("%s:%s", out$Response, out$Parameter))
    return(out)
}


#' @include methods_nnet.R
#' @rdname get_predict
#' @export
get_predict.brmultinom <- get_predict.multinom

