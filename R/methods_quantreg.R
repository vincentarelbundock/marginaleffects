#' @rdname get_predict
#' @export
get_predict.rq <- function(
    model,
    newdata = insight::get_data(model),
    type = NULL,
    ...
) {
    # type argument of the method is used to specify confidence interval type
    insight::check_if_installed("quantreg")
    if (
        isTRUE(
            getOption("marginaleffects_linalg", default = "RcppEigen") == "RcppEigen"
        )
    ) {
        MM <- attr(newdata, "marginaleffects_model_matrix")
        if (isTRUE(checkmate::check_matrix(MM))) {
            beta <- get_coef(model)
            out <- hush(eigenMatMult(MM, beta))
            if (isTRUE(checkmate::check_numeric(out, len = nrow(newdata)))) {
                out <- data.frame(rowid = newdata$rowid, estimate = out)
                return(out)
            } else {
                out <- data.frame(rowid = seq_len(length(out)), estimate = out)
            }
        }
    }
    out <- quantreg::predict.rq(model, newdata = newdata, ...)
    if (isTRUE(checkmate::check_numeric(out, len = nrow(newdata)))) {
        out <- data.frame(rowid = newdata$rowid, estimate = out)
    } else {
        out <- data.frame(rowid = seq_len(length(out)), estimate = out)
    }
    return(out)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific.rqs <- function(model, ...) {
    stop(
        "`marginaleffects` only supports `quantreg::rq` models with a single `tau` value.",
        call. = FALSE
    )
}

# #' @rdname get_model_matrix
# #' @keywords internal
# #' @export
# get_model_matrix.rq <- function(object, newdata) {
#         tt <- stats::terms(object)
#         Terms <- delete.response(tt)
#         m <- model.frame(Terms, newdata, na.action = na.pass, xlev = object$xlevels)
#         if (!is.null(cl <- attr(Terms, "dataClasses")))
#             stats::.checkMFClasses(cl, m)
#         X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
#         if (!isTRUE(nrow(X) == nrow(newdata))) {
#             return(NULL)
#         } else {
#             return(X)
#         }
# }
