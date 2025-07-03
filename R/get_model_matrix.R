#' Get a named model matrix
#'
#' @inheritParams slopes
#' @rdname get_model_matrix
#' @keywords internal
#' @export
get_model_matrix <- function(model, newdata) {
    UseMethod("get_model_matrix", model)
}


#' @rdname get_model_matrix
#' @keywords internal
#' @export
get_model_matrix.default <- function(model, newdata) {

    # some models require the response variable first value only allows us to
    # handle `newdata="balanced"` and friends. This is a hack, but it probably
    # doesn't matter.
    dv <- insight::find_response(model)
    if (!dv %in% colnames(newdata)) {
        newdata[[dv]] <- insight::get_response(model)[1]
    }

    # faster
    if (class(model)[1] %in% c("lm", "glm")) {
        out <- stats::model.matrix(model, data = newdata)
        # more general
    } else {
        out <- hush(insight::get_modelmatrix(model, data = newdata))
    }

    beta <- get_coef(model)
    if (!isTRUE(nrow(out) == nrow(newdata)) || !isTRUE(ncol(out) == length(beta))) {
        return(NULL)
    } else {
        return(out)
    }
}
