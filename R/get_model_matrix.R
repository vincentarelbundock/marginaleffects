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
    # faster
    if (class(model)[1] %in% c("lm", "glm")) {
        out <- hush(stats::model.matrix(model, data = newdata))
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
