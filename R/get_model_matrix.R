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
get_model_matrix.default <- function(object, newdata) {

    # faster
    if (class(object)[1] %in% c("lm", "glm")) { 
        out <- hush(stats::model.matrix(object, data = newdata))
    # more general
    } else { 
        out <- hush(insight::get_modelmatrix(object, data = newdata))
    }
    
    beta <- get_coef(object)
    if (!isTRUE(nrow(out) == nrow(newdata)) || !isTRUE(ncol(out) == length(beta))) {
        return(NULL)
    } else {
        return(out)
    }
}