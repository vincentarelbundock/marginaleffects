#' @rdname set_coef
#' @export
set_coef.flexsurvreg <- function(model, coefs, ...) {
    out <- model
    out$res[, 1] <- coefs
    out$coefficients <- coefs
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.flexsurvreg <- function(model, newdata, type, ...) {
    preds <- stats::predict(
        object = model,
        newdata = newdata,
        type = type,
        ...
    )

    if (ncol(preds) == 1L) {
        if (names(preds) == ".pred") {
            gp <- unlist(lapply(preds$.pred, function(x) {
                x[, 1, drop = TRUE]
            }))
            val <- unlist(lapply(preds$.pred, function(x) {
                x[, 2, drop = TRUE]
            }))

            out <- data.table(
                group = as.vector(gp),
                estimate = as.vector(val)
            )
            out$group <- group_to_factor(out$group, model)
            return(out)
        }
        out <- data.table(estimate = as.vector(preds[, 1, drop = TRUE]))
        return(out)
    }

    out <- data.table(
        group = as.vector(preds[, 1, drop = TRUE]),
        estimate = as.vector(preds[, 2, drop = TRUE])
    )
    out <- add_rowid(out, newdata)
    out$group <- group_to_factor(out$group, model)
    out
}
