#' @rdname set_coef
#' @export
set_coef.flexsurvreg <- function(model, coefs, ...) {
    out <- model

    # 'coefs' are on transformed (real-line) scale, same as coef(out)
    out$coefficients <- coefs
    out$res.t[, "est"] <- coefs

    # go back and set 'res' coefs on the parameters' "natural" scales
    out$res[, "est"] <- coefs
    for (j in seq_along(out$dlist$pars)) {
        p <- out$dlist$pars[j]
        out$res[p, "est"] <- out$dlist$inv.transforms[[j]](out$res.t[p, "est"])
    }

    out
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
