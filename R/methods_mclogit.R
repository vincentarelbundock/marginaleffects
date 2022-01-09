#' @rdname get_group_names
#' @export
get_group_names.mblogit <- function(model, type, ...) {
    out <- get_predict(model, type = type)
    if ("group" %in% colnames(out)) {
        out <- unique(out$group)
    } else {
        out <- "main_marginaleffects"
    }
    return(out)
}
