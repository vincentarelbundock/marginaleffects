# #' @rdname get_predict
# #' @export
# get_predict.mlogit <- function(model,
#                                newdata,
#                                ...) {
#
#     mat <- stats::predict(model, newdata = as.data.frame(newdata))
#     if (isTRUE(checkmate::check_atomic_vector(mat))) {
#         out <- data.table(rowid = seq_along(mat),
#                           group = names(mat),
#                           estimate = mat)
#     } else {
#     out <- data.table(rowid = rep(seq_len(nrow(mat)), rep = ncol(mat)),
#                       group = rep(colnames(mat), each = nrow(mat)),
#                       estimate = as.vector(mat))
#     }
#     setkey(out, rowid, group)
#     if ("term" %in% colnames(newdata)) {
#         out[, "term" := newdata[["term"]]]
#     }
#     # do not convert to factor because DV is often "yes" or "no" while the "group" is outcome levels.
#     # out$group <- group_to_factor(out$group, model)
#     return(out)
# }
#

#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific.mlogit <- function(model, ...) {
    msg <- "Support for `mlogit` models was deprecated in version 0.23.0. The reason is that the data structure for these models is one observation-choice per row. Every other model-fitting package supported by `marginaleffects` treats rows as individual observations. The observation-choice structure made it harder to track indices and match individual predictions to rows in the original data. This added a lot of complexity to `marginaleffects`, and the results were not always reliable or safe."
    insight::format_error(msg)
    # if (!is.null(newdata)) {
    #     nchoices <- length(unique(model$model$idx[, 2]))
    #     if (!isTRUE(nrow(newdata) %% nchoices == 0)) {
    #         msg <- sprintf("The `newdata` argument for `mlogit` models must be a data frame with a number of rows equal to a multiple of the number of choices: %s.", nchoices)
    #         stop(msg, call. = FALSE)
    #     }
    # }
    # return(model)
}
