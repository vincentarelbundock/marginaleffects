# We no longer need the sanity_model_specific method because `newdata` is not supported at all for mlogit models



# #' @include sanity_model.R
# #' @rdname sanity_model_specific
# #' @keywords internal
# sanity_model_specific.mlogit <- function(model, newdata, ...) {
#
#     is_balanced <- TRUE
#
#     if ("idx" %in% colnames(newdata) && inherits(newdata[["idx"]], "idx")) {
#         is_balanced <-
#             # each individual appears an equal number of times
#             isTRUE(length(unique(table(newdata[["idx"]][[1]]))) == 1) &&
#             # each outcome appears an equal number of times
#             isTRUE(length(unique(table(newdata[["idx"]][[2]]))) == 1)
#     }
#
#     # only allow on original data extracted by `get_data` because otherwise we have no way of 
#
#     if (!isTRUE(is_balanced)) {
#         stop("The `newdata` used for `mlogit` models must be balanced.")
#     }
# }
#

#' @rdname get_predict
#' @export
get_predict.mlogit <- function(model,
                               newdata,
                               ...) {
    out <- stats::predict(model, newdata = newdata)
    out <- data.table(out)
    out[, "rowid" := seq_len(.N)]
    out <- melt(out,
                id.vars = "rowid",
                variable.name = "group",
                value.name = "predicted")
    setkey(out, rowid, group)
    if ("term" %in% colnames(newdata)) {
        out[, "term" := newdata[["term"]]]
    }
    return(out)
}
