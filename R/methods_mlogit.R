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
    out <- as.vector(out)

    group_idx <- model[["model"]][["idx"]][[2]]

    if (nrow(newdata) %% length(group_idx) != 0) {
        stop("mlogit: Unable to extract a choice index that matches the size of the prediction vector.", call. = FALSE)

    }
    n_terms <- length(out) / table(group_idx)[1]

    out <- data.frame(rowid = newdata[["rowid"]],
                      group = rep(group_idx, each = n_terms),
                      predicted = out)
    return(out)
}

