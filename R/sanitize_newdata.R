sanity_newdata <- function(model, newdata) {

    checkmate::assert(
        checkmate::check_data_frame(newdata, null.ok = TRUE),
        checkmate::check_choice(newdata, choices = c("mean", "median", "marginalmeans")),
        combine = "or")

    if (is.null(newdata)) {
        newdata <- suppressWarnings(insight::get_data(model))

    } else if (isTRUE(checkmate::check_choice(newdata, "mean"))) {
        newdata <- datagrid(model = model)

    } else if (isTRUE(checkmate::check_choice(newdata, "median"))) {
        newdata <- datagrid(model = model, FUN.numeric = function(x) stats::median(x, na.rm = TRUE))

    } else if (isTRUE(checkmate::check_choice(newdata, "marginalmeans"))) {
        newdata <- datagrid(
            model = model,
            FUN.factor = unique,
            FUN.character = unique,
            FUN.logical = unique)
    }

    if (!inherits(newdata, "data.frame")) {
        msg <- sprintf("Unable to extract the data from model of class `%s`. This can happen in a variety of cases, such as when a `marginaleffects` package function is called from inside a user-defined function. Please supply a data frame explicitly via the `newdata` argument.", class(model)[1])
        stop(msg, call. = FALSE)
    }

    # required for the type of column indexing to follow
    data.table::setDF(newdata)

    # mlogit: each row is an individual-choice, but the index is not easily
    # trackable, so we pre-sort it here, and the sort in `get_predict()`. We
    # need to cross our fingers, but this probably works.
    if (inherits(model, "mlogit") && isTRUE(inherits(newdata[["idx"]], "idx"))) {
        idx <- list(newdata[["idx"]][, 1], newdata[["idx"]][, 2])
        newdata <- newdata[order(newdata[["idx"]][, 1], newdata[["idx"]][, 2]),]
    }

    # rbindlist breaks on matrix columns
    idx <- sapply(newdata, function(x) class(x)[1] != "matrix")
    newdata <- newdata[, idx, drop = FALSE]

    # if there are no categorical variables in `newdata`, check the model terms
    # to find transformation and warn accordingly.
    categorical_variables <- find_categorical(newdata = newdata, model = model)
    flag <- FALSE
    if (length(categorical_variables) == 0) {
        termlabs <- try(attr(stats::terms(model), "term.labels"), silent = TRUE)
        termlabs <- try(any(grepl("^factor\\(|^as.factor\\(|^as.logical\\(", termlabs)), silent = TRUE)
        if (isTRUE(termlabs)) {
            flag <- TRUE
        }
    }
    # This may be useful but it raises way too many warnings
    #} else {
    #     for (cv in categorical_variables) {
    #         if (is.numeric(newdata[[cv]])) {
    #             flag <- TRUE
    #         }
    #     }
    # }

    # we will need this to merge the original data back in, and it is better to
    # do it in a centralized upfront way.
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    return(newdata)
}


