sanitize_newdata <- function(model, newdata, by = NULL, modeldata = NULL) {

    checkmate::assert(
        checkmate::check_data_frame(newdata, null.ok = TRUE),
        checkmate::check_choice(newdata, choices = c("mean", "median", "tukey", "grid", "marginalmeans")),
        combine = "or")

    # to respect the `by` argument, we need all values to be preserved
    if (isTRUE(checkmate::check_data_frame(by))) {
        by <- setdiff(colnames(by), "by")
    }
    args <- list(model = model)
    for (b in by) {
        args[[b]] <- unique
    }

    # we always need this to extract attributes
    if (is.null(modeldata)) {
        modeldata <- hush(insight::get_data(model))
        # cannot extract data on unsupported custom models (e.g., numpyro)
        if (is.null(modeldata)) {
            modeldata <- newdata
        }
    }

    if (is.null(newdata)) {
        newdata <- modeldata

    } else if (identical(newdata, "mean")) {
        newdata <- do.call("datagrid", args)

    } else if (identical(newdata, "median")) {
        args[["FUN.numeric"]] <- function(x) stats::median(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)

    } else if (identical(newdata, "tukey")) {
        args[["FUN.numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)

    } else if (identical(newdata, "grid")) {
        args[["FUN.numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        args[["FUN.factor"]] <- args[["FUN.character"]] <- args[["FUN.logical"]] <- unique
        newdata <- do.call("datagrid", args)

    # grid with all unique values of categorical variables, and numerics at their means
    } else if (identical(newdata, "marginalmeans")) {
        args[["FUN.factor"]] <- args[["FUN.character"]] <- args[["FUN.logical"]] <- unique
        newdata <- do.call("datagrid", args)
    }

    if (!inherits(newdata, "data.frame")) {
        msg <- format_msg(
        "Unable to extract the data from model of class `%s`. This can happen in a
        variety of cases, such as when a `marginaleffects` package function is called
        from inside a user-defined function. Please supply a data frame explicitly via
        the `newdata` argument.")
        msg <- sprintf(msg, class(model)[1])
        stop(msg, call. = FALSE)
    }

    # column subsets later and predict
    setDF(modeldata)

    # column attributes
    mc <- Filter(function(x) is.matrix(modeldata[[x]]), colnames(modeldata))
    cl <- Filter(function(x) is.character(modeldata[[x]]), colnames(modeldata))
    cl <- lapply(modeldata[, cl], unique)
    vc <- sapply(names(modeldata), find_variable_class, newdata = modeldata, model = model)
    column_attributes <- list(
        "matrix_columns" = mc,
        "character_levels" = cl,
        "variable_class" = vc)

    # {modelbased} sometimes attaches useful attributes
    exclude <- c("class", "row.names", "names", "data", "reference")
    modelbased_attributes <- get_attributes(newdata, exclude = exclude)

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
    idx <- sapply(newdata, function(x) class(x)[1] == "matrix")
    if (any(idx)) {
        # Issue #363
        # unpacking matrix columns works with {mgcv} but breaks {mclogit}
        if (inherits(model, "gam")) {
            newdata <- unpack_matrix_cols(newdata)
        } else {
            newdata <- newdata[, !idx, drop = FALSE]
        }
    }

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

    # attributes
    newdata <- set_attributes(newdata, modelbased_attributes, prefix = "newdata_")
    newdata <- set_attributes(newdata, column_attributes, prefix = "newdata_")
    attr(newdata, "newdata_modeldata") <- modeldata

    # we will need this to merge the original data back in, and it is better to
    # do it in a centralized upfront way.
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    # placeholder response; sometimes insight::get_predicted breaks without this
    resp <- insight::find_response(model)
    if (isTRUE(checkmate::check_character(resp, len = 1)) &&
        !resp %in% colnames(newdata)) {
        y <- insight::get_response(model)
        # protect df or matrix response
        if (isTRUE(checkmate::check_atomic_vector(y))) {
            newdata[[resp]] <- y[1]
        }
    }

    return(newdata)
}



