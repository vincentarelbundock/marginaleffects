sanitize_newdata <- function(model, newdata) {

    checkmate::assert(
        checkmate::check_data_frame(newdata, null.ok = TRUE),
        checkmate::check_choice(newdata, choices = c("mean", "median", "tukey", "grid", "marginalmeans")),
        combine = "or")

    # we always need this to extract attributes
    modeldata <- hush(insight::get_data(model))

    if (is.null(newdata)) {
        newdata <- modeldata

    } else if (identical(newdata, "mean")) {
        newdata <- datagrid(model = model)

    } else if (identical(newdata, "median")) {
        newdata <- datagrid(
            model = model,
            FUN.numeric = function(x) stats::median(x, na.rm = TRUE))

    } else if (identical(newdata, "tukey")) {
        newdata <- datagrid(
            model = model,
            FUN.numeric = function(x) stats::fivenum(x, na.rm = TRUE))

    } else if (identical(newdata, "grid")) {
        newdata <- datagrid(
            model = model,
            FUN.numeric = function(x) stats::fivenum(x, na.rm = TRUE),
            FUN.factor = unique,
            FUN.character = unique,
            FUN.logical = unique)

    # grid with all unique values of categorical variables, and numerics at their means
    } else if (identical(newdata, "marginalmeans")) {
        newdata <- datagrid(
            model = model,
            FUN.factor = unique,
            FUN.character = unique,
            FUN.logical = unique)
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

    return(newdata)
}



