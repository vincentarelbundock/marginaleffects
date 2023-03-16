sanitize_newdata_call <- function(scall, newdata = NULL, model) {
    if (is.call(scall)) {
        out <- NULL
        lcall <- as.list(scall)
        fun_name <- as.character(scall)[1]
        if (fun_name %in% c("datagrid", "datagridcf", "typical", "counterfactual")) {
            if (!"model" %in% names(lcall)) {
                lcall <- c(lcall, list("model" = model))
                # don't evalup because we want the informative error
                out <- eval(as.call(lcall))
            }
        } else if (fun_name == "visualisation_matrix") {
            if (!"x" %in% names(lcall)) {
                lcall <- c(lcall, list("x" = get_modeldata))
                # don't evalup because we want the informative error
                out <- eval(as.call(lcall))
            }
        }
        if (is.null(out)) {
            out <- evalup(scall)
        }
    } else {
        out <- newdata
    }
    return(out)
}


sanitize_newdata <- function(model, newdata, by, modeldata) {

    checkmate::assert(
        checkmate::check_data_frame(newdata, null.ok = TRUE),
        checkmate::check_choice(newdata, choices = c("mean", "median", "tukey", "grid", "marginalmeans")),
        combine = "or")

    # to respect the `by` argument, we need all values to be preserved
    if (isTRUE(checkmate::check_data_frame(by))) {
        by <- setdiff(colnames(by), "by")
    } else if (isTRUE(checkmate::check_flag(by))) {
        by <- NULL
    }
    args <- list(model = model)
    for (b in by) {
        args[[b]] <- unique
    }

    newdata_explicit <- TRUE

    if (is.null(newdata)) {
        newdata <- modeldata
        newdata_explicit <- FALSE

    } else if (identical(newdata, "mean")) {
        newdata <- do.call("datagrid", args)

    } else if (identical(newdata, "median")) {
        args[["FUN_numeric"]] <- function(x) stats::median(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)

    } else if (identical(newdata, "tukey")) {
        args[["FUN_numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)

    } else if (identical(newdata, "grid")) {
        args[["FUN_numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        args[["FUN_factor"]] <- args[["FUN_character"]] <- args[["FUN_logical"]] <- unique
        newdata <- do.call("datagrid", args)

    # grid with all unique values of categorical variables, and numerics at their means
    } else if (identical(newdata, "marginalmeans")) {
        args[["FUN_factor"]] <- args[["FUN_character"]] <- args[["FUN_logical"]] <- unique
        newdata <- do.call("datagrid", args)
        # Issue #580: outcome should not duplicate grid rows
        dv <- hush(insight::find_response(model))
        if (isTRUE(dv %in% colnames(newdata))) {
            newdata[[dv]] <- get_mean_or_mode(newdata[[dv]])
            newdata <- unique(newdata)
        }
    }

    if (!inherits(newdata, "data.frame")) {
        msg <- "Unable to extract the data from model of class `%s`. This can happen in a variety of cases, such as when a `marginaleffects` package function is called from inside a user-defined function, or using an `*apply()`-style operation on a list. Please supply a data frame explicitly via the `newdata` argument."
        msg <- sprintf(msg, class(model)[1])
        insight::format_error(msg)
    }

    # otherwise we get a warning in setDT()
    if (inherits(model, "mlogit") && isTRUE(inherits(modeldata[["idx"]], "idx"))) {
        modeldata$idx <- NULL
    }

    data.table::setDT(modeldata)

    # column attributes
    mc <- Filter(function(x) is.matrix(modeldata[[x]]), colnames(modeldata))
    cl <- Filter(function(x) is.character(modeldata[[x]]), colnames(modeldata))
    cl <- lapply(modeldata[, ..cl], unique)
    vc <- attributes(modeldata)$marginaleffects_variable_class
    column_attributes <- list(
        "matrix_columns" = mc,
        "character_levels" = cl,
        "variable_class" = vc)

    # {modelbased} sometimes attaches useful attributes
    exclude <- c("class", "row.names", "names", "data", "reference")
    modelbased_attributes <- get_marginaleffects_attributes(newdata, exclude = exclude)

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
    categorical_variables <- get_variable_class(newdata, compare = "categorical")
    flag <- FALSE
    if (length(categorical_variables) == 0) {
        termlabs <- try(attr(stats::terms(model), "term.labels"), silent = TRUE)
        termlabs <- try(any(grepl("^factor\\(|^as.factor\\(|^as.logical\\(", termlabs)), silent = TRUE)
        if (isTRUE(termlabs)) {
            flag <- TRUE
        }
    }

    # attributes
    newdata <- set_marginaleffects_attributes(newdata, modelbased_attributes, prefix = "newdata_")
    newdata <- set_marginaleffects_attributes(newdata, column_attributes, prefix = "newdata_")
    attr(newdata, "newdata_modeldata") <- modeldata

    # we will need this to merge the original data back in, and it is better to
    # do it in a centralized upfront way.
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    # placeholder response
    resp <- insight::find_response(model)
    if (isTRUE(checkmate::check_character(resp, len = 1)) &&
        !resp %in% colnames(newdata)) {
        y <- hush(insight::get_response(model))
        # protect df or matrix response
        if (isTRUE(checkmate::check_atomic_vector(y))) {
            newdata[[resp]] <- y[1]
        }
    }

    if (is.null(attr(newdata, "marginaleffects_variable_class"))) {
        newdata <- set_variable_class(newdata, model = model)
    }

    attr(newdata, "newdata_explicit") <- newdata_explicit

    return(newdata)
}


dedup_newdata <- function(model, newdata, by, wts, comparison = "difference", cross = FALSE, byfun = NULL) {

    flag <- isTRUE(checkmate::check_string(comparison, pattern = "avg"))
    if (!flag && (
        isFALSE(by) || # weights only make sense when we are marginalizing
        !is.null(wts) ||
        !is.null(byfun) ||
        !isFALSE(cross) ||
        isFALSE(getOption("marginaleffects_dedup", default = TRUE)))) {
        return(newdata)
    }
    
    vclass <- attr(newdata, "marginaleffects_variable_class")

    # copy to allow mod by reference later without overwriting newdata
    out <- data.table(newdata)

    dv <- hush(unlist(insight::find_response(model), use.names = FALSE))
    if (isTRUE(checkmate::check_string(dv)) && dv %in% colnames(out)) {
        out[, (dv) := NULL]
        vclass <- vclass[names(vclass) != dv]
    }

    if ("rowid" %in% colnames(out)) {
        out[, "rowid" := NULL]
    }
    
    categ <- c("factor", "character", "logical", "strata", "cluster", "binary")
    if (!all(vclass %in% categ)) {
        return(newdata)
    }
    
    cols <- colnames(out)
    out <- out[, .("marginaleffects_wts_internal" = .N), by = cols]
    data.table::setDF(out)
    
    out[["rowid_dedup"]] <- seq_len(nrow(out))
    attr(out, "marginaleffects_variable_class") <- vclass
    
    return(out)
}