sanitize_newdata_call <- function(scall, newdata = NULL, model, by = NULL) {
    if (rlang::quo_is_call(scall)) {
        df <- FALSE
        if (grepl("^datagrid", rlang::call_name(scall))) {
            if (!"model" %in% rlang::call_args_names(scall)) {
                scall <- rlang::call_modify(scall, model = model)
            }
        } else if (isTRUE(rlang::call_name(scall) == "data.frame")) {
            df <- TRUE
        } else if (isTRUE(rlang::call_name(scall) == "subset")) {
            argnames <- rlang::call_args_names(scall)
            if (!"x" %in% argnames && length(argnames) == 1) {
                tmp <- get_modeldata(model, additional_variables = by)
                scall <- rlang::call_modify(scall, x = tmp)
            }
        } else if (isTRUE(rlang::call_name(scall) == "filter")) {
            argnames <- rlang::call_args_names(scall)
            if (!".data" %in% argnames && length(argnames) == 1) {
                tmp <- get_modeldata(model, additional_variables = by)
                scall <- rlang::call_modify(scall, .data = tmp)
            }
        } else if (rlang::call_name(scall) %in% "visualisation_matrix") {
            if (!"x" %in% rlang::call_args_names(scall)) {
                scall <- rlang::call_modify(scall, x = get_modeldata)
            }
        }
        out <- rlang::eval_tidy(scall)
        # newdata=data.frame() all columns must be printed as explicit in print.R
        if (isTRUE(df)) {
            attr(out, "implicit") <- unique(c(attr(out, "implicit"), colnames(out)))
        }
    } else {
        out <- newdata
    }
    return(out)
}


build_newdata <- function(model, newdata, by, modeldata) {
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

    # NULL -> modeldata
    if (is.null(newdata)) {
        newdata <- modeldata
        newdata_explicit <- FALSE

        # string -> datagrid()
    } else if (identical(newdata, "mean")) {
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "median")) {
        args[["FUN_numeric"]] <- args[["FUN_integer"]] <- args[["FUN_logical"]] <- function(x) stats::median(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "tukey")) {
        args[["FUN_numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "grid")) {
        args[["FUN_numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        args[["FUN_factor"]] <- args[["FUN_character"]] <- args[["FUN_logical"]] <- unique
        newdata <- do.call("datagrid", args)

        # grid with all unique values of categorical variables, and numerics at their means
    } else if (identical(newdata, "balanced")) {
        args[["grid_type"]] <- "balanced"
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

    out <- list(
        "newdata" = newdata,
        "explicit" = newdata_explicit,
        "modeldata" = modeldata
    )
    return(out)
}


add_wts_column <- function(wts, newdata, model) {
    # weights must be available in the `comparisons()` function, NOT in
    # `tidy()`, because comparisons will often duplicate newdata for
    # multivariate outcomes and the like. We need to track which row matches
    # which.
    if (isFALSE(wts)) {
        return(newdata)
    } else if (isTRUE(wts)) {
        wtsname <- insight::find_weights(model)
        if (!is.character(wtsname) || length(wtsname) != 1 || !wtsname %in% colnames(newdata)) {
            msg <- "Unable to retrieve weights automatically from the model. Please specify `wts` argument explicitly."
            insight::format_error(msg)
        } else {
            newdata[["marginaleffects_wts_internal"]] <- newdata[[wtsname]]
            return(newdata)
        }
    }

    if (!isFALSE(wts)) {
        flag1 <- isTRUE(checkmate::check_string(wts)) && isTRUE(wts %in% colnames(newdata))
        flag2 <- isTRUE(checkmate::check_numeric(wts, len = nrow(newdata)))
        if (!flag1 && !flag2) {
            msg <- sprintf("The `wts` argument must be a numeric vector of length %s, or a string which matches a column name in `newdata`. If you did not supply a `newdata` explicitly, `marginaleffects` extracted it automatically from the model object, and the `wts` variable may not have been available. The easiest strategy is often to supply a data frame such as the original data to `newdata` explicitly, and to make sure that it includes an appropriate column of weights, identified by the `wts` argument.", nrow(newdata))
            stop(msg, call. = FALSE)
        }
    }

    # weights: before sanitize_variables
    if (!isFALSE(wts) && isTRUE(checkmate::check_string(wts))) {
        newdata[["marginaleffects_wts_internal"]] <- newdata[[wts]]
    } else {
        newdata[["marginaleffects_wts_internal"]] <- wts
    }

    return(newdata)
}


set_newdata_attributes <- function(model, modeldata, newdata, explicit) {
    attr(newdata, "explicit") <- explicit

    # column classes
    mc <- Filter(function(x) is.matrix(modeldata[[x]]), colnames(modeldata))
    cl <- Filter(function(x) is.character(modeldata[[x]]), colnames(modeldata))
    cl <- lapply(modeldata[, ..cl], unique)
    vc <- attributes(modeldata)$marginaleffects_variable_class
    column_attributes <- list(
        "matrix_columns" = mc,
        "character_levels" = cl,
        "variable_class" = vc)
    newdata <- set_marginaleffects_attributes(newdata, column_attributes)

    # {modelbased} sometimes attaches useful attributes
    exclude <- c("class", "row.names", "names", "data", "reference")
    modelbased_attributes <- get_marginaleffects_attributes(newdata, exclude = exclude)
    newdata <- set_marginaleffects_attributes(newdata, modelbased_attributes)

    # original data
    attr(newdata, "newdata_modeldata") <- modeldata

    if (is.null(attr(newdata, "marginaleffects_variable_class"))) {
        newdata <- set_variable_class(newdata, model = model)
    }

    return(newdata)
}


clean_newdata <- function(model, newdata) {
    # rbindlist breaks on matrix columns
    # scale() creates 1-column matrices
    for (i in seq_along(newdata)) {
        if (inherits(newdata[[i]], "matrix") && ncol(newdata[[i]]) == 1) {
            newdata[[i]] <- drop(newdata[[i]])
        }
    }

    # (more) rbindlist breaks on matrix columns
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

    # we will need this to merge the original data back in, and it is better to
    # do it in a centralized upfront way.
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    # mlogit: each row is an individual-choice, but the index is not easily
    # trackable, so we pre-sort it here, and the sort in `get_predict()`. We
    # need to cross our fingers, but this probably works.
    if (inherits(model, "mlogit") && isTRUE(inherits(newdata[["idx"]], "idx"))) {
        idx <- list(newdata[["idx"]][, 1], newdata[["idx"]][, 2])
        newdata <- newdata[order(newdata[["idx"]][, 1], newdata[["idx"]][, 2]), ]
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
    return(newdata)
}


sanitize_newdata <- function(model, newdata, by, modeldata, wts) {
    checkmate::assert(
        checkmate::check_data_frame(newdata, null.ok = TRUE),
        checkmate::check_choice(newdata, choices = c("mean", "median", "tukey", "grid", "balanced")),
        combine = "or")
    tmp <- build_newdata(
        model = model,
        newdata = newdata,
        by = by,
        modeldata = modeldata)
    newdata <- tmp[["newdata"]]
    modeldata <- tmp[["modeldata"]]

    newdata <- clean_newdata(model, newdata)
    modeldata <- clean_newdata(model, modeldata)
    newdata_explicit <- attr(newdata, "explicit")

    # required by some model-fitting functions
    data.table::setDT(modeldata)

    # required for the type of column indexing to follow
    data.table::setDT(newdata)

    if (is.null(wts)) wts <- FALSE
    newdata <- add_wts_column(newdata = newdata, wts = wts, model = model)
    newdata <- set_newdata_attributes(
        model = model,
        modeldata = modeldata,
        newdata = newdata,
        explicit = newdata_explicit)

    out <- data.table::copy(newdata)

    return(out)
}


dedup_newdata <- function(model, newdata, by, wts, comparison = "difference", cross = FALSE, byfun = NULL) {
    # issue #1113: elasticities or custom functions should skip dedup because it is difficult to align x and y
    elasticities <- c("eyexavg", "eydxavg", "dyexavg")
    if (isTRUE(checkmate::check_choice(comparison, elasticities)) ||
        isTRUE(checkmate::check_function(comparison))) {
        return(data.table(newdata))
    }

    elasticities <- c("eyex", "eydx", "dyex")
    if (!isFALSE(by) && isTRUE(checkmate::check_choice(comparison, elasticities))) {
        return(data.table(newdata))
    }

    flag <- isTRUE(checkmate::check_string(comparison, pattern = "avg"))
    if (!flag && (
        isFALSE(by) || # weights only make sense when we are marginalizing
            !isFALSE(wts) ||
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

    # rowid is useless, except for intercept-only models, where we want to retain all rows
    if ("rowid" %in% colnames(out) && ncol(out) > 1) {
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
