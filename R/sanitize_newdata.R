sanitize_newdata_call <- function(scall, newdata = NULL, mfx = NULL, by = NULL) {
    # For mice objects, defer newdata processing to process_imputation()
    if (!is.null(mfx) && inherits(mfx@model, c("mira", "amest"))) {
        # Return the call as-is, will be processed later with actual model data
        return(scall)
    }

    if (rlang::quo_is_call(scall)) {
        df <- FALSE
        if (grepl("^datagrid", rlang::call_name(scall))) {
            if (!"model" %in% rlang::call_args_names(scall)) {
                scall <- rlang::call_modify(scall, model = mfx@model)
            }
        } else if (isTRUE(rlang::call_name(scall) == "data.frame")) {
            df <- TRUE
        } else if (isTRUE(rlang::call_name(scall) == "subset")) {
            argnames <- rlang::call_args_names(scall)
            if (!"x" %in% argnames && length(argnames) == 1) {
                scall <- rlang::call_modify(scall, x = mfx@modeldata)
            }
        } else if (isTRUE(rlang::call_name(scall) == "filter")) {
            argnames <- rlang::call_args_names(scall)
            if (!".data" %in% argnames && length(argnames) == 1) {
                scall <- rlang::call_modify(scall, .data = mfx@modeldata)
            }
        } else if (rlang::call_name(scall) %in% "visualisation_matrix") {
            if (!"x" %in% rlang::call_args_names(scall)) {
                scall <- rlang::call_modify(scall, x = mfx@modeldata)
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


build_newdata <- function(mfx, newdata, by) {
    model <- mfx@model
    modeldata <- mfx@modeldata

    # For mice objects, defer processing - newdata might be a call
    if (inherits(model, c("mira", "amest"))) {
        # Return placeholder that will be handled in process_imputation()
        return(list(
            "newdata" = newdata, # could be a call like subset(treat == 1)
            "modeldata" = modeldata
        ))
    }

    if (isTRUE(checkmate::check_data_frame(by))) {
        by <- setdiff(colnames(by), "by")
    } else if (isTRUE(checkmate::check_flag(by))) {
        by <- NULL
    }
    args <- list(model = model)
    for (b in by) {
        args[[b]] <- unique
    }

    # NULL -> modeldata
    if (is.null(newdata)) {
        newdata <- modeldata

        # string -> datagrid()
    } else if (identical(newdata, "mean")) {
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "median")) {
        args[["FUN_numeric"]] <- args[["FUN_integer"]] <- args[[
            "FUN_logical"
        ]] <- function(x) stats::median(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "tukey")) {
        args[["FUN_numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "grid")) {
        args[["FUN_numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        args[["FUN_factor"]] <- args[["FUN_character"]] <- args[[
            "FUN_logical"
        ]] <- unique
        newdata <- do.call("datagrid", args)

        # grid with all unique values of categorical variables, and numerics at their means
    } else if (identical(newdata, "balanced")) {
        args[["grid_type"]] <- "balanced"
        newdata <- do.call("datagrid", args)
        # Issue #580: outcome should not duplicate grid rows
        # there can be multiple response variables
        dv <- mfx@variable_names_response
        for (d in dv) {
            if (isTRUE(d %in% colnames(newdata))) {
                newdata[[d]] <- get_mean_or_mode(newdata[[d]])
                newdata <- unique(newdata)
            }
        }
    }

    if (!inherits(newdata, "data.frame")) {
        msg <- "Unable to extract the data from model of class `%s`. This can happen in a variety of cases, such as when a `marginaleffects` package function is called from inside a user-defined function, or using an `*apply()`-style operation on a list. Please supply a data frame explicitly via the `newdata` argument."
        msg <- sprintf(msg, class(model)[1])
        stop_sprintf(msg)
    }

    out <- list(
        "newdata" = newdata,
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
    }

    if (isTRUE(wts)) {
        wtsname <- insight::find_weights(model)
        if (
            !is.character(wtsname) ||
                length(wtsname) != 1 ||
                !wtsname %in% colnames(newdata)
        ) {
            msg <- "Unable to retrieve weights automatically from the model. Please specify `wts` argument explicitly."
            stop_sprintf(msg)
        } else {
            newdata[["marginaleffects_wts_internal"]] <- newdata[[wtsname]]
            return(newdata)
        }
    }

    flag1 <- isTRUE(checkmate::check_string(wts)) &&
        isTRUE(wts %in% colnames(newdata))
    flag2 <- isTRUE(checkmate::check_numeric(wts, len = nrow(newdata)))
    if (!flag1 && !flag2) {
        msg <- sprintf(
            "The `wts` argument must be a numeric vector of length %s, or a string which matches a column name in `newdata`. If you did not supply a `newdata` explicitly, `marginaleffects` extracted it automatically from the model object, and the `wts` variable may not have been available. The easiest strategy is often to supply a data frame such as the original data to `newdata` explicitly, and to make sure that it includes an appropriate column of weights, identified by the `wts` argument.",
            nrow(newdata)
        )
        stop(msg, call. = FALSE)
    }

    # weights: before sanitize_variables
    if (isTRUE(checkmate::check_string(wts))) {
        newdata[["marginaleffects_wts_internal"]] <- newdata[[wts]]
    } else {
        newdata[["marginaleffects_wts_internal"]] <- wts
    }

    return(newdata)
}


sanitize_newdata <- function(mfx, newdata, by, wts) {
    # For mice objects, skip validation as newdata might be a deferred call
    if (!inherits(mfx@model, c("mira", "amest"))) {
        checkmate::assert(
            checkmate::check_data_frame(newdata, null.ok = TRUE),
            checkmate::check_choice(
                newdata,
                choices = c("mean", "median", "tukey", "grid", "balanced")
            ),
            combine = "or"
        )
    }

    # overwrite with processed `newdata`
    tmp <- build_newdata(
        mfx = mfx,
        newdata = newdata,
        by = by
    )
    model <- mfx@model
    newdata <- tmp[["newdata"]]
    modeldata <- tmp[["modeldata"]]

    # Issue #1327: matrix columns with single column breaks rbindlist(). See `scale()`
    newdata <- unpack_matrix_1col(newdata)

    # Issue #363
    # unpacking matrix columns works with {mgcv} but breaks {mclogit}
    if (inherits(model, "gam")) {
        newdata <- unpack_matrix_cols(newdata)
    }

    # placeholder response
    resp <- mfx@variable_names_response
    if (
        isTRUE(checkmate::check_character(resp, len = 1)) &&
            !resp %in% colnames(newdata)
    ) {
        y <- modeldata[[resp]]
        # protect df or matrix response
        if (isTRUE(checkmate::check_atomic_vector(y))) {
            newdata[[resp]] <- y[1]
        }
    }

    # we will need this to merge the original data back in, and it is better to
    # do it in a centralized upfront way.
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    data.table::setDT(newdata)

    return(newdata)
}


dedup_newdata <- function(
    mfx,
    newdata,
    by,
    wts,
    comparison = "difference",
    cross = FALSE,
    byfun = NULL) {
    # init
    model <- mfx@model

    # issue #1113: elasticities or custom functions should skip dedup because it is difficult to align x and y
    elasticities <- c("eyexavg", "eydxavg", "dyexavg")
    if (
        isTRUE(checkmate::check_choice(comparison, elasticities)) ||
            isTRUE(checkmate::check_function(comparison))
    ) {
        return(newdata)
    }

    elasticities <- c("eyex", "eydx", "dyex")
    if (!isFALSE(by) && isTRUE(checkmate::check_choice(comparison, elasticities))) {
        return(data.table(newdata))
    }

    flag <- isTRUE(checkmate::check_string(comparison, pattern = "avg"))
    if (
        !flag &&
            (isFALSE(by) || # weights only make sense when we are marginalizing
                !isFALSE(wts) ||
                !is.null(byfun) ||
                !isFALSE(cross) ||
                isFALSE(getOption("marginaleffects_dedup", default = TRUE)))
    ) {
        return(newdata)
    }

    vclass <- attr(newdata, "marginaleffects_variable_class")

    # copy to allow mod by reference later without overwriting newdata
    out <- data.table(newdata)

    dv <- mfx@variable_names_response
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

    out[["rowid_dedup"]] <- seq_len(nrow(out))
    attr(out, "marginaleffects_variable_class") <- vclass

    return(out)
}


#' Add processed newdata to mfx object
#'
#' Orchestrates the complete newdata processing pipeline by calling sanitize_newdata_call,
#' sanitize_newdata, and dedup_newdata in sequence, then stores the result in mfx@newdata.
#'
#' @param mfx marginaleffects_internal S4 object
#' @param scall Quoted expression for newdata (from rlang::enquo)
#' @param newdata Raw newdata input
#' @param by Grouping variables
#' @param wts Weights specification
#' @param cross Cross-contrast flag (for comparisons)
#' @param comparison Comparison type (for comparisons)
#' @param byfun Function for aggregation (for predictions)
#' @return Updated mfx object with processed newdata in @newdata slot and updated wts in @wts slot
#' @keywords internal
add_newdata <- function(
    mfx,
    scall,
    newdata = NULL,
    by = FALSE,
    wts = FALSE,
    cross = NULL,
    comparison = NULL,
    byfun = NULL) {
    # For mice objects, defer all processing to process_imputation()
    if (inherits(mfx@model, c("mira", "amest"))) {
        # Store the raw newdata (could be a call) for later processing
        mfx@newdata <- scall
        return(mfx)
    }

    # Step 1: Handle quoted calls to datagrid, subset, etc.
    newdata <- sanitize_newdata_call(scall, newdata, mfx = mfx, by = by)

    # Step 2: Core newdata sanitization
    newdata <- sanitize_newdata(
        mfx = mfx,
        newdata = newdata,
        by = by,
        wts = wts
    )

    # Step 4: Extract numeric weights from newdata and store in @wts slot
    if ("marginaleffects_wts_internal" %in% colnames(newdata)) {
        mfx@wts <- newdata[["marginaleffects_wts_internal"]]
    } else {
        mfx@wts <- NULL
    }

    # Store processed newdata in the mfx object
    mfx@newdata <- newdata

    # if `modeldata` is unavailable, we default to `newdata`
    flag1 <- isTRUE(checkmate::check_data_frame(mfx@modeldata, min.rows = 1))
    flag2 <- isTRUE(checkmate::check_data_frame(newdata, min.rows = 1))
    if (!flag1 && flag2) {
        newdata <- set_variable_class(newdata, model = mfx@model)
        mfx@modeldata <- newdata
    }

    # Return the updated mfx object
    return(mfx)
}
