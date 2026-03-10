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


sanitize_newdata <- function(mfx, newdata, by, wts) {
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

    # Input validation (skip for mice objects)
    checkmate::assert(
        checkmate::check_data_frame(newdata, null.ok = TRUE),
        checkmate::check_choice(
            newdata,
            choices = c("mean", "median", "tukey", "grid", "balanced")
        ),
        combine = "or"
    )

    # Process 'by' argument for datagrid calls
    if (isTRUE(checkmate::check_data_frame(by))) {
        by_vars <- setdiff(colnames(by), "by")
    } else if (isTRUE(checkmate::check_flag(by))) {
        by_vars <- NULL
    } else {
        by_vars <- by
    }

    # Build datagrid arguments
    args <- list(model = model)
    for (b in by_vars) {
        args[[b]] <- unique
    }

    # Process newdata input
    if (is.null(newdata)) {
        # NULL -> modeldata
        newdata <- modeldata
    } else if (identical(newdata, "mean")) {
        # string -> datagrid()
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "median")) {
        args[["FUN_numeric"]] <- args[["FUN_integer"]] <- args[["FUN_logical"]] <-
            function(x) stats::median(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "tukey")) {
        args[["FUN_numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "grid")) {
        args[["FUN_numeric"]] <- function(x) stats::fivenum(x, na.rm = TRUE)
        args[["FUN_factor"]] <- args[["FUN_character"]] <- args[["FUN_logical"]] <- unique
        newdata <- do.call("datagrid", args)
    } else if (identical(newdata, "balanced")) {
        # grid with all unique values of categorical variables, and numerics at their means
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

    # Validate that we have a data.frame
    if (!inherits(newdata, "data.frame")) {
        msg <- "Unable to extract the data from model of class `%s`. This can happen in a variety of cases, such as when a `marginaleffects` package function is called from inside a user-defined function, or using an `*apply()`-style operation on a list. Please supply a data frame explicitly via the `newdata` argument."
        msg <- sprintf(msg, class(model)[1])
        stop_sprintf(msg)
    }

    # Process matrix columns
    # Issue #1327: matrix columns with single column breaks rbindlist(). See `scale()`
    newdata <- unpack_matrix_1col(newdata)

    # Issue #363: unpacking matrix columns works with {mgcv} but breaks {mclogit}
    if (inherits(model, "gam")) {
        newdata <- unpack_matrix_cols(newdata)
    }

    # Add rowid column for tracking
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    # Add weights column if needed
    newdata <- add_wts_column(wts, newdata, model)

    # Convert to data.table
    # setDT() raises warnings and breaks mergin with mlogit index columns
    # but as.data.table() breaks a bunch of other models, for reasons I can't
    # quite figure out
    if (inherits(model, "mlogit")) {
        out <- as.data.table(newdata)
    } else {
        data.table::setDT(newdata)
    }

    safe <- isTRUE(getOption("marginaleffects_safe", default = TRUE))
    nc <- ncol(newdata)
    if (safe && isTRUE(nc > 100)) {
        warn_sprintf("The `newdata` data frame has %s columns. This can slow down computation and increase memory costs. Please supply a data frame with fewer columns to the `newdata` argument, or set `options('marginaleffects_safe'=FALSE)` to silence this warning.", nc)
    }

    return(newdata)
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
            "The `wts` argument must be a numeric vector of length %s, or a string which matches one of the `colnames()` in the data frame that you supplied to the `newdata`, or in the `marginaleffects` objects.",
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

    # Step 2: Streamlined newdata processing (combines sanitize_newdata + build_newdata)
    newdata <- sanitize_newdata(
        mfx = mfx,
        newdata = newdata,
        by = by,
        wts = wts
    )

    # Step 3: Extract numeric weights from newdata and store in @wts slot
    if ("marginaleffects_wts_internal" %in% colnames(newdata)) {
        mfx@wts <- newdata[["marginaleffects_wts_internal"]]
    } else {
        mfx@wts <- NULL
    }

    # Store processed newdata in the mfx object
    mfx@newdata <- newdata

    # Merge variable class info from newdata to handle workflows that drop raw predictors
    newdata_vclass <- detect_variable_class(newdata, model = mfx@model)
    if (length(newdata_vclass) > 0) {
        if (length(mfx@variable_class) == 0) {
            mfx@variable_class <- newdata_vclass
        } else {
            missing <- setdiff(names(newdata_vclass), names(mfx@variable_class))
            if (length(missing) > 0) {
                mfx@variable_class[missing] <- newdata_vclass[missing]
            }
        }
    }

    # Extract and store variable_names_datagrid attribute from newdata
    datagrid_vars <- attr(newdata, "variable_names_datagrid")
    if (!is.null(datagrid_vars)) {
        mfx@variable_names_datagrid <- datagrid_vars
    }

    # if `modeldata` is unavailable, we default to `newdata`
    flag1 <- isTRUE(checkmate::check_data_frame(mfx@modeldata, min.rows = 1))
    flag2 <- isTRUE(checkmate::check_data_frame(newdata, min.rows = 1))
    if (!flag1 && flag2) {
        mfx@modeldata <- newdata
        mfx@modeldata_available <- FALSE
        mfx@variable_class <- detect_variable_class(newdata, model = mfx@model)
    }

    # Return the updated mfx object
    return(mfx)
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
