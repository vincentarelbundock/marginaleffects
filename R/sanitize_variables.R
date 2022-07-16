# return a list with LL
sanitize_variables <- function(model,
                               newdata,
                               variables,
                               contrast_numeric = 1,
                               contrast_factor = "reference") {

    # TODO: do we still need this?
    contrast_types <- NULL

    checkmate::assert(
        checkmate::check_character(variables, min.len = 1, null.ok = TRUE),
        checkmate::check_list(variables, names = "unique"),
        combine = "or")
    checkmate::assert_data_frame(newdata, min.row = 1, null.ok = TRUE)

    if (!is.null(model) && is.null(newdata)) {
        origindata <- hush(insight::get_data(model))
    } else {
        origindata <- newdata
    }

    if (is.null(newdata)) {
        newdata <- origindata
    }

    # rename to avoid overwriting in case we need info later
    predictors <- variables
    cluster <- instruments <- others <- correlation <- NULL

    # variables is NULL: put all variable names from model
    if (is.null(predictors)) {
        # mhurdle names the variables weirdly
        if (inherits(model, "mhurdle")) {
            predictors <- insight::find_predictors(model, flatten = TRUE)
        } else {
            predictors <- insight::find_variables(model)
            cluster <- c(
                predictors[["random"]],
                predictors[["cluster"]],
                predictors[["strata"]])
            others <- c(
                predictors[["instruments"]],
                predictors[["correlation"]])
            bad <- c("response", "weights", "random", "cluster", "instruments", "correlation", "strata")
            predictors <- predictors[!names(predictors) %in% bad]
            predictors <- unlist(predictors, recursive = TRUE, use.names = FALSE)
        }
    }

    # variables is character vector: convert to named list
    if (isTRUE(checkmate::check_character(predictors))) {
        tmp <- list()
        for (v in predictors) {
            if (isTRUE(find_variable_class(v, newdata, model) == "numeric")) {
                tmp[[v]] <- contrast_numeric
            } else {
                tmp[[v]] <- contrast_factor
            }
        }
        predictors <- tmp

    # variables is named list: check validity
    } else if (isTRUE(checkmate::check_list(predictors, names = "unique"))) {
        for (n in names(variables)) {
            if (n %in% colnames(newdata)) {
                if (isTRUE(find_variable_class(n, newdata, model) == "numeric")) {
                    flag <- sanity_contrast_numeric(variables[[n]])
                }
                if (isTRUE(find_variable_class(n, newdata, model) %in% c("factor", "character"))) {
                    flag <- sanity_contrast_factor(variables[[n]])
                }
            }
        }
        predictors <- variables
    }

    # sometimes `insight` returns interaction component as if it were a constituent variable
    idx <- !grepl(":", names(predictors))
    predictors <- predictors[idx]
    if (length(predictors) == 0) {
        stop("Please specify the `variables` argument explicitly.", call. = FALSE)
    }

    # reserved keywords
    reserved <- intersect(
        names(predictors),
        c("rowid", "group", "term", "estimate", "std.error", "statistic", "conf.low", "conf.high"))
    if (isTRUE(length(reserved) > 0)) {
        msg <- format_msg(sprintf(
        "The following variable names are forbidden to avoid conflicts with the column 
        names of the outputs produced by the `marginaleffects` package:

        %s

        Please rename your variables before fitting the model or specify the `variables` argument.",
        paste(reserved, collapse = ", ")))
        if (length(reserved) == length(predictors)) {
            stop(msg, call. = FALSE)
        } else {
            warning(msg, call. = FALSE)
        }
    }

    predictors <- predictors[!names(predictors) %in% reserved]

    # weights
    w <- tryCatch(insight::find_weights(model), error = function(e) NULL)
    w <- intersect(w, colnames(newdata))

    # check missing variables
    miss <- setdiff(names(predictors), colnames(newdata))
    if (length(miss) > 0) {
        stop(sprintf("Variables missing from `newdata` and/or the data extracted from the model objects: %s",
                     paste(miss, collapse = ", ")),
             call. = FALSE)
    }

    # cannot compute contrasts for matrix variables
    idx <- names(predictors) %in% attr(newdata, "matrix_columns")
    predictors <- predictors[!idx]
    if (any(idx)) others <- predictors[idx]

    # output
    out <- list(
        conditional = predictors,
        cluster = cluster,
        instruments = instruments,
        others = c(others, correlation),
        weights = w)

    # save character levels
    # Character variables are treated as factors by model-fitting functions,
    # but unlike factors, they do not not keep a record of all factor levels.
    # This poses problem when feeding `newdata` to `predict`, which often
    # breaks (via `model.matrix`) when the data does not include all possible
    # factor levels.
    levels_character <- list()
    for (v in names(variables)) {
        if (v %in% colnames(origindata) && is.character(origindata[[v]])) {
            levels_character[[v]] <- unique(origindata[[v]])
        }
    }
    attr(out, "levels_character") <- levels_character
    attr(out, "contrast_types") <- contrast_types

    return(out)
}



