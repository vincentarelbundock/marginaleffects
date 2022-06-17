#' Generate a data grid of "typical," "counterfactual," or user-specified values for use in the `newdata` argument of the `marginaleffects` or `predictions` functions.
#'
#' @param ... named arguments with vectors of values for user-specified
#' variables. The output will include all combinations of these variables (see
#' Examples below.)
#' @param model Model object
#' @param newdata data.frame (one and only one of the `model` and `newdata` arguments
#' @param grid_type character
#'   * "typical": variables whose values are not explicitly specified by the user in `...` are set to their mean or mode, or to the output of the functions supplied to `FUN_type` arguments.
#'   * "counterfactual": the entire dataset is duplicated for each combination of the variable values specified in `...`. Variables not explicitly supplied to `datagrid()` are set to their observed values in the original dataset.
#' @param FUN_character the function to be applied to character variables.
#' @param FUN_factor the function to be applied to factor variables.
#' @param FUN_logical the function to be applied to factor variables.
#' @param FUN_numeric the function to be applied to numeric variables.
#' @param FUN_other the function to be applied to other variable types.
#' @details
#' If `datagrid` is used in a `marginaleffects` or `predictions` call as the
#' `newdata` argument, the model is automatically inserted in the function
#' call, and users do not need to specify either the `model` or `newdata`
#' arguments. Note that only the variables used to fit the models will be
#' attached to the results. If a user wants to attach other variables as well
#' (e.g., weights or grouping variables), they can supply a data.frame
#' explicitly to the `newdata` argument inside `datagrid()`.
#'
#' If users supply a model, the data used to fit that model is retrieved using
#' the `insight::get_data` function.
#' @return
#' A `data.frame` in which each row corresponds to one combination of the named
#' predictors supplied by the user via the `...` dots. Variables which are not
#' explicitly defined are held at their mean or mode.
#' @export
#' @examples
#' # The output only has 2 rows, and all the variables except `hp` are at their
#' # mean or mode.
#' datagrid(newdata = mtcars, hp = c(100, 110))
#'
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp, mtcars)
#' datagrid(model = mod, hp = c(100, 110))
#'
#' # Use in `marginaleffects` to compute "Typical Marginal Effects". When used
#' # in `marginaleffects()` or `predictions()` we do not need to specify the
#' #`model` or `newdata` arguments.
#' marginaleffects(mod, newdata = datagrid(hp = c(100, 110)))
#'
#' # The full dataset is duplicated with each observation given counterfactual
#' # values of 100 and 110 for the `hp` variable. The original `mtcars` includes
#' # 32 rows, so the resulting dataset includes 64 rows.
#' dg <- datagrid(newdata = mtcars, hp = c(100, 110), grid_type = "counterfactual")
#' nrow(dg)
#'
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp, mtcars)
#' dg <- datagrid(model = mod, hp = c(100, 110), grid_type = "counterfactual")
#' nrow(dg)
datagrid <- function(
    ...,
    model = NULL,
    newdata = NULL,
    grid_type = "typical",
    FUN_character = Mode,
    # need to be explicit for numeric variables transfered to factor in model formula
    FUN_factor = Mode,
    FUN_logical = Mode,
    FUN_numeric = function(x) mean(x, na.rm = TRUE),
    FUN_other = function(x) mean(x, na.rm = TRUE)) {

    # backward compatibility
    dots <- list(...)
    FUN_character <- arg_name_change(FUN_character, "FUN.character", dots)
    FUN_factor <- arg_name_change(FUN_factor, "FUN.factor", dots)
    FUN_logical <- arg_name_change(FUN_logical, "FUN.logical", dots)
    FUN_numeric <- arg_name_change(FUN_numeric, "FUN.numeric", dots)
    FUN_other <- arg_name_change(FUN_other, "FUN.other", dots)
    grid_type <- arg_name_change(grid_type, "grid.type", dots)
    idx <- !grepl("^FUN\\.|grid\\.type", names(dots))
    dots <- dots[idx]

    # sanity
    checkmate::assert_choice(grid_type, choices = c("typical", "counterfactual"))
    checkmate::assert_function(FUN_character)
    checkmate::assert_function(FUN_factor)
    checkmate::assert_function(FUN_logical)
    checkmate::assert_function(FUN_numeric)
    checkmate::assert_function(FUN_other)

    if (grid_type == "typical") {
        args <- list( # cleaned for backward compatibility
            model = model,
            newdata = newdata,
            FUN_character = FUN_character,
            FUN_factor = FUN_factor,
            FUN_logical = FUN_logical,
            FUN_numeric = FUN_numeric,
            FUN_other = FUN_other)
        args <- c(dots, args)
        out <- do.call("typical", args)
    } else {
        args <- list(
            model = model,
            newdata = newdata)
        args <- c(dots, args)
        out <- do.call("counterfactual", args)
    }
    return(out)
}


#' Superseded by datagrid(..., grid_type = "counterfactual")
#'
#' @inheritParams datagrid
#' @keywords internal
#' @export
counterfactual <- function(..., model = NULL, newdata = NULL) {

    tmp <- prep_datagrid(..., model = model, newdata = newdata)
    at <- tmp$at
    dat <- tmp$newdata
    variables_all <- tmp$all
    variables_manual <- tmp$variables_manual
    variables_automatic <- tmp$variables_automatic

    # `at` -> `data.frame`
    at <- lapply(at, unique)

    fun <- data.table::CJ
    args <- c(at, list(sorted = FALSE))
    at <- do.call("fun", args)

    rowid <- data.frame(rowid_counterfactual = seq_len(nrow(dat)))
    if (length(variables_automatic) > 0) {
        dat_automatic <- dat[, variables_automatic, drop = FALSE]
        dat_automatic <- cbind(rowid, dat_automatic)
        out <- merge(dat_automatic, at, all = TRUE)
    }  else {
        out <- merge(rowid, at, all = TRUE)
    }

    return(out)
}


#' Superseded by datagrid(...)
#'
#' @inheritParams datagrid
#' @keywords internal
#' @export
typical <- function(
    ...,
    model = NULL,
    newdata = NULL,
    FUN_character = Mode,
    # need to be explicit for numeric variables transfered to factor in model formula
    FUN_factor = Mode,
    FUN_logical = Mode,
    FUN_numeric = function(x) mean(x, na.rm = TRUE),
    FUN_other = function(x) mean(x, na.rm = TRUE)) {

    tmp <- prep_datagrid(..., model = model, newdata = newdata)
    at <- tmp$at
    dat <- tmp$newdata
    variables_all <- tmp$all
    variables_manual <- tmp$variables_manual
    variables_automatic <- tmp$variables_automatic

    if (length(variables_automatic) > 0) {
        dat_automatic <- dat[, variables_automatic, drop = FALSE]
        dat_automatic <- stats::na.omit(dat_automatic)
        out <- list()
        # na.omit destroys attributes, and we need the "factor" attribute
        # created by insight::get_data
        for (n in names(dat_automatic)) {
            variable_class <- find_variable_class(n, newdata = dat_automatic, model = model)
            if (variable_class == "factor") out[[n]] <- FUN_factor(dat_automatic[[n]])
            if (variable_class == "logical") out[[n]] <- FUN_logical(dat_automatic[[n]])
            if (variable_class == "character") out[[n]] <- FUN_character(dat_automatic[[n]])
            if (variable_class == "numeric") out[[n]] <- FUN_numeric(dat_automatic[[n]])
            if (variable_class == "other") out[[n]] <- FUN_other(dat_automatic[[n]])
        }
    } else {
        out <- list()
    }

    if (!is.null(at)) {
        for (n in names(at)) {
            out[n] <- at[n]
        }
    }


    # unique before counting
    out <- lapply(out, unique)

    # warn on very large prediction grid
    num <- as.numeric(sapply(out, length)) # avoid integer overflow
    num <- Reduce(f = "*", num)
    if (num > 1e9) {
        stop("You are trying to create a prediction grid with more than 1 billion rows, which is likely to exceed the memory and computational power available on your local machine. Presumably this is because you are considering many variables with many levels. All of the functions in the `marginaleffects` package include arguments to specify a restricted list of variables over which to create a prediction grid.", call. = FALSE)
    }

    fun <- data.table::CJ
    args <- c(out, list(sorted = FALSE))
    out <- do.call("fun", args)

    # na.omit destroys attributes, and we need the "factor" attribute
    # created by insight::get_data
    for (n in names(out)) {
        attr(out[[n]], "factor") <- attr(dat[[n]], "factor")
    }

    return(out)
}


prep_datagrid <- function(..., model = NULL, newdata = NULL) {

    checkmate::assert_data_frame(newdata, null.ok = TRUE)

    at <- list(...)

    # if (!is.null(model) & !is.null(newdata)) {
    #     msg <- "One of the `model` or `newdata` arguments must be `NULL`."
    #     stop(msg, call. = FALSE)
    # }

    if (is.null(model) & is.null(newdata)) {
        msg <- "When calling `datagrid()` *inside* the `marginaleffects()` or `comparisons()` functions, the `model` and `newdata` arguments can both be omitted. However, when calling `datagrid()` on its own, users must specify either the `model` or the `newdata` argument (but not both)."
        stop(msg, call. = FALSE)
    }

    # data: all variables
    if (!is.null(newdata)) {
        variables <- colnames(newdata)
    # model: variables = NULL because otherwise `sanitize_variables` excludes others
    } else {
        variables <- NULL
    }

    variables_list <- suppressWarnings(
        sanitize_variables(model = model,
            newdata = newdata,
            variables = variables))
    variables_all <- unique(unlist(variables_list))
    variables_manual <- names(at)
    variables_automatic <- setdiff(variables_all, variables_manual)

    # fill in missing data after sanity checks
    if (is.null(newdata)) {
        newdata <- suppressWarnings(insight::get_data(model))
    }

    if (any(sapply(newdata, function(x) "matrix" %in% class(x)))) {
        msg <- format_msg(
        "The `datagrid()`, `marginalmeans()`, `plot_cap()`, and `plot_cme()` functions
        do not support datasets with matrix columns. You can construct your own
        prediction dataset and supply it explicitly to the `newdata` argument of the
        `predictions()`, `marginaleffects()`, or `comparisons()` functions instead.")
        stop(msg, call. = FALSE)
    }

    # check `at` names
    variables_missing <- setdiff(names(at), variables_all)
    if (length(variables_missing) > 0) {
        warning(sprintf("Some of the variable names are missing from the model data: %s",
                        paste(variables_missing, collapse = ", ")),
                call. = FALSE)
    }

    # check `at` elements and convert them to factor as needed
    for (n in names(at)) {
        if (is.factor(newdata[[n]]) || isTRUE(attributes(newdata[[n]])$factor)) {
            if (is.factor(newdata[[n]])) {
                levs <- levels(newdata[[n]])
            } else {
                levs <- as.character(sort(unique(newdata[[n]])))
            }
            at[[n]] <- as.character(at[[n]])
            if (!all(at[[n]] %in% c(levs, NA))) {
                msg <- sprintf('The "%s" element of the `at` list corresponds to a factor variable. The values entered in the `at` list must be one of the factor levels: "%s".', n, paste(levels(newdata[[n]]), collapse = '", "'))
                stop(msg, call. = FALSE)
            } else {
                at[[n]] <- factor(at[[n]], levels = levs)
            }
        }
    }

    # warn if cluster variables after the | in the random effects formula are
    # numeric. users probably do not want to take their means, because this
    # makes prediction impossible in many models (e.g., `fixest::feols(mpg ~ hp
    # | cyl)`)
    # insight::find
    variables_cluster <- unlist(c(variables_list$cluster, variables_list$random))
    variables_cluster <- intersect(variables_cluster, variables_automatic)
    if (length(variables_cluster) > 0) {
        cluster_ids <- insight::find_random(model, flatten = TRUE)

        # random effects component only cyl after pipe: hp ~ drat + (1 + mpg | cyl)
        if (length(cluster_ids) > 0) {
            idx <- sapply(cluster_ids, function(x) is.numeric(newdata[[x]]))
            cluster_ids <- cluster_ids[idx]

        # fixest et al. everything after the pipe: hp ~ drat | cyl + gear
        } else {
            idx <- sapply(variables_cluster, function(x) is.numeric(newdata[[x]]))
            cluster_ids <- variables_cluster[idx]
        }

        if (length(cluster_ids) > 0) {
            msg <- format_msg(
            "Some cluster or group identifiers are numeric. Unless otherwise
            instructed, `datagrid()` sets all numeric variables to their mean.
            This is probably inappropriate in the case of cluster or group
            identifiers. A safer strategy is to convert them to factors before
            fitting the model.")
            warning(msg, call. = FALSE)
        }
    }

    out <- list("newdata" = newdata,
                "at" = at,
                "variables_all" = variables_all,
                "variables_manual" = variables_manual,
                "variables_automatic" = variables_automatic)
    return(out)
}
