#' Generate a data grid of "typical," "counterfactual," or user-specified values for use in the `newdata` argument of the `marginaleffects` or `predictions` functions.
#'
#' @param ... named arguments with vectors of values for user-specified
#' variables. The output will include all combinations of these variables (see
#' Examples below.)
#' @param model Model object
#' @param newdata data.frame (one and only one of the `model` and `newdata` arguments
#' @param grid.type character
#'   * "typical": variables whose values are not explicitly specified by the user in `...` are set to the output of the functions supplied to the `FUN.type` arguments.
#'   * "counterfactual": the entire dataset is duplicated for each combination of the variable values specified in `...`.
#' @param FUN.character the function to be applied to character variables.
#' @param FUN.factor the function to be applied to factor variables.
#' @param FUN.logical the function to be applied to factor variables.
#' @param FUN.numeric the function to be applied to numeric variables.
#' @param FUN.other the function to be applied to other variable types.
#' @details
#' If `datagrid` is used in a `marginaleffects` or `predictions` call as the
#' `newdata` argument, users do not need to specify the `model` or `newdata`
#' argument. The data is extracted automatically from the model.
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
#' dg <- datagrid(newdata = mtcars, hp = c(100, 110), grid.type = "counterfactual")
#' nrow(dg)
#'
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp, mtcars)
#' dg <- datagrid(model = mod, hp = c(100, 110), grid.type = "counterfactual")
#' nrow(dg)
datagrid <- function(
    ...,
    model = NULL,
    newdata = NULL,
    grid.type = "typical",
    FUN.character = Mode,
    # need to be explicit for numeric variables transfered to factor in model formula
    FUN.factor = Mode,
    FUN.logical = Mode,
    FUN.numeric = function(x) mean(x, na.rm = TRUE),
    FUN.other = function(x) mean(x, na.rm = TRUE)) {

    checkmate::assert_choice(grid.type, choices = c("typical", "counterfactual"))
    checkmate::assert_function(FUN.character)
    checkmate::assert_function(FUN.factor)
    checkmate::assert_function(FUN.logical)
    checkmate::assert_function(FUN.numeric)
    checkmate::assert_function(FUN.other)

    if (grid.type == "typical") {
        out <- typical(...,
                       model = model,
                       newdata = newdata,
                       FUN.character = FUN.character,
                       FUN.factor = FUN.factor,
                       FUN.logical = FUN.logical,
                       FUN.numeric = FUN.numeric,
                       FUN.other = FUN.other)
    } else {
        out <- counterfactual(...,
                              model = model,
                              newdata = newdata)
    }

    return(out)
}


#' Superseded by datagrid(..., grid.type = "counterfactual")
#'
#' @inheritParams datagrid
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
    at <- expand.grid(at, stringsAsFactors = FALSE)

    rowid <- data.frame(rowid_original = seq_len(nrow(dat)))
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
#' @export
typical <- function(
    ...,
    model = NULL,
    newdata = NULL,
    FUN.character = Mode,
    # need to be explicit for numeric variables transfered to factor in model formula
    FUN.factor = Mode,
    FUN.logical = Mode,
    FUN.numeric = function(x) mean(x, na.rm = TRUE),
    FUN.other = function(x) mean(x, na.rm = TRUE)) {

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
            # factor before character because of attribute check
            if (is.factor(dat_automatic[[n]]) || isTRUE(attr(dat[[n]], "factor"))) {
                out[[n]] <- FUN.factor(dat_automatic[[n]])
            } else if (is.logical(dat_automatic[[n]])) {
                out[[n]] <- FUN.logical(dat_automatic[[n]])
            } else if (is.character(dat_automatic[[n]])) {
                out[[n]] <- FUN.character(dat_automatic[[n]])
            } else if (is.numeric(dat_automatic[[n]])) {
                out[[n]] <- FUN.numeric(dat_automatic[[n]])
            } else {
                out[[n]] <- FUN.other(dat_automatic[[n]])
            }
        }
    } else {
        out <- list()
    }

    if (!is.null(at)) {
        for (n in names(at)) {
            out[n] <- at[n]
        }
    }

    # warn on very large prediction grid
    num <- as.numeric(sapply(out, length)) # avoid integer overflow
    num <- Reduce(f = "*", num)
    if (num > 1e9) {
        stop("You are trying to create a prediction grid with more than 1 billion rows, which is likely to exceed the memory and computational power available on your local machine. Presumably this is because you are considering many variables with many levels. All of the functions in the `marginaleffects` package include arguments to specify a restricted list of variables over which to create a prediction grid.")
    }

    out <- lapply(out, unique)
    out <- expand.grid(out, stringsAsFactors = FALSE)

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

    if (!is.null(model) & !is.null(newdata)) {
        stop("One of the `model` or `newdata` arguments must be `NULL`.")
    }

    if (is.null(model) & is.null(newdata)) {
        stop("The `model` and `newdata` arguments should not both be `NULL`.")
    }

    # data: all variables
    if (!is.null(newdata)) {
        variables <- colnames(newdata)
    # model: variables = NULL because otherwise `sanity_variables` excludes others
    } else {
        variables <- NULL
    }

    variables_list <- sanity_variables(model = model,
                                       newdata = newdata,
                                       variables = variables)
    variables_all <- unique(unlist(variables_list))
    variables_manual <- names(at)
    variables_automatic <- setdiff(variables_all, variables_manual)

    # fill in missing data after sanity checks
    if (is.null(newdata)) {
        newdata <- insight::get_data(model)
    }

    # check `at` names
    variables_missing <- setdiff(names(at), variables_all)
    if (length(variables_missing) > 0) {
        warning(sprintf("Some of the variable names are missing from the model data: %s",
                        paste(variables_missing, collapse = ", ")))
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
                stop(msg)
            } else {
                at[[n]] <- factor(at[[n]], levels = levs)
            }
        }
    }

    # warn if cluster variables are numeric. users probably do not want to take
    # their means, because this makes prediction impossible in many models
    # (e.g., `fixest::feols(mpg ~ hp | cyl)`)
    variables_cluster <- unlist(c(variables_list$cluster, variables_list$random))
    variables_cluster <- intersect(variables_automatic, variables_cluster)
    if (length(variables_cluster) > 0) {
        idx <- sapply(variables_cluster, function(x) is.numeric(newdata[[x]]))
        if (any(idx)) {
            idx <- paste(sprintf('"%s"', variables_cluster[idx]), collapse = ", ")
            warning(sprintf("Unless otherwise instructed, this function sets numeric variables to their mean. This is probably inappropriate in the case of cluster variables or group identifiers like %s. A safer strategy is to convert cluster variables to factors before fitting the model.", idx))
        }
    }

    out <- list("newdata" = newdata,
                "at" = at,
                "variables_all" = variables_all,
                "variables_manual" = variables_manual,
                "variables_automatic" = variables_automatic)
    return(out)
}
