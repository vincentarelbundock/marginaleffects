#' Marginal Means
#'
#' Compute estimated marginal means for specified factors.
#'
#' @inheritParams marginaleffects
#' @param variables Categorical predictors over which to compute marginal means
#'   (character vector). `NULL` calculates marginal means for all logical,
#'   character, or factor variables in the dataset used to fit `model`.
#' @param variables_grid Categorical predictors used to construct the
#'   prediction grid over which adjusted predictions are averaged (character
#'   vector). `NULL` creates a grid with all combinations of all categorical
#'   predictors. This grid can be very large when there are many variables and
#'   many response levels, so it is advisable to select a limited number of
#'   variables in the `variables` and `variables_grid` arguments.
#' @param type Type(s) of prediction (string or character vector). This can
#'   differ based on the model type, but will typically be a string such as:
#'   "response", "link", "probs", or "zero".
#' @details
#'   This function begins by calling the `predictions` function to obtain a
#'   grid of predictors, and adjusted predictions for each cell. The grid
#'   includes all combinations of the categorical variables listed in the
#'   `variables` and `variables_grid` arguments, or all combinations of the
#'   categorical variables used to fit the model if `variables_grid` is `NULL`.
#'   In the prediction grid, numeric variables are held at their means.
#'
#'   After constructing the grid and filling the grid with adjusted predictions,
#'   `marginalmeans` computes marginal means for the variables listed in the
#'   `variables` argument, by average across all categories in the grid.
#'
#'   `marginalmeans` can only compute standard errors for linear models, or for
#'   predictions on the link scale, that is, with the `type` argument set to
#'   "link".
#'
#'   The `marginaleffects` website compares the output of this function to the
#'   popular `emmeans` package, which provides similar but more advanced
#'   functionality: https://vincentarelbundock.github.io/marginaleffects/
#' @return Data frame of marginal means with one row per variable-value
#' combination.
#' @export
#' @examples
#' library(marginaleffects)
#'
#' # Convert numeric variables to categorical before fitting the model
#' dat <- mtcars
#' dat$cyl <- as.factor(dat$cyl)
#' dat$am <- as.logical(dat$am)
#' mod <- lm(mpg ~ hp + cyl + am, data = dat)
#'
#' # Compute and summarize marginal means
#' mm <- marginalmeans(mod)
#' summary(mm)
marginalmeans <- function(model,
                          variables = NULL,
                          variables_grid = NULL,
                          vcov = NULL,
                          type = "response") {

    newdata <- insight::get_data(model)

    if (is.null(vcov)) {
        vcov <- get_vcov(model)
    }

    # sanity
    if (inherits(model, "brmsfit")) {
        stop("`brmsfit` objects are yet not supported by the `marginalmeans` function. Follow this link to track progress: https://github.com/vincentarelbundock/marginaleffects/issues/137")
    }
    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)
    if (!is.null(variables)) {
        bad <- setdiff(variables, colnames(newdata))
        if (length(bad) > 0) {
            stop(sprintf("Elements of the `variables` argument were not found as column names in the data used to fit the model: %s", paste(bad, collapse = ", ")))
        }
    }

    checkmate::assert_character(variables_grid, min.len = 1, null.ok = TRUE)
    if (!is.null(variables_grid)) {
        bad <- setdiff(variables_grid, colnames(newdata))
        if (length(bad) > 0) {
            stop(sprintf("Elements of the `variables_grid` argument were not found as column names in the data used to fit the model: %s", paste(bad, collapse = ", ")))
        }
    }

    # interactions are not supported
    # stats::terms does not work with brmsfit
    interactions <- try(any(grepl(":", attr(stats::terms(model), "term.labels"))), silent = TRUE)
    if (isTRUE(interactions)) {
        warning("The `marginalmeans` function does not support models with interactions. The reported standard errors may be misleading.")
    }

    # categorical variables, excluding response
    column_labels <- colnames(newdata)
    term_labels <- insight::find_terms(model, flatten = TRUE)
    variables_categorical <- find_categorical(newdata)
    variables_categorical <- setdiff(variables_categorical, insight::find_response(model, flatten = TRUE))
    variables_categorical <- intersect(variables_categorical, term_labels)
    if (length(variables_categorical) == 0) {
        stop("No logical, factor, or character variable was found in the dataset used to fit the `model` object. This error is often raised when users convert variables to factor in the model formula (e.g., `lm(y ~ factor(x)`). If this is the case, you may consider converting variables in the dataset before fitting the model.")
    }

    # subset variables and grid
    if (is.null(variables)) {
        variables <- variables_categorical
    } else {
        variables <- intersect(variables, variables_categorical)
    }

    if (is.null(variables_grid)) {
        variables_grid <- variables_categorical
    } else {
        variables_grid <- intersect(variables_grid, variables_categorical)
    }
    variables_grid <- unique(c(variables, variables_grid))

    args <- lapply(variables_grid, function(x) unique(newdata[[x]]))
    args <- stats::setNames(args, variables_grid)
    args[["newdata"]] <- newdata
    newgrid <- do.call("datagrid", args)

    mm <- get_marginalmeans(model = model,
                            newdata = newgrid,
                            type = type,
                            variables = variables)

    # we want consistent output, regardless of whether `data.table` is installed/used or not
    out <- as.data.frame(mm)

    # standard errors via delta method
    se <- standard_errors_delta(model,
                                vcov = vcov,
                                type = type,
                                FUN = standard_errors_delta_marginalmeans,
                                index = NULL,
                                variables = variables,
                                newdata = newgrid)

    # get rid of attributes in column
    out[["std.error"]] <- as.numeric(se)

    # column order
    cols <- c("type", "group", "term", "value", "marginalmean", "std.error", sort(colnames(out)))
    cols <- unique(cols)
    cols <- intersect(cols, colnames(out))
    out <- out[, cols, drop = FALSE]

    # attributes
    class(out) <- c("marginalmeans", class(out))
    attr(out, "J") <- attr(se, "J")
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables

    return(out)
}


#' Workhorse function for `marginalmeans`
#'
#' Needs to be separate because we also need it in `delta_method`
#' @inheritParams marginalmeans
#' @inheritParams predictions
#' @param ... absorb useless arguments from other get_* workhorse functions
#' @noRd
get_marginalmeans <- function(model,
                              newdata,
                              type,
                              variables,
                              ...) {

    # predictions for each cell of all categorical data, but not the response
    pred <- predictions(
        model = model,
        newdata = newdata,
        type = type,
        conf.level = NULL,
        ...)

    # marginal means
    mm <- list()
    for (v in variables) {
        lhs <- "predicted ~"
        rhs <- intersect(colnames(pred), c("group", v))
        rhs <- paste(rhs, collapse = " + ")
        f <- stats::as.formula(paste(lhs, rhs))
        tmp <- stats::aggregate(f, data = pred, FUN = mean)
        cols <- intersect(c("term", "group", v, "predicted"), colnames(tmp))
        tmp <- tmp[, cols]
        tmp[["term"]] <- v
        colnames(tmp)[colnames(tmp) == v] <- "value"
        colnames(tmp)[colnames(tmp) == "predicted"] <- "marginalmean"
        mm[[v]] <- tmp
    }

    # try to preserve term-value class, but convert to character if needed to bind
    classes <- sapply(mm, function(x) class(x$value)[1])
    if (length(unique(classes)) > 1) {
        for (i in seq_along(mm)) {
            mm[[i]]$value <- as.character(mm[[i]]$value)
        }
    }
    out <- bind_rows(mm)

    return(out)
}


