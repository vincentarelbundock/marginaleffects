#' Marginal Means
#'
#' Compute estimated marginal means for specified factors. Marginal means are
#' adjusted predictions, averaged across a grid of categorical predictors,
#' holding other numeric predictors at their means. For more information, see
#' the Details and Examples sections below, and in the vignettes on the
#' `marginaleffects` website: <https://vincentarelbundock.github.io/marginaleffects/>
#' * [Getting Started](https://vincentarelbundock.github.io/marginaleffects/#getting-started)
#' * [Marginal Means Vignette](https://vincentarelbundock.github.io/marginaleffects/articles/mfx04_marginalmeans.html)
#'
#' @inheritParams marginaleffects
#' @param variables character vector Categorical predictors over which to
#' compute marginal means. `NULL` calculates marginal means for all logical,
#' character, or factor variables in the dataset used to fit `model`. Set
#' `interaction=TRUE` to compute marginal means at combinations of the
#' predictors specified in the `variables` argument.
#' @param variables_grid character vector Categorical predictors used to construct the
#'   prediction grid over which adjusted predictions are averaged (character
#'   vector). `NULL` creates a grid with all combinations of all categorical
#'   predictors. This grid can be very large when there are many variables and
#'   many response levels, so it is advisable to select a limited number of
#'   variables in the `variables` and `variables_grid` arguments.
#' @param interaction TRUE, FALSE, or NULL
#' * `FALSE`: Marginal means are computed for each predictor individually.
#' * `TRUE`: Marginal means are computed for each combination of predictors specified in the `variables` argument.
#' * `NULL` (default): Behaves like `TRUE` when the `variables` argument is specified and the model formula includes interactions. Behaves like `FALSE` otherwise.
#' @inheritParams comparisons
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
#'
#' @template model_specific_arguments
#'
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
                          vcov = TRUE,
                          conf_level = 0.95,
                          type = "response",
                          transform_post = NULL,
                          interaction = NULL,
                          ...) {

    newdata <- insight::get_data(model)

    checkmate::assert_function(transform_post, null.ok = TRUE)
    interaction <- sanitize_interaction(interaction, variables, model)
    conf_level <- sanitize_conf_level(conf_level, ...)

    # fancy vcov processing to allow strings like "HC3"
    vcov <- get_vcov(model, vcov = vcov)

    # sanity
    sanity_dots(model = model, ...)
    if (inherits(model, "brmsfit")) {
        stop("`brmsfit` objects are yet not supported by the `marginalmeans` function. Follow this link to track progress: https://github.com/vincentarelbundock/marginaleffects/issues/137", call. = FALSE)
    }
    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)
    if (!is.null(variables)) {
        bad <- setdiff(variables, colnames(newdata))
        if (length(bad) > 0) {
            stop(sprintf("Elements of the `variables` argument were not found as column names in the data used to fit the model: %s", paste(bad, collapse = ", ")), call. = FALSE)
        }
    }

    checkmate::assert_character(variables_grid, min.len = 1, null.ok = TRUE)
    if (!is.null(variables_grid)) {
        bad <- setdiff(variables_grid, colnames(newdata))
        if (length(bad) > 0) {
            stop(sprintf("Elements of the `variables_grid` argument were not found as column names in the data used to fit the model: %s", paste(bad, collapse = ", ")), call. = FALSE)
        }
    }

    # categorical variables, excluding response
    column_labels <- colnames(newdata)
    term_labels <- insight::find_terms(model, flatten = TRUE)
    variables_categorical <- find_categorical(newdata = newdata, model = model)
    variables_categorical <- setdiff(variables_categorical, insight::find_response(model, flatten = TRUE))
    variables_categorical <- intersect(variables_categorical, term_labels)
    if (length(variables_categorical) == 0) {
        stop("No logical, factor, or character variable was found in the dataset used to fit the `model` object. This error is often raised when users convert variables to factor in the model formula (e.g., `lm(y ~ factor(x)`). If this is the case, you may consider converting variables in the dataset before fitting the model.", call. = FALSE)
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
                            variables = variables,
                            interaction = interaction,
                            ...)

    # we want consistent output, regardless of whether `data.table` is installed/used or not
    out <- as.data.frame(mm)

    # standard errors via delta method
    J <- NULL
    if (!isFALSE(vcov)) {
        se <- standard_errors_delta(model,
                                    vcov = vcov,
                                    type = type,
                                    FUN = standard_errors_delta_marginalmeans,
                                    index = NULL,
                                    variables = variables,
                                    newdata = newgrid,
                                    interaction = interaction)
        # get rid of attributes in column
        out[["std.error"]] <- as.numeric(se)
        J <- attr(se, "J")
    }

    lin <- tryCatch(insight::model_info(model)$is_linear, error = function(e) FALSE)
    if (isTRUE(type == "link") || isTRUE(lin)) {
        if ("std.error" %in% colnames(out) && !"conf.low" %in% colnames(out)) {
            critical_z <- abs(stats::qnorm((1 - conf_level) / 2))
            out[["conf.low"]] <- out[["marginalmean"]] - critical_z * out[["std.error"]]
            out[["conf.high"]] <- out[["marginalmean"]] + critical_z * out[["std.error"]]
        }
    }

    if (!is.null(transform_post)) {
        out <- backtransform(out, transform_post)
    }

    # column order
    cols <- c("type", "group", "term", "value", variables, "marginalmean",
              "std.error", "conf.low", "conf.high", sort(colnames(out)))
    cols <- unique(cols)
    cols <- intersect(cols, colnames(out))
    out <- out[, cols, drop = FALSE]

    # attributes
    class(out) <- c("marginalmeans", class(out))
    attr(out, "model") <- model
    attr(out, "J") <- J
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
                              interaction,
                              ...) {

    # predictions for each cell of all categorical data, but not the response
    pred <- predictions(
        model = model,
        newdata = newdata,
        type = type,
        vcov = FALSE,
        ...)

    # marginal means
    if (!isTRUE(interaction)) {
        mm <- list()
        for (v in variables) {
            idx <- intersect(colnames(pred), c("term", "group", v))
            tmp <- data.table(pred)[, .(marginalmean = mean(predicted, na.rm = TRUE)), by = idx]
            tmp[, "term" := v]
            setnames(tmp, old = v, new = "value")
            mm[[v]] <- tmp
        }

        # try to preserve term-value class, but convert to character if needed to bind
        classes <- sapply(mm, function(x) class(x$value)[1])
        if (length(unique(classes)) > 1) {
            for (i in seq_along(mm)) {
                mm[[i]]$value <- as.character(mm[[i]]$value)
            }
        }
        out <- rbindlist(mm)
        setorder(out, "term", "value")
    } else {
        idx <- intersect(colnames(pred), c("term", "group", variables))
        out <- data.table(pred)[, .(marginalmean = mean(predicted, na.rm = TRUE)), by = idx]
    }

    return(out)
}


