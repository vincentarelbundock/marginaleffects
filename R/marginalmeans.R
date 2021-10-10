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
#' @param type Type(s) of prediction as string or vector This can
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
                          vcov = insight::get_varcov(model),
                          type = "response") {

    dat <- insight::get_data(model)

    # sanity
    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)
    if (!is.null(variables)) {
        bad <- setdiff(variables, colnames(dat))
        if (length(bad) > 0) {
            stop(sprintf("Elements of the `variables` argument were not found as column names in the data used to fit the model: %s", paste(bad, collapse = ", ")))
        }
    }

    checkmate::assert_character(variables_grid, min.len = 1, null.ok = TRUE)
    if (!is.null(variables_grid)) {
        bad <- setdiff(variables_grid, colnames(dat))
        if (length(bad) > 0) {
            stop(sprintf("Elements of the `variables_grid` argument were not found as column names in the data used to fit the model: %s", paste(bad, collapse = ", ")))
        }
    }

    # categorical variables, excluding response
    column_labels <- colnames(dat)
    term_labels <- insight::find_terms(model, flatten = TRUE)
    variables_categorical <- find_categorical(dat)
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

    # predictions for each cell of all categorical data, but not the response
    dat <- predictions(model = model, variables = variables_grid, type = type)

    # interactions are not supported
    interactions <- any(grepl(":", attr(stats::terms(model), "term.labels")))
    if (isTRUE(interactions)) {
        warning("The `marginalmeans` function does not support models with interactions. The reported standard errors may be misleading.")
    }

    # model.matrix requires a dataset with response
    dat[[insight::find_response(model)[1]]] <- 0
    modmat <- insight::get_modelmatrix(model, data = dat)

    # marginal means and standard errors for a single variable
    get_mm_se <- function(v) {
        modmat_tmp <- modmat

        # assign columns of the matrix unrelated to v to their mean value
        idx <- grep(match(v, attr(stats::terms(model), "term.labels")), attr(modmat_tmp, "assign"))
        idx <- setdiff(1:ncol(modmat_tmp), idx)
        for (i in idx) {
            modmat_tmp[, i] <- mean(modmat_tmp[, i])
        }

        # one row per combination of the categorical variable
        idx <- duplicated(modmat_tmp)
        modmat_tmp <- modmat_tmp[!idx, , drop = FALSE]

        # marginal means
        f <- stats::as.formula(paste("predicted ~", v))
        yhat <- stats::aggregate(f, data = dat, FUN = mean)
        colnames(yhat) <- c("value", "predicted")
        yhat$term <- v

        # variance: M V M'
        # only supported on the links scale or for linear models
        if (type == "link" || isTRUE(insight::model_info(model)$is_linear)) {
            if (!all(colnames(vcov) %in% colnames(modmat_tmp)) || !all(colnames(vcov) %in% colnames(modmat_tmp))) {
                stop("The column names produced by `insight::get_varcov(model)` and `insight::get_modelmatrix(model, data=dat)` do not match. This can sometimes happen when using character variables as a factor when fitting a model. A safer strategy is to convert character variables to factors before fitting the model. This makes it easier for `marginaleffects` to keep track of the reference category.")
            }
            modmat_tmp <- modmat_tmp[, colnames(vcov), drop = FALSE]
            se <- dat[!idx, v, drop = FALSE]
            colnames(se)[match(colnames(se), v)] <- "value"
            se$term <- v
            se$std.error <- sqrt(colSums(t(modmat_tmp %*% vcov) * t(modmat_tmp)))
            out <- merge(yhat, se)
        } else {
            out <- yhat
        }
        idx <- intersect(c("term", "value", "predicted", "std.error"), colnames(out))
        out <- out[, idx]
        return(out)
    }

    out_list <- list()
    for (v in variables) {
        out_list[[v]] <- get_mm_se(v)
    }
    out_list <- out_list[sapply(out_list, function(x) !is.null(x))]

    # try to preserve term-value class, but convert if needed to bind
    if (length(out_list) > 1) {
        value_types <- sapply(out_list, function(x) class(x$value)[1])
        if (length(unique(value_types)) > 1) {
            for (i in seq_along(out_list)) {
                out_list[[i]]$value <- as.character(out_list[[i]]$value)
            }
        }
    }
    out <- do.call("rbind", out_list)
    row.names(out) <- NULL

    # attributes
    if (isTRUE(check_dependency("modelsummary"))) {
        gl <- suppressMessages(suppressWarnings(try(modelsummary::get_gof(model), silent = TRUE)))
        if (inherits(gl, "data.frame")) {
            attr(out, "glance") <- data.frame(gl)
        } else {
            attr(out, "glance") <- NULL
        }
    } else {
        attr(out, "glance") <- NULL
    }
    class(out) <- c("marginalmeans", class(out))
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables

    return(out)
}
