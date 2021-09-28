#' Marginal Means
#'
#' Compute estimated marginal means for specified factors.
#'
#' @inheritParams marginaleffects
#' @param variables predictors over which to compute marginal means (character
#'   vector). `NULL` calculates marginal means for all logical, character, or
#'   factor variables in the dataset used to fit `model`.
#' @details
#'   This function begins by calling the `predictions` function to
#'   obtain a grid of predictors, including cells for all combinations of all
#'   categorical variables used to fit `model`, with numeric variables held at
#'   their means. Then, it computes marginal means for the variables listed in
#'   the `variables` argument.
#'
#'   The `marginaleffects` website compares the output of this function to the
#'   popular `emmeans` package, which provides similar functionality and more
#'   advanced options: https://vincentarelbundock.github.io/marginaleffects/
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
                          vcov = insight::get_varcov(model)) {

    # TODO: remove this when we allow a predict type again
    type <- "expectation"

    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)

    dat <- insight::get_data(model)

    # variable selection
    column_labels <- colnames(dat)
    term_labels <- insight::find_terms(model, flatten = TRUE)
    variables_valid <- get_categorical(dat)
    variables_valid <- intersect(variables_valid, term_labels)
    if (is.null(variables)) {
        variables <- variables_valid
    } else {
        variables <- intersect(variables, variables_valid)
    }
    if (length(variables_valid) == 0) {
        stop("No logical, factor, or character variable was found in the dataset used to fit the `model` object. This error is often raised when users convert variables to factor in the model formula (e.g., `lm(y ~ factor(x)`). If this is the case, you may consider converting variables in the dataset before fitting the model.")
    }

    # predictions for each cell of all categorical data
    variables_categorical <- get_categorical(dat)
    dat <- predictions(model = model, variables = variables_categorical)

    # interactions are not supported
    interactions <- any(grepl(":", attr(stats::terms(model), "term.labels")))
    if (isTRUE(interactions)) {
        warning("The `marginalmeans` function does not support models with interactions. The reported standard errors may be misleading.")
    }

    # model.matrix requires a dataset with response
    dat[[insight::find_response(model)[1]]] <- 0
    mm <- insight::get_modelmatrix(model, data = dat)

    # marginal means and standard errors for a single variable
    get_mm_se <- function(v) {
        mm_tmp <- mm

        # assign columns of the matrix unrelated to v to their mean value
        idx <- grep(match(v, attr(stats::terms(model), "term.labels")), attr(mm_tmp, "assign"))
        idx <- setdiff(1:ncol(mm_tmp), idx)
        for (i in idx) {
            mm_tmp[, i] <- mean(mm_tmp[, i])
        }

        # one row per combination of the categorical variable
        mm_tmp<- unique(mm_tmp)

        # marginal means
        f <- stats::as.formula(paste("predicted ~", v))
        yhat <- stats::aggregate(f, data = dat, FUN = mean)
        colnames(yhat) <- c("value", "predicted")
        yhat$term <- v

        # variance: M V M'
        se <- data.frame(
            term = v,
            value = unique(dat[[v]]),
            std.error = sqrt(diag(mm_tmp %*% vcov %*% t(mm_tmp))))

        # output
        out <- merge(yhat, se)
        out <- out[, c("term", "value", "predicted", "std.error")]
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
