#' Generate "counterfactual" datasets for use in `marginaleffects`'s `newdata` argument
#'
#' @param ... named arguments with vectors of values for the variables to construct (see Examples below.)
#' @param model Model object
#' @param data data.frame (one and only one of the `model` and `data` arguments
#' must be true).
#' @details
#' If `counterfactual` is used in a `marginaleffects` or `predictions` call as
#' the `newdata` argument, users do not need to specify the `model` or `data`
#' argument. The data is extracted automatically from the model.
#'
#' If users supply a model, the data used to fit that model is retrieved using
#' the `insight::get_data` function.
#' 
#' If users supply a model, the data used to fit that model is retrieved using
#' the `insight::get_data` function, and then replicated with different values
#' of the variables in the `at` list.
#' @return 
#' A `data.frame` where each row of the original data is repeated multiple
#' times for each of the values of the variables in the `at` list. See example
#' below.
#' @export
#' @examples
#' # All rows are repeated twice, with different values of `hp`
#' cd <- counterfactual(data = mtcars, hp = c(100, 110))
#' cd[cd$rowid %in% 1:3,]
#'
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp + wt, mtcars)
#' cd <- counterfactual(model = mod, hp = c(100, 110))
#' cd[cd$rowid %in% 1:3,]
#'
#' # Use in `marginaleffects` to compute "Counterfactual Average Marginal Effects"
#' marginaleffects(mod, newdata = counterfactual(hp = c(100, 110)))
counterfactual <- function(..., model = NULL, data = NULL) {

    tmp <- prep_counterfactual_typical(..., model = model, data = data)
    at <- tmp$at
    dat <- tmp$data
    variables_all <- tmp$all
    variables_manual <- tmp$variables_manual
    variables_automatic <- tmp$variables_automatic

    # `at` -> `data.frame`
    at <- lapply(at, unique)
    at <- expand.grid(at, stringsAsFactors = FALSE)

    rowid <- data.frame(rowid = 1:nrow(dat))
    if (length(variables_automatic) > 0) {
        dat_automatic <- dat[, variables_automatic, drop = FALSE]
        dat_automatic <- cbind(rowid, dat_automatic)
        out <- merge(dat_automatic, at, all = TRUE)
    }  else {
        out <- merge(rowid, at, all = TRUE)
    }

    return(out)
}


#' Generate "typical" datasets for use in `marginaleffects`'s `newdata` argument
#'
#' @param ... named arguments with vectors of values for the typical variables
#' to construct (see Examples below.) The typical data will include
#' combinations of unique values from these vectors
#' @param model Model object
#' @param data data.frame (one and only one of the `model` and `data` arguments
#' must be true).
#' @details
#' If `typical` is used in a `marginaleffects` or `predictions` call as the
#' `newdata` argument, users do not need to specify the `model` or `data`
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
#' typical(data = mtcars, hp = c(100, 110))
#'
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp, mtcars)
#' typical(model = mod, hp = c(100, 110))
#'
#' # Use in `marginaleffects` to compute "Typical Marginal Effects"
#' marginaleffects(mod, newdata = typical(hp = c(100, 110)))
typical <- function(..., model = NULL, data = NULL) {

    tmp <- prep_counterfactual_typical(..., model = model, data = data)
    at <- tmp$at
    dat <- tmp$data
    variables_all <- tmp$all
    variables_manual <- tmp$variables_manual
    variables_automatic <- tmp$variables_automatic

    if (length(variables_automatic) > 0) {
        dat_automatic <- dat[, variables_automatic, drop = FALSE]
        dat_automatic <- stats::na.omit(dat_automatic)
        out <- mean_or_mode(dat_automatic)
    } else {
        out <- list()
    }

    if (!is.null(at)) {
        for (n in names(at)) {
            out[n] <- at[n]
        }
    }

    out <- lapply(out, unique)
    out <- expand.grid(out, stringsAsFactors = FALSE)
    return(out)

}


prep_counterfactual_typical <- function(..., model = NULL, data = NULL) {

    checkmate::assert_data_frame(data, null.ok = TRUE)

    at <- list(...)

    if (!is.null(model) & !is.null(data)) {
        stop("One of the `model` or `data` arguments must be `NULL`.")
    }

    if (is.null(model) & is.null(data)) {
        stop("The `model` and `data` arguments should not both be `NULL`.")
    }

    # data: all variables
    if (!is.null(data)) {
        variables <- colnames(data)
    # model: variables=NULL because otherwise `sanity_variables` excludes others
    } else {
        variables <- NULL
    }

    variables_list <- sanity_variables(model = model, newdata = data, variables = variables)
    variables_all <- unique(unlist(variables_list))
    variables_manual <- names(at)
    variables_automatic <- setdiff(variables_all, variables_manual)

    # fill in missing data after sanity checks
    if (is.null(data)) {
        data <- insight::get_data(model)
    }

    # check `at` names
    variables_missing <- setdiff(names(at), variables_all)
    if (length(variables_missing) > 0) {
        warning(sprintf("Elements of the `variables` argument are missing from the model data: %s", 
                        paste(variables_missing, collapse = ", ")))
    }

    # check `at` elements and convert them to factor as needed
    for (n in names(at)) {
        if (is.factor(data[[n]])) {
            at[[n]] <- as.character(at[[n]])
            if (!all(at[[n]] %in% levels(data[[n]]))) {
                msg <- sprintf('The "%s" element of the `at` list corresponds to a factor variable. The values entered in the `at` list must be one of the factor levels: "%s".', n, paste(levels(data[[n]]), collapse = '", "'))
                stop(msg)
            } else {
                at[[n]] <- factor(at[[n]], levels = levels(data[[n]]))
            }
        }
    }

    # warn if cluster variables are numeric. users probably do not want to take
    # their means, because this makes prediction impossible in many models
    # (e.g., `fixest::feols(mpg ~ hp | cyl)`)
    variables_cluster <- intersect(variables_automatic, variables_list$cluster)
    if (length(variables_cluster) > 0) {
        idx <- sapply(variables_cluster, function(x) is.numeric(data[[x]]))
        if (any(idx)) {
            idx <- paste(sprintf('"%s"', variables_cluster[idx]), collapse = ", ")
            warning(sprintf("Unless otherwise instructed, this function sets numeric variables to their mean. This is probably inappropriate in the case of cluster variables such as %s. A safer strategy is to convert cluster variables to factors before fitting the model.", idx))
        }
    }

    out <- list("data" = data, 
                "at" = at, 
                "variables_all" = variables_all, 
                "variables_manual" = variables_manual, 
                "variables_automatic" = variables_automatic)
    return(out)
}
