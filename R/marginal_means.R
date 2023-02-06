#' Marginal Means
#'
#' @description
#' Marginal means are adjusted predictions, averaged across a grid of categorical predictors,
#' holding other numeric predictors at their means. To learn more, read the marginal means vignette, visit the
#' package website, or scroll down this page for a full list of vignettes:
#' 
#' * <https://vincentarelbundock.github.io/marginaleffects/articles/marginalmeans.html>
#' * <https://vincentarelbundock.github.io/marginaleffects/>
#'
#' @param variables Focal variables
#' + Character vector of variable names: compute marginal means for each category of the listed variables.
#' + `NULL`: calculate marginal means for all logical, character, or factor variables in the dataset used to fit `model`. Hint:  Set `cross=TRUE` to compute marginal means for combinations of focal variables.
#' @param newdata Grid of predictor values over which we marginalize.
#' + `NULL` create a grid with all combinations of all categorical predictors in the model. Warning: can be expensive.
#' + Character vector: subset of categorical variables to use when building the balanced grid of predictors. Other variables are held to their mean or mode.
#' + Data frame: must include all the variables used to fit the model.
#' @param type string indicates the type (scale) of the predictions used to
#' compute marginal effects or contrasts. This can differ based on the model
#' type, but will typically be a string such as: "response", "link", "probs",
#' or "zero". When an unsupported string is entered, the model-specific list of
#' acceptable values is returned in an error message. When `type` is `NULL`, the
#' default value is used. This default is the first model-related row in
#' the `marginaleffects:::type_dictionary` dataframe. If `type` is `NULL` and
#' the default value is "response", the function tries to compute marginal means
#' on the link scale before backtransforming them using the inverse link function.
#' @param wts character value. Weights to use in the averaging.
#' + "equal": each combination of variables in `newdata` gets equal weight.
#' + "cells": each combination of values for the variables in the `newdata` gets a weight proportional to its frequency in the original data.
#' + "proportional": each combination of values for the variables in `newdata` -- except for those in the `variables` argument -- gets a weight proportional to its frequency in the original data.
#' @param cross TRUE or FALSE
#' * `FALSE` (default): Marginal means are computed for each predictor individually.
#' * `TRUE`: Marginal means are computed for each combination of predictors specified in the `variables` argument.
#' @param by Collapse marginal means into categories. Data frame with a `by` column of group labels, and merging columns shared by `newdata` or the data frame produced by calling the same function without the `by` argument.
#' @inheritParams slopes
#' @inheritParams predictions
#' @inheritParams comparisons
#' @details
#'   This function begins by calling the `predictions` function to obtain a
#'   grid of predictors, and adjusted predictions for each cell. The grid
#'   includes all combinations of the categorical variables listed in the
#'   `variables` and `newdata` arguments, or all combinations of the
#'   categorical variables used to fit the model if `newdata` is `NULL`.
#'   In the prediction grid, numeric variables are held at their means.
#'
#'   After constructing the grid and filling the grid with adjusted predictions,
#'   `marginal_means` computes marginal means for the variables listed in the
#'   `variables` argument, by average across all categories in the grid.
#'
#'   `marginal_means` can only compute standard errors for linear models, or for
#'   predictions on the link scale, that is, with the `type` argument set to
#'   "link".
#'
#'   The `marginaleffects` website compares the output of this function to the
#'   popular `emmeans` package, which provides similar but more advanced
#'   functionality: https://vincentarelbundock.github.io/marginaleffects/
#'
#' @template model_specific_arguments
#' @template bayesian
#'
#' @return Data frame of marginal means with one row per variable-value
#' combination.
#' @export
#' @examples
#' library(marginaleffects)
#'
#' # simple marginal means for each level of `cyl`
#' dat <- mtcars
#' dat$carb <- factor(dat$carb)
#' dat$cyl <- factor(dat$cyl)
#' dat$am <- as.logical(dat$am)
#' mod <- lm(mpg ~ carb + cyl + am, dat)
#' 
#' marginal_means(
#'   mod,
#'   variables = "cyl")
#' 
#' # collapse levels of cyl by averaging
#' by <- data.frame(
#'   cyl = c(4, 6, 8),
#'   by = c("4 & 6", "4 & 6", "8"))
#' marginal_means(mod,
#'   variables = "cyl",
#'   by = by)
#' 
#' # pairwise differences between collapsed levels
#' marginal_means(mod,
#'   variables = "cyl",
#'   by = by,
#'   hypothesis = "pairwise")
#' 
#' # cross
#' marginal_means(mod,
#'   variables = c("cyl", "carb"),
#'   cross = TRUE)
#' 
#' # collapsed cross
#' by <- expand.grid(
#'   cyl = unique(mtcars$cyl),
#'   carb = unique(mtcars$carb))
#' by$by <- ifelse(
#'   by$cyl == 4,
#'   paste("Control:", by$carb),
#'   paste("Treatment:", by$carb))
#' 
#' 
#' # Convert numeric variables to categorical before fitting the model
#' dat <- mtcars
#' dat$am <- as.logical(dat$am)
#' dat$carb <- as.factor(dat$carb)
#' mod <- lm(mpg ~ hp + am + carb, data = dat)
#'
#' # Compute and summarize marginal means
#' marginal_means(mod)
#'
#' # Contrast between marginal means (carb2 - carb1), or "is the 1st marginal means equal to the 2nd?"
#' # see the vignette on "Hypothesis Tests and Custom Contrasts" on the `marginaleffects` website.
#' lc <- c(-1, 1, 0, 0, 0, 0)
#' marginal_means(mod, variables = "carb", hypothesis = "b2 = b1")
#'
#' marginal_means(mod, variables = "carb", hypothesis = lc)
#'
#' # Multiple custom contrasts
#' lc <- matrix(c(
#'     -2, 1, 1, 0, -1, 1,
#'     -1, 1, 0, 0, 0, 0
#'     ),
#'   ncol = 2,
#'   dimnames = list(NULL, c("A", "B")))
#' marginal_means(mod, variables = "carb", hypothesis = lc)
#' 
marginal_means <- function(model,
                           variables = NULL,
                           newdata = NULL,
                           vcov = TRUE,
                           conf_level = 0.95,
                           type = NULL,
                           transform_post = NULL,
                           cross = FALSE,
                           hypothesis = NULL,
                           df = Inf,
                           wts = "equal",
                           by = NULL,
                           ...) {


    # deprecation and backward compatibility
    dots <- list(...)
    if ("variables_grid" %in% names(dots)) {
        if (!is.null(newdata)) {
            insight::format_error("The `variables_grid` argument and has been replaced by `newdata`. These two arguments cannot be used simultaneously.")
        }
        newdata <- dots[["variables_grid"]]
    }

    # if type is NULL, we backtransform if relevant
    if (is.null(type)) {
        type <- sanitize_type(model = model, type = type, calling_function = "marginalmeans")
        linv <- tryCatch(
            insight::link_inverse(model),
            error = function(e) NULL)
        if (type == "response" &&
            is.null(transform_post) &&
            class(model)[1] %in% type_dictionary$class &&
            isTRUE("link" %in% subset(type_dictionary, class == class(model)[1])$base) &&
            is.function(linv)) {
            type <- "link"
            transform_post <- linv
        }
    } else {
        type <- sanitize_type(model = model, type = type, calling_function = "marginalmeans")
    }

    modeldata <- get_modeldata(model)

    checkmate::assert_flag(cross)
    transform_post <- sanitize_transform_post(transform_post)
    conf_level <- sanitize_conf_level(conf_level, ...)
    model <- sanitize_model(model, vcov = vcov, calling_function = "marginalmeans")

    checkmate::assert_choice(wts, choices = c("equal", "cells", "proportional"))
    if (wts != "equal" && is.data.frame(newdata)) {
        insight::format_error('The `wts` argument must be "equal" when `newdata` is a data frame.')
    }

    tmp <- sanitize_hypothesis(hypothesis, ...)
    hypothesis <- tmp$hypothesis
    hypothesis_null <- tmp$hypothesis_null


    sanity_dots(model = model, ...)
    if (inherits(model, "brmsfit")) {
        insight::format_error(c(
            "`brmsfit` objects are yet not supported by the `marginal_means` function.  Follow this link to track progress:",
            "https://github.com/vincentarelbundock/marginaleffects/issues/137"))
    }

    # fancy vcov processing to allow strings like "HC3"
    vcov_false <- isTRUE(vcov == FALSE)
    vcov <- get_vcov(model, vcov = vcov, ...)

    # focal categorical variables
    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)
    if (any(variables %in% insight::find_response(model))) {
        insight::format_error("The `variables` vector cannot include the response.")
    }
    if (is.null(variables)) {
        variables <- insight::find_predictors(model, flatten = TRUE)
    }
    idx <- vapply(
        variables,
        FUN = get_variable_class,
        newdata = modeldata,
        FUN.VALUE = logical(1),
        compare = c("logical", "character", "factor"))
    focal <- variables[idx]
    if (length(focal) == 0) {
        insight::format_error("No categorical predictor was found in the model data or `variables` argument.")
    }

    # non-focal categorical variables
    checkmate::assert(
        checkmate::check_null(newdata),
        checkmate::check_character(newdata),
        checkmate::check_data_frame(newdata))
    if (is.null(newdata)) {
        nonfocal <- insight::find_predictors(model, flatten = TRUE)
        nonfocal <- setdiff(nonfocal, focal)
    } else if (is.character(newdata)) {
        if (!all(newdata %in% colnames(modeldata))) {
            insight::format_error("Some of the variables in `newdata` are missing from the data used to fit the model.")
        }
        nonfocal <- setdiff(newdata, focal)
    } else if (is.data.frame(newdata)) {
        nonfocal <- colnames(newdata)
    }
    idx <- vapply(
        nonfocal,
        FUN = get_variable_class,
        newdata = modeldata,
        FUN.VALUE = logical(1),
        compare = c("logical", "character", "factor"))
    nonfocal <- nonfocal[idx]

    # grid
    args <- list(model = model)
    if (is.data.frame(newdata)) {
        for (v in focal) {
            args[[v]] <- unique(modeldata[[v]])
        }
        newgrid <- do.call(datagridcf, args)
    } else {
        for (v in c(focal, nonfocal)) {
            args[[v]] <- unique(modeldata[[v]])
        }
        newgrid <- do.call(datagrid, args)
    }

    # by: usual tests + only data frames in `marginal_means()`
    # after newgrid
    checkmate::assert_data_frame(by, null.ok = TRUE)
    sanity_by(by, newgrid)

    # weights
    wtsgrid <- copy(data.table(newgrid)[, ..nonfocal])
    if (identical(wts, "equal")) {
        newgrid[["wts"]] <- 1

    } else if (identical(wts, "cells")) {
        idx <- nonfocal
        wtsgrid[, N := .N]
        wtsgrid[, "wts" := .N / N, by = idx]
        # sometimes datagrid() converts to factors when there is a transformation
        # in the model formula, so we need to standardize the data
        for (v in colnames(newgrid)) {
            if (v %in% colnames(wtsgrid) && is.factor(newgrid[[v]])) {
                wtsgrid[[v]] <- factor(wtsgrid[[v]], levels = levels(newgrid[[v]]))
            }
        }
        wtsgrid <- unique(wtsgrid)
        newgrid <- merge(newgrid, wtsgrid, all.x = TRUE)
        newgrid[["wts"]][is.na(newgrid[["wts"]])] <- 0

    } else if (identical(wts, "proportional")) {
    # https://stackoverflow.com/questions/66748520/what-is-the-difference-between-weights-cell-and-weights-proportional-in-r-pa
        idx <- setdiff(nonfocal, variables)
        if (length(idx) == 0) {
            newgrid[["wts"]] <- 1
            return(newgrid)
        } else {
            wtsgrid <- data.table(newgrid)[
                , .(wts = .N), by = idx][
                , wts := wts / sum(wts)]
            # sometimes datagrid() converts to factors when there is a transformation
            # in the model formula, so we need to standardize the data
            for (v in colnames(newgrid)) {
                if (v %in% colnames(wtsgrid) && is.factor(newgrid[[v]])) {
                    wtsgrid[[v]] <- factor(wtsgrid[[v]], levels = levels(newgrid[[v]]))
                 }
            }
            wtsgrid <- unique(wtsgrid)
            newgrid <- merge(newgrid, wtsgrid, all.x = TRUE)
            newgrid[["wts"]][is.na(newgrid[["wts"]])] <- 0

        }
    }

    # `equivalence` should not be passed to predictions() at this stage
    args <- list(
        model = model,
        newdata = newgrid,
        type = type,
        variables = focal,
        cross = cross,
        hypothesis = hypothesis,
        by = by,
        modeldata = modeldata)
    args <- c(args, list(...))
    args[["equivalence"]] <- NULL
    mm <- do.call(get_marginalmeans, args)

    # we want consistent output, regardless of whether `data.table` is installed/used or not
    out <- as.data.frame(mm)

    # standard errors via delta method
    if (!vcov_false) {
        args <- list(
            model,
            vcov = vcov,
            type = type,
            FUN = get_se_delta_marginalmeans,
            index = NULL,
            variables = focal,
            newdata = newgrid,
            cross = cross,
            modeldata = modeldata,
            hypothesis = hypothesis,
            by = by)
        args <- c(args, list(...))
        args[["equivalence"]] <- NULL
        se <- do.call(get_se_delta, args)

        # get rid of attributes in column
        out[["std.error"]] <- as.numeric(se)
        J <- attr(se, "jacobian")
    } else {
        J <- NULL
    }

    lin <- tryCatch(insight::model_info(model)$is_linear, error = function(e) FALSE)
    if (isTRUE(type == "link") || isTRUE(lin)) {
        out <- get_ci(
            out,
            conf_level = conf_level,
            vcov = vcov,
            null_hypothesis = hypothesis_null,
            df = df,
            model = model,
            ...)
    }

    # equivalence tests
    out <- equivalence(out, df = df, ...)

    # after assign draws
    out <- backtransform(out, transform_post)

    # column order
    cols <- c("rowid", "type", "group", colnames(by), "term", "hypothesis", "value", variables, "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", sort(colnames(out)))
    cols <- unique(cols)
    cols <- intersect(cols, colnames(out))
    out <- out[, cols, drop = FALSE]

    # attributes
    class(out) <- c("marginalmeans", class(out))
    attr(out, "model") <- model
    attr(out, "jacobian") <- J
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables
    attr(out, "call") <- match.call()
    attr(out, "conf_level") <- conf_level
    attr(out, "transform_post_label") <- names(transform_post)[1]

    if (isTRUE(cross)) {
        attr(out, "variables_grid") <- setdiff(nonfocal, variables)
    } else {
        attr(out, "variables_grid") <- unique(c(nonfocal, variables))
    }

    if (inherits(model, "brmsfit")) {
        insight::check_if_installed("brms")
        attr(out, "nchains") <- brms::nchains(model)
    }

    return(out)
}


#' Workhorse function for `marginal_means`
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
                              cross,
                              modeldata,
                              hypothesis = NULL,
                              by = NULL,
                              ...) {

    if ("wts" %in% colnames(newdata)) {
        wts <- "wts"
    } else {
        wts <- NULL
    }

    # predictions for each cell of all categorical data, but not the response
    if (isTRUE(cross) || length(variables) == 1) {
        out <- predictions(
            model = model,
            newdata = newdata,
            type = type,
            vcov = FALSE,
            modeldata = modeldata,
            wts = wts,
            by = c("group", variables),
            ...)
        if (length(variables) == 1) {
            out$term <- variables
            out$value <- out[[variables]]
        }

    # predictions for each variable individual, then bind
    } else {
        pred_list <- draw_list <- list()
        for (v in variables) {
            tmp <- predictions(
                model = model,
                newdata = newdata,
                type = type,
                vcov = FALSE,
                modeldata = modeldata,
                wts = wts,
                by = c("group", v),
                ...)
            tmp$rowid <- NULL
            draw_list[[v]] <- attr(tmp, "posterior_draws")
            tmp$term <- v
            data.table::setnames(tmp, old = v, new = "value")
            pred_list[[v]] <- tmp
        }
        # try to preserve term-value class, but convert to character if needed to bind
        classes <- sapply(pred_list, function(x) class(x$value)[1])
        if (length(unique(classes)) > 1) {
            for (i in seq_along(pred_list)) {
                pred_list[[i]]$value <- as.character(pred_list[[i]]$value)
            }
        }
        out <- rbindlist(pred_list)
        data.table::setorderv(out, c("term", "value"))
    }

    data.table::setDT(out)
    keys <- intersect(colnames(out), c("term", "value", variables))
    data.table::setorderv(out, keys)

    if (isTRUE(checkmate::check_data_frame(by))) {
        # warnings for factor vs numeric vs character. merge.data.table usually still works.
        bycols <- intersect(colnames(out), colnames(by))
        if (length(bycols) == 0) {
            msg <- "There is no common columns in `by` and in the output of `marginal_means()`. Make sure one of the entries in the `variables` argument corresponds to one of the columns in `by`."
            insight::format_error(msg)
        }
        for (b in bycols) {
            if (is.factor(out[[b]]) && is.numeric(by[[b]])) {
                out[[b]] <- as.numeric(as.character(out[[b]]))
            } else if (is.numeric(out[[b]]) && is.factor(by[[b]])) {
                by[[b]] <- as.numeric(as.character(by[[b]]))
            } else if (is.factor(out[[b]]) && is.character(by[[b]])) {
                out[[b]] <- as.character(out[[b]])
            } else if (is.character(out[[b]]) && is.factor(by[[b]])) {
                by[[b]] <- as.character(by[[b]])
            }
        }
        out <- merge(out, by)
        out <- out[, .(estimate = mean(estimate)), by = "by"]
    }

    if (!is.null(hypothesis)) {
        out <- get_hypothesis(out, hypothesis, by = by)
    }

    return(out)
}



#' `marginal_means()` is an alias to `marginal_means()`
#' 
#' This alias is kept for backward compatibility and because some users may prefer that name.
#'
#' @inherit marginal_means
#' @keywords internal
#' @export
marginalmeans <- marginal_means