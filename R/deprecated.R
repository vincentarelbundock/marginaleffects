#' Deprecated function
#'
#' @keywords internal
#' @export
deltamethod <- function(...) {
    .Deprecated("hypotheses()")
}


#' Deprecated function
#' 
#' @keywords internal
#' @export
marginaleffects <- function(...) {
    .Deprecated("slopes()")
    slopes(...)
}


#' Deprecated function
#' 
#' @keywords internal
#' @export
meffects <- marginaleffects



#' Deprecated function
#' 
#' @keywords internal
#' @export
datagridcf <- function(...) {
    .Deprecated('datagrid(x = 1:2, grid_type = "counterfactual")')
    datagridcf_internal(...)
}


#' Deprecated function
#'
#' @keywords internal
#' @export
marginal_means <- function(model,
                           variables = NULL,
                           newdata = NULL,
                           vcov = TRUE,
                           conf_level = 0.95,
                           type = NULL,
                           transform = NULL,
                           cross = FALSE,
                           hypothesis = NULL,
                           equivalence = NULL,
                           p_adjust = NULL,
                           df = Inf,
                           wts = "equal",
                           by = NULL,
                           numderiv = "fdforward",
                           ...) {

    .Deprecated('
predictions(mod,
  by = "x",
  newdata = datagrid(grid_type = \"balanced\"))
')

    # deprecation and backward compatibility
    dots <- list(...)
    sanity_equivalence_p_adjust(equivalence, p_adjust)
    if ("transform_post" %in% names(dots)) transform <- dots[["transform_post"]]
    if ("variables_grid" %in% names(dots)) {
        if (!is.null(newdata)) {
            insight::format_error("The `variables_grid` argument and has been replaced by `newdata`. These two arguments cannot be used simultaneously.")
        }
        newdata <- dots[["variables_grid"]]
    }

    if (!is.null(equivalence) && !is.null(p_adjust)) {
        insight::format_error("The `equivalence` and `p_adjust` arguments cannot be used together.")
    }

    numderiv = sanitize_numderiv(numderiv)

    # build call: match.call() doesn't work well in *apply()
    call_attr <- c(list(
        name = "marginal_means",
        model = model,
        newdata = newdata,
        variables = variables,
        type = type,
        vcov = vcov,
        by = by,
        conf_level = conf_level,
        transform = transform,
        wts = wts,
        hypothesis = hypothesis,
        equivalence = equivalence,
        p_adjust = p_adjust,
        df = df),
        list(...))
    call_attr <- do.call("call", call_attr)

    # multiple imputation
    if (inherits(model, c("mira", "amest"))) {
        out <- process_imputation(model, call_attr, marginal_means = TRUE)
        return(out)
    }

    # if type is NULL, we backtransform if relevant
    type_string <- sanitize_type(model = model, type = type, calling_function = "marginal_means")
    if (type_string == "invlink(link)") {
        if (is.null(hypothesis)) {
            type_call <- "link"
        } else {
            type_call <- "response"
            type_string <- "response"
            insight::format_warning('The `type="invlink"` argument is not available unless `hypothesis` is `NULL` or a single number. The value of the `type` argument was changed to "response" automatically. To suppress this warning, use `type="response"` explicitly in your function call.')
        }
    } else {
        type_call <- type_string
    }

    modeldata <- get_modeldata(model, additional_variables = FALSE, wts = wts)

    checkmate::assert_flag(cross)
    transform <- sanitize_transform(transform)
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
    if (inherits(model, c("brmsfit", "bart"))) {
        insight::format_error("This model object type is not yet supported by the `marginal_means` function.")
    }

    # fancy vcov processing to allow strings like "HC3"
    vcov_false <- isTRUE(vcov == FALSE)
    vcov <- get_vcov(model, vcov = vcov, type = type, ...)

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
    if (identical(wts, "equal")) {
        newgrid[["wts"]] <- 1

    } else if (identical(wts, "proportional")) {
        wtsgrid <- copy(data.table(modeldata)[, ..nonfocal])
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

    } else if (identical(wts, "cells")) {
    # https://stackoverflow.com/questions/66748520/what-is-the-difference-between-weights-cell-and-weights-proportional-in-r-pa
        idx <- c(focal, nonfocal)
        wtsgrid <- copy(data.table(modeldata)[, ..idx])
        if (length(idx) == 0) {
            newgrid[["wts"]] <- 1
            return(newgrid)
        } else {
            wtsgrid <- data.table(modeldata)[
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
        type = type_call,
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
            type = type_call,
            FUN = get_se_delta_marginalmeans,
            index = NULL,
            variables = focal,
            newdata = newgrid,
            cross = cross,
            modeldata = modeldata,
            hypothesis = hypothesis,
            by = by,
            numderiv = numderiv)
        args <- c(args, list(...))
        args[["equivalence"]] <- NULL
        se <- do.call(get_se_delta, args)

        # get rid of attributes in column
        out[["std.error"]] <- as.numeric(se)
        J <- attr(se, "jacobian")
    } else {
        J <- NULL
    }

    out <- get_ci(
        out,
        conf_level = conf_level,
        vcov = vcov,
        null_hypothesis = hypothesis_null,
        df = df,
        p_adjust = p_adjust,
        model = model,
        ...)

    # equivalence tests
    out <- equivalence(out, equivalence = equivalence, df = df, ...)

    # after assign draws
    if (identical(type_string, "invlink(link)")) {
        linv <- tryCatch(insight::link_inverse(model), error = function(e) identity)
        out <- backtransform(out, transform = linv)
    }
    out <- backtransform(out, transform)

    # column order
    cols <- c("rowid", "group", colnames(by), "term", "hypothesis", "value", variables, "estimate", "std.error", "statistic", "p.value", "s.value", "conf.low", "conf.high", sort(colnames(out)))
    cols <- unique(cols)
    cols <- intersect(cols, colnames(out))
    out <- out[, cols, drop = FALSE]

    # attributes
    attr(out, "model") <- model
    attr(out, "jacobian") <- J
    attr(out, "type") <- type_string
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables
    attr(out, "call") <- call_attr
    attr(out, "conf_level") <- conf_level
    attr(out, "transform_label") <- names(transform)[1]

    if (isTRUE(cross)) {
        attr(out, "variables_grid") <- setdiff(nonfocal, variables)
    } else {
        attr(out, "variables_grid") <- unique(c(nonfocal, variables))
    }

    if (inherits(model, "brmsfit")) {
        insight::check_if_installed("brms")
        attr(out, "nchains") <- brms::nchains(model)
    }

    class(out) <- c("marginalmeans", class(out))

    return(out)
}


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
    }

    data.table::setDT(out)

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
        out <- merge(out, by, sort = FALSE)
        out <- out[, .(estimate = mean(estimate)), by = "by"]
    }

    if (!is.null(hypothesis)) {
        out <- get_hypothesis(out, hypothesis, by = by)
    }

    return(out)
}


#' Deprecated function
#'
#' @keywords internal
#' @export
marginalmeans <- marginal_means
