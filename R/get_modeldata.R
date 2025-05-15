get_modeldata <- function(
    model,
    additional_variables = FALSE,
    modeldata = NULL,
    wts = FALSE,
    ...
) {
    # mice
    if (inherits(model, c("mira", "amest"))) {
        return(modeldata)
    }

    # tidymodels: always require `newdata`, because sometimes there needs to be
    # some pre-processing, and we want to rely on the workflow to do that.
    # workflows are triggered on `stats::predict()`
    if (inherits(model, c("model_fit", "workflow"))) {
        if ("fit" %in% names(model)) {
            tmp <- try(get_modeldata(model$fit), silent = TRUE)
            if (inherits(tmp, "data.frame")) {
                out <- unpack_matrix_1col(tmp)
                data.table::setDT(out)
                return(out)
            }
        }
        return(NULL)
    }

    # otherwise, insight::get_data can sometimes return only the the outcome variable
    if (inherits(model, "bart")) {
        modeldata <- insight::get_data(model, additional_variables = TRUE)
        modeldata <- unpack_matrix_1col(modeldata)
        data.table::setDT(modeldata)
        return(modeldata)
    }

    if (!is.null(modeldata)) {
        modeldata <- set_variable_class(modeldata, model = model)
        return(modeldata)
    }

    # often used to extract `by`
    if (isTRUE(checkmate::check_data_frame(additional_variables))) {
        additional_variables <- colnames(additional_variables)
    }

    # always get weights
    if (isTRUE(checkmate::check_string(wts))) {
        additional_variables <- c(additional_variables, wts)
    }

    # feols weights can be a formula
    if (inherits(model, "fixest")) {
        fwts <- tryCatch(all.vars(model$call$weights), error = function(e) NULL)
        additional_variables <- c(additional_variables, fwts)
    }

    # always need weights in this model class
    if (any(c("svylm", "svyglm") %in% class(model))) {
        wts <- TRUE
    }

    # after by
    if (isTRUE(checkmate::check_flag(additional_variables))) {
        out <- hush(insight::get_data(
            model,
            additional_variables = additional_variables,
            verbose = FALSE
        ))
        out <- set_variable_class(out, model = model)
        return(out)
    }

    # always extract offset variable if available
    off <- hush(insight::find_offset(model))
    if (isTRUE(checkmate::check_formula(off))) {
        additional_variables <- c(additional_variables, hush(all.vars(off)))
    } else if (isTRUE(checkmate::check_character(off, max.len = 4))) {
        if (isTRUE(grepl("~", off))) {
            additional_variables <- c(
                additional_variables,
                hush(all.vars(stats::as.formula(off)))
            )
        } else {
            additional_variables <- c(additional_variables, off)
        }
    }

    # always extract weights variable if available
    wtsvname <- hush(insight::find_weights(model))
    if (isTRUE(checkmate::check_formula(wtsvname))) {
        additional_variables <- c(additional_variables, hush(all.vars(wtsvname)))
    } else if (isTRUE(checkmate::check_character(wtsvname, max.len = 4))) {
        if (isTRUE(grepl("~", wtsvname))) {
            additional_variables <- c(
                additional_variables,
                hush(all.vars(stats::as.formula(wtsvname)))
            )
        } else {
            additional_variables <- c(additional_variables, wtsvname)
        }
    }

    out <- hush(insight::get_data(
        model,
        verbose = FALSE,
        additional_variables = additional_variables
    ))

    if (isTRUE(wts)) {
        tmp <- insight::get_weights(model)
        if (is.numeric(tmp) && length(tmp) == nrow(out)) {
            out[["marginaleffects_wts_internal"]] <- tmp
        }
    }

    # iv_robust and some others
    if (is.null(out)) {
        out <- evalup(model[["call"]][["data"]])
    }

    if (is.null(out)) {
        out <- evalup(attr(model, "call")$data)
    }

    out <- unpack_matrix_1col(out)
    data.table::setDT(out)
    out <- set_variable_class(out, model = model)
    return(out)
}


set_variable_class <- function(modeldata, model = NULL) {
    if (is.null(modeldata)) return(modeldata)

    # this can be costly on large datasets, when only a portion of
    # variables are used in the model
    variables <- NULL
    if (is.null(model)) {
        variables <- tryCatch(
            unlist(insight::find_variables(model, flatten = TRUE), use.names = FALSE),
            error = function(e) NULL
        )
    }
    if (is.null(variables)) variables <- colnames(modeldata)

    out <- modeldata

    cl <- NULL
    for (col in variables) {
        if (is.logical(out[[col]])) {
            cl[col] <- "logical"
        } else if (is.character(out[[col]])) {
            cl[col] <- "character"
        } else if (is.factor(out[[col]])) {
            cl[col] <- "factor"
        } else if (inherits(out[[col]], "Surv")) {
            # is numeric but breaks the %in% 0:1 check
            cl[col] <- "other"
        } else if (is.numeric(out[[col]])) {
            if (is_binary(out[[col]])) {
                cl[col] <- "binary"
            } else {
                cl[col] <- "numeric"
            }
        } else {
            cl[col] <- "other"
        }
    }

    if (is.null(model)) {
        attr(out, "marginaleffects_variable_class") <- cl
        return(out)
    }

    te <- hush(insight::find_terms(model, flatten = TRUE))

    # in-formula factor
    regex <- "^(^as\\.factor|^factor)\\((.*)\\)"
    idx <- gsub(
        regex,
        "\\2",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "factor"

    # in-formula categoricals
    regex <- "^(^mo|^strata)\\((.*)\\)"
    idx <- gsub(
        regex,
        "\\2",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "strata"

    # in-formula numeric
    regex <- "^numeric\\((.*)\\)$|^as.numeric\\((.*)\\)$"
    idx <- gsub(
        regex,
        "\\1",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "numeric"

    # in-formula logical
    regex <- "^logical\\((.*)\\)$|^as.logical\\((.*)\\)$"
    idx <- gsub(
        regex,
        "\\1",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "logical"

    # in-formula: fixest::i()
    fi <- NULL
    idx <- grepl("^i\\(.*\\)$", te)
    if (sum(idx) > 0) {
        arg1 <- lapply(te[idx], function(x) hush(as.character(str2lang(x)[[2]])))
        arg2 <- lapply(te[idx], function(x) hush(as.character(str2lang(x)[[3]])))
        arg1 <- unlist(arg1, recursive = TRUE)
        arg2 <- unlist(arg2, recursive = TRUE)
        arg2 <- gsub("^i\\.", "", arg2[grepl("^i\\.", arg2)])
        fi <- unique(c(arg1, arg2))
    }
    for (f in fi) {
        cl[f] <- "cluster"
    }

    # attributes
    attr(out, "marginaleffects_variable_class") <- cl

    return(out)
}


get_variable_class <- function(newdata, variable = NULL, compare = NULL) {
    if ("marginaleffects_variable_class" %in% names(attributes(newdata))) {
        cl <- attributes(newdata)$marginaleffects_variable_class
    } else {
        newdata <- set_variable_class(newdata)
        cl <- attributes(newdata)$marginaleffects_variable_class
    }

    if (is.null(compare) && is.null(variable)) {
        out <- cl
    } else if (is.null(compare)) {
        out <- cl[variable]
    } else if (is.null(variable)) {
        if (isTRUE(compare == "categorical")) {
            out <- cl[
                cl %in%
                    c("factor", "character", "logical", "strata", "cluster", "binary")
            ]
        } else {
            out <- cl[cl %in% compare]
        }
    } else {
        if (isTRUE(compare == "categorical")) {
            out <- cl[variable] %in%
                c("factor", "character", "logical", "strata", "cluster", "binary")
        } else {
            out <- cl[variable] %in% compare
        }
    }

    return(out)
}
