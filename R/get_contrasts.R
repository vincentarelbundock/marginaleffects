get_contrasts <- function(model,
                          variable,
                          newdata = insight::get_data(model),
                          type = "response",
                          step_size = 1,
                          normalize_dydx = FALSE,
                          numDeriv_method = NULL, # do not push to ...
                          ...) {

    # if `newdata` is a call to `datagrid`, `typical`, or `counterfactual`, insert `model`
    scall <- substitute(newdata)
    if (is.call(scall) && as.character(scall)[1] %in% c("datagrid", "typical", "counterfactual")) {
        lcall <- as.list(scall)
        if (!any(c("model", "data") %in% names(lcall))) {
            lcall <- c(lcall, list("model" = model))
            newdata <- eval.parent(as.call(lcall))
        }
    }
    newdata <- sanity_newdata(model, newdata)

    # Create counterfactual datasets with different factor values and compare the predictions
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    if (is.factor(newdata[[variable]]) || isTRUE(attr(newdata[[variable]], "factor"))) {
        get_contrasts_fun <- get_contrasts_factor
    } else if (is.logical(newdata[[variable]])) {
        get_contrasts_fun <- get_contrasts_logical
    } else if (is.character(newdata[[variable]])) {
        get_contrasts_fun <- get_contrasts_character
    } else if (is.numeric(newdata[[variable]])) {
        get_contrasts_fun <- get_contrasts_numeric
    } else {
        stop(sprintf("Cannot compute contrasts for variable %s of class %s",
                     variable,
                     class(newdata[[variable]])))
    }

    out <- get_contrasts_fun(model = model,
                             variable = variable,
                             newdata = newdata,
                             type = type,
                             step_size = step_size,
                             normalize_dydx = normalize_dydx,
                             ...)

    # required for merging in models with multiple response levels
    if (!"group" %in% colnames(out)) {
        out$group <- "main_marginaleffect"
    }

    return(out)
}


get_contrasts_logical <- function(model,
                                  newdata,
                                  variable,
                                  type = "response",
                                  ...) {

    baseline <- newdata
    baseline[[variable]] <- FALSE
    pred_false <- get_predict(model,
                              newdata = baseline,
                              type = type,
                              ...)
    baseline[[variable]] <- TRUE
    pred_true <- get_predict(model = model,
                             newdata = baseline,
                             type = type,
                             ...)

    baseline$estimate <- pred_true$predicted - pred_false$predicted
    baseline$term <- variable
    baseline$contrast <- "TRUE - FALSE"
    cols <- intersect(colnames(baseline),
                      c("rowid", "group", "term", "contrast", "estimate"))
    pred <- baseline[, cols]
    row.names(pred) <- NULL

    # bayes: posterior draws and credible intervals
    if ("posterior_draws" %in% names(attributes(pred_false))) {
        attr(pred, "posterior_draws") <- attr(pred_true, "posterior_draws") - attr(pred_false, "posterior_draws")
    }

    return(pred)
}


get_contrasts_factor <- function(model,
                                 newdata,
                                 variable,
                                 type = "response",
                                 ...) {

    baseline <- newdata
    pred_list <- list()
    if (is.factor(baseline[[variable]])) {
        levs <- levels(baseline[[variable]])
    } else {
        original_data <- insight::get_data(model)
        if (is.factor(original_data[[variable]])) {
            levs <- levels(original_data[[variable]])
        } else {
            levs <- sort(unique(original_data[[variable]]))
        }
    }
    baseline[[variable]] <- factor(levs[1], levels = levs)
    baseline_prediction <- get_predict(model,
                                       newdata = baseline,
                                       type = type,
                                       ...)
    draws_list <- list()

    for (i in 2:length(levs)) {
        baseline[[variable]] <- factor(levs[i], levels = levs)
        incremented_prediction <- get_predict(model = model,
                                              newdata = baseline,
                                              type = type,
                                              ...)
        incremented_prediction$term <- variable
        incremented_prediction$contrast <- sprintf("%s - %s", levs[i], levs[1])
        incremented_prediction$estimate <- incremented_prediction$predicted -
                                           baseline_prediction$predicted
        incremented_prediction$predicted <- NULL
        pred_list[[i]] <- incremented_prediction

        # bayes: posterior draws and credible intervals
        if ("posterior_draws" %in% names(attributes(baseline_prediction))) {
            draws_list[[i]] <- attr(incremented_prediction, "posterior_draws") -
                               attr(baseline_prediction, "posterior_draws")
        }
    }
    pred <- do.call("rbind", pred_list)
    draws <- do.call("rbind", draws_list)

    # TODO: check if this is still relevant
    # # clean for hurdle models from package `pscl`
    # coef_names <- gsub("count_|zero_", "", coef_names)

    # output
    cols <- intersect(colnames(pred), c("rowid", "group", "term", "contrast", "estimate"))
    row.names(pred) <- NULL
    attr(pred, "posterior_draws") <- draws

    return(pred)
}


get_contrasts_character <- function(model,
                                    newdata,
                                    variable,
                                    type = "response",
                                    ...) {

    # factors store all levels, but characters do not, so we need to extract the
    # original data from the model.
    tmp <- insight::get_data(model)
    levs <- sort(unique(tmp[[variable]]))

    pred_list <- list()
    baseline <- newdata
    baseline[[variable]] <- levs[1]
    baseline_prediction <- get_predict(model,
                                       newdata = baseline,
                                       type = type,
                                       ...)

    draws_list <- list()
    for (i in 2:length(levs)) {
        pred <- baseline
        pred[[variable]] <- levs[i]
        incremented_prediction <- get_predict(model = model,
                                              newdata = pred,
                                              type = type,
                                              ...)

        contr <- as.vector(incremented_prediction$predicted) -
                 as.vector(baseline_prediction$predicted)

        # bayes: posterior draws and credible intervals
        if ("posterior_draws" %in% names(attributes(baseline_prediction))) {
            draws_list[[i]] <- attr(incremented_prediction, "posterior_draws") -
                               attr(baseline_prediction, "posterior_draws")
        }

        tmp <- incremented_prediction
        tmp$predicted <- NULL
        tmp$term <- variable
        tmp$contrast <- sprintf("%s - %s", levs[i], levs[1])
        tmp$estimate <- contr
        pred_list[[i - 1]] <- tmp
    }

    pred <- do.call("rbind", pred_list)
    draws <- do.call("rbind", draws_list)
    cols <- intersect(c("rowid", "group", "term", "contrast", "estimate"), colnames(pred))
    pred <- pred[, cols]
    row.names(pred) <- NULL
    attr(pred, "posterior_draws") <- draws
    return(pred)
}


get_contrasts_numeric <- function(model,
                                  newdata,
                                  variable,
                                  type = "response",
                                  step_size = 1,
                                  normalize_dydx = FALSE,
                                  return_data = FALSE,
                                  ...) {

    baseline <- newdata

    pred_baseline <- get_predict(model,
                                 newdata = baseline,
                                 type = type,
                                 ...)

    baseline[[variable]] <- baseline[[variable]] + step_size
    pred_increment <- get_predict(model,
                                  newdata = baseline,
                                  type = type,
                                  ...)

    contr <- as.vector(pred_increment$predicted) - as.vector(pred_baseline$predicted)

    pred_increment$term <- variable

    if (isTRUE(normalize_dydx)) {
        contr <- contr / step_size
        pred_increment$contrast <- "dydx"
        pred_increment$estimate <- contr
    } else {
        pred_increment$contrast <- sprintf("+%s", step_size)
        pred_increment$estimate <- contr
    }
    pred_increment$predicted <- NULL

    out <- pred_increment

    # subset columns before assigning attributes later
    if (!isTRUE(return_data)) {
        cols <- intersect(colnames(out), c("rowid", "term", "group", "variable", "term", "contrast", "estimate", "conf.low", "conf.high"))
        out <- out[, cols]
    } else {
        out <- merge(out, newdata, all.x = TRUE)
    }

    # bayes: posterior draws and credible intervals
    if ("posterior_draws" %in% names(attributes(pred_increment))) {
        draws <- attr(pred_increment, "posterior_draws") - attr(pred_baseline, "posterior_draws")
        if (isTRUE(normalize_dydx)) {
            attr(out, "posterior_draws") <- draws / step_size
        } else {
            attr(out, "posterior_draws") <- draws
        }
    }

    return(out)
}
