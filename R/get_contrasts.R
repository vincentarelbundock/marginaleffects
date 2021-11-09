get_contrasts <- function(model,
                          variable,
                          newdata = insight::get_data(model),
                          group_name = NULL,
                          type = "response",
                          step_size = 1,
                          contrast_to_dydx = FALSE,
                          ...) {

    # if `newdata` is a call to `typical()` or `counterfactual()`, insert `model`
    scall <- substitute(newdata)
    if (is.call(scall) && as.character(scall)[1] %in% c("typical", "counterfactual")) {
        lcall <- as.list(scall)
        if (!any(c("model", "data") %in% names(lcall))) {
            lcall <- c(lcall, list("model" = model))
            newdata <- eval.parent(as.call(lcall))
        }
    }
    newdata <- sanity_newdata(model, newdata)

    # Create counterfactual datasets with different factor values and compare the predictions
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- 1:nrow(newdata)
    }

    if (is.factor(newdata[[variable]])) {
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
                             baseline = newdata,
                             group_name = group_name,
                             type = type,
                             step_size = step_size,
                             contrast_to_dydx = contrast_to_dydx,
                             ...)
    return(out)
}


get_contrasts_logical <- function(model,
                                  baseline,
                                  variable,
                                  group_name = NULL,
                                  type = "response",
                                  ...) {
    baseline[[variable]] <- FALSE
    pred_true <- get_predict(model,
                             newdata = baseline,
                             type = type,
                             group_name = group_name)
    baseline[[variable]] <- TRUE
    pred_false <- get_predict(model = model,
                              newdata = baseline,
                              type = type,
                              group_name = group_name)
    baseline$predicted <- pred_true - pred_false
    baseline$term <- paste0(variable, baseline[[variable]])
    pred <- baseline[, c("rowid", "term", "predicted")]
    colnames(pred) <- c("rowid", "term", "contrast")
    row.names(pred) <- NULL
    return(pred)
}


get_contrasts_factor <- function(model,
                                 baseline,
                                 variable,
                                 group_name = NULL,
                                 type = "response",
                                 ...) {
    pred_list <- list()
    baseline[[variable]] <- factor(levels(baseline[[variable]])[1], levels = levels(baseline[[variable]]))
    baseline_prediction <- get_predict(model,
                                       newdata = baseline,
                                       type = type,
                                       group_name = group_name)
    for (i in 2:length(levels(baseline[[variable]]))) {
        baseline[[variable]] <- factor(levels(baseline[[variable]])[i], levels = levels(baseline[[variable]]))
        baseline$predicted <- get_predict(model = model,
                                          newdata = baseline,
                                          type = type,
                                          group_name = group_name) - baseline_prediction
        pred_list[[i]] <- baseline[, c("rowid", variable, "predicted")]
    }
    pred <- do.call("rbind", pred_list)

    # two possible label formats for factor level coefficients: factor(cyl)4 vs. cyl4
    levs <- levels(baseline[[variable]])
    levs <- levs[2:length(levs)]
    lab_fmt1 <- sprintf("factor(%s)%s", variable, levs)
    lab_fmt2 <- sprintf("%s%s", variable, levs)
    if (all(lab_fmt1 %in% names(get_coef(model)))) {
        pred$term <- sprintf("factor(%s)%s", variable, pred[[variable]])
    } else if (all(lab_fmt2 %in% names(get_coef(model)))) {
        pred$term <- sprintf("%s%s", variable, pred[[variable]])
    } else {
        pred$term <- pred[[variable]]
    }
    pred <- pred[, c("rowid", "term", "predicted")]
    colnames(pred) <- c("rowid", "term", "contrast")
    row.names(pred) <- NULL
    return(pred)
}


get_contrasts_character <- function(model,
                                    baseline,
                                    variable,
                                    group_name = NULL,
                                    type = "response",
                                    ...) {
    pred_list <- list()
    levs <- unique(baseline[[variable]])
    baseline[[variable]] <- levs[1]
    baseline_prediction <- get_predict(model,
                                       newdata = baseline,
                                       type = type,
                                       group_name = group_name)
    for (i in 2:length(levs)) {
        baseline[[variable]] <- levs[i]
        baseline$predicted <- get_predict(model = model,
                                            newdata = baseline,
                                            type = type,
                                            group_name = group_name) - baseline_prediction
        baseline$term <- sprintf("%s%s", variable, baseline[[variable]])
        pred_list[[i]] <- baseline[, c("rowid", "term", "predicted")]
    }
    pred <- do.call("rbind", pred_list)
    colnames(pred) <- c("rowid", "term", "contrast")
    row.names(pred) <- NULL
    return(pred)
}


get_contrasts_numeric <- function(model,
                                  baseline,
                                  variable,
                                  group_name = NULL,
                                  type = "response",
                                  step_size = 1,
                                  normalize_dydx = FALSE,
                                  ...) {

    # term reveals the increment size, which is analogous to level for factor/character/logical variables
    if (!"term" %in% colnames(baseline)) {
        baseline$term <- sprintf("%s + %s", variable, step_size)
    }

    pred_baseline <- get_predict(model,
                                 newdata = baseline,
                                 type = type,
                                 group_name = group_name)

    baseline[[variable]] <- baseline[[variable]] + step_size
    pred_increment <- get_predict(model,
                                  newdata = baseline,
                                  type = type,
                                  group_name = group_name)

    baseline[["contrast"]] <- as.vector(pred_increment) - as.vector(pred_baseline)
    out <- baseline

    # bayes: posterior draws and credible intervals
    if ("posterior_draws" %in% names(attributes(pred_increment))) {
        draws <- attr(pred_increment, "posterior_draws") - attr(pred_baseline, "posterior_draws")
        ci <- apply(draws, 1, quantile, prob = c(.025, .975))
        out[["conf.low"]] <- ci[1, ]
        out[["conf.high"]] <- ci[2, ]
        attr(out, "contrast_draws") <- draws
    }

    return(out)
}
