get_dydx <- function(model,
                     variable,
                     newdata,
                     type,
                     numDeriv_method,
                     ...) {

    # if there are no categorical variables in `newdata`, check the terms to
    # find transformation and warn accordingly.
    categorical_variables <- find_categorical(newdata)
    if (length(categorical_variables) == 0) {
        termlabs <- try(attr(stats::terms(model), "term.labels"), silent = TRUE)
        termlabs <- try(any(grepl("^factor\\(|^as.factor\\(|^as.logical\\(", termlabs)), silent = TRUE)
        if (isTRUE(termlabs)) {
            warning("When using `marginaleffects`, it is safer to convert variables to factors or logicals in the dataset *before* fitting the model, rather than by wrapping terms in `factor()` or `as.logical() in the model formula.")
        }
    }


    if (variable %in% find_categorical(newdata) || isTRUE(attr(newdata[[variable]], "factor"))) {
        dydx_fun <- get_contrasts
    } else if (inherits(model, "brmsfit") || inherits(model, "stanreg")) {
        dydx_fun <- get_dydx_via_contrasts
    } else {
        dydx_fun <- get_dydx_continuous
    }

    out <- dydx_fun(model = model,
                    newdata = newdata,
                    v = variable,
                    type = type,
                    numDeriv_method = numDeriv_method,
                    contrast_to_dydx = TRUE,
                    ...)

    # normalize names to merge when requesting dydx
    colnames(out)[colnames(out) == "contrast"] <- "dydx"

    return(out)
}

get_dydx_continuous <- function(model,
                                variable,
                                newdata = insight::get_data(model),
                                type = "response",
                                numDeriv_method = "simple",
                                ...) {

    # we need to loop over group names because the input and output of grad()
    # must be of the same dimensions. This is inefficient with
    # grouped/categorical outcomes, but VAB cannot currently think of a good
    # way to avoid this.
    group_names <- get_group_names(model, type = type)

    out_list <- list()
    for (gn in group_names) {
        newdata_tmp <- newdata
        inner <- function(x) {
            newdata_tmp[[variable]] <- x
            tmp <- get_predict(model = model,
                               newdata = newdata_tmp,
                               type = type,
                               ...)
            if (gn != "main_marginaleffect") {
                tmp$predicted[tmp$group == gn]
            } else {
                tmp$predicted
            }
        }
        gr <- numDeriv::grad(func = inner,
                             x = newdata[[variable]],
                             method = numDeriv_method)
        out_list[[gn]] <- data.frame(rowid = 1:nrow(newdata),
                                     group = gn,
                                     term = variable,
                                     dydx = gr)
    }
    out <- bind_rows(out_list)
    return(out)
}


get_dydx_via_contrasts <- function(model,
                                   newdata,
                                   variable,
                                   group_name = NULL,
                                   type = "response",
                                   ...) {
    out <- get_contrasts(model = model,
                  newdata = newdata,
                  variable = variable,
                  group_name = group_name,
                  type = type,
                  step_size = 1e-5,
                  normalize_dydx = TRUE,
                  return_data = FALSE,
                  ...)
    return(out)
}
