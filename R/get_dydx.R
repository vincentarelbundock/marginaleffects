#' Compute marginal effect for of single regressor, for a single prediction type
#' @noRd
get_dydx <- function(model,
                     variable,
                     newdata,
                     type,
                     ...) {

    if (variable %in% find_categorical(newdata) || isTRUE(attr(newdata[[variable]], "factor"))) {
        dydx_fun <- get_contrasts
    } else if (inherits(model, "brmsfit") || inherits(model, "stanreg")) {
        dydx_fun <- get_dydx_via_contrasts
    } else {
        dydx_fun <- get_dydx_continuous
    }

    out <- dydx_fun(model = model,
                    newdata = newdata,
                    variable = variable,
                    type = type,
                    normalize_dydx = TRUE,
                    ...)

    # normalize names to merge when requesting dydx
    if (all(c("contrast", "estimate") %in% colnames(out))) {
        colnames(out)[colnames(out) == "estimate"] <- "dydx"
    }

    return(out)
}


#' Compute marginal effect for a continuous regressor
#' @noRd
get_dydx_continuous <- function(model,
                                variable,
                                newdata = insight::get_data(model),
                                type = "response",
                                normalize_dydx = NULL, # do not push to ...
                                vcov = NULL, # do not push to ...
                                step_size = NULL, # do not push to ...
                                ...) {

    # we need to loop over group names because the input and output of grad()
    # must be of the same dimensions. This is inefficient with
    # grouped/categorical outcomes, but VAB cannot currently think of a good
    # way to avoid this.
    group_names <- get_group_names(model, type = type)

    numDeriv_method <- sanitize_numDeriv_method()

    out_list <- list()
    for (gn in group_names) {
        newdata_tmp <- newdata
        inner <- function(x) {
            newdata_tmp[[variable]] <- x

            # some predict methods raise warnings on unused arguments
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
        gr <- get_gradient(func = inner, x = newdata[[variable]])
        out_list[[gn]] <- data.frame(rowid = 1:nrow(newdata),
                                     group = gn,
                                     term = variable,
                                     dydx = gr)
    }
    out <- bind_rows(out_list)
    return(out)
}


#' In some cases (e.g., Bayesian models) the automatic differentiation approach
#' with `numDeriv` does not apply straightforwardly. We use the `get_contrasts`
#' function with a small step to get a very small contrast. Then normalize by
#' dividing by the step via the `normalize_dydx` argument.
#' @noRd
get_dydx_via_contrasts <- function(model,
                                   newdata,
                                   variable,
                                   type = "response",
                                   normalize_dydx = TRUE,
                                   ...) {
    out <- get_contrasts(model = model,
                  newdata = newdata,
                  variable = variable,
                  type = type,
                  step_size = 1e-5,
                  normalize_dydx = normalize_dydx,
                  return_data = FALSE,
                  ...)
    return(out)
}

