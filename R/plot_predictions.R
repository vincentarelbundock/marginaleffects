#' Plot Predictions
#'
#' Plot predictions on the y-axis against values of on or more predictors (x-axis, colors, and facets). Plot average predictions.
#'
#' @param condition character vector or named list of length smaller than 4. Character vectors must be the names of the predictor variables to display. The names of the list must The first element is displayed on the x-axis. The second element determines the colors. The third element creates facets. Other variables are held at their means or modes. Lists can include these types of values:
#' * Numeric vector
#' * Function which returns a numeric vector or a set of unique categorical values 
#' * Shortcut strings for common reference values: "minmax", "quartile", "threenum"
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams plot_slopes
#' @inheritParams predictions
#' @return A `ggplot2` object or data frame (if `draw=FALSE`)
#' @export
#' @examples
#' mod <- lm(mpg ~ hp + wt, data = mtcars)
#' plot_predictions(mod, condition = "wt")
#'
#' mod <- lm(mpg ~ hp * wt * am, data = mtcars)
#' plot_predictions(mod, condition = c("hp", "wt"))
#'
#' plot_predictions(mod, condition = list("hp", wt = "threenum"))
#' 
#' plot_predictions(mod, condition = list("hp", wt = range))
#'
plot_predictions <- function(model,
                             condition = NULL,
                             by = NULL,
                             type = NULL,
                             vcov = NULL,
                             conf_level = 0.95,
                             transform_post = NULL,
                             draw = TRUE,
                             ...) {

    # sanity check
    checkmate::assert_character(by, null.ok = TRUE, max.len = 3, min.len = 1, names = "unnamed")
    if ((!is.null(condition) && !is.null(by)) || (is.null(condition) && is.null(by))) {
        msg <- "One of the `condition` and `by` arguments must be supplied, but not both."
        insight::format_error(msg)
    }
    if (is.null(by)) by <- FALSE

    # conditional
    if (!is.null(condition)) {
        modeldata <- get_modeldata(model, additional_variables = names(condition$condition))
        condition <- sanitize_condition(model, condition, effect = NULL, modeldata = modeldata)
        v_x <- condition$condition1
        v_color <- condition$condition2
        v_facet <- condition$condition3
        datplot <- predictions(
            model,
            newdata = condition$newdata,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            transform_post = transform_post,
            modeldata = modeldata,
            ...)
    }

    # marginal
    if (!isFALSE(by)) { # switched from NULL above
        condition <- NULL
        modeldata <- get_modeldata(model, additional_variables = by)
        datplot <- predictions(
            model,
            by = by,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            wts = NULL,
            transform_post = transform_post,
            modeldata = modeldata,
            ...)
        v_x <- by[[1]]
        v_color <- hush(by[[2]])
        v_facet <- hush(by[[3]])
    }

    datplot <- plot_preprocess(datplot, v_x = v_x, v_color = v_color, v_facet = v_facet, condition = condition, modeldata = modeldata)

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        out <- as.data.frame(datplot)
        attr(out, "posterior_draws") <- attr(datplot, "posterior_draws")
        return(out)
    }

    # ggplot2
    insight::check_if_installed("ggplot2")
    p <- plot_build(datplot, v_x = v_x, v_color = v_color, v_facet = v_facet)
    
    p <- p + ggplot2::labs(
        x = v_x,
        y = unlist(insight::find_response(model, combine = TRUE), use.names = FALSE)[1],
        color = v_color,
        fill = v_color,
        linetype = v_color)

    # condition 3: facets
    if (!is.null(v_facet)) {
        fo <- as.formula(paste("~", v_facet))
        p <- p + ggplot2::facet_wrap(fo)
    }


    # attach model data for each of use
    attr(p, "modeldata") <- modeldata

    return(p)
}




#' `plot_predictions()` is an alias to `plot_predictions()`
#'
#' This alias is kept for backward compatibility.
#' @inherit plot_predictions
#' @keywords internal
#' @export
plot_cap <- plot_predictions