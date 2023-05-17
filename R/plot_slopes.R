#' Plot Conditional or Marginal Slopes
#'
#' @description
#' Plot slopes on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).
#'
#' The `by` argument is used to plot marginal slopes, that is, slopes made on the original data, but averaged by subgroups. This is analogous to using the `by` argument in the `slopes()` function.
#'
#' The `condition` argument is used to plot conditional slopes, that is, slopes made on a user-specified grid. This is analogous to using the `newdata` argument and `datagrid()` function in a `slopes()` call.
#' 
#' All unspecified variables are held at their mean or mode. This includes grouping variables in mixed-effects models, so analysts who fit such models may want to specify the groups of interest using the `variables` argument, or supply model-specific arguments to compute population-level estimates. See details below.

#' See the "Plots" vignette and website for tutorials and information on how to customize plots:
#'
#' * https://vincentarelbundock.github.io/marginaleffects/articles/plot.html
#' * https://vincentarelbundock.github.io/marginaleffects
#' 
#' @param variables Name of the variable whose marginal effect (slope) we want to plot on the y-axis.
#' @param condition Conditional slopes
#' + Character vector (max length 3): Names of the predictors to display.
#' + Named list (max length 3): List names correspond to predictors. List elements can be:
#'   - Numeric vector
#'   - Function which returns a numeric vector or a set of unique categorical values 
#'   - Shortcut strings for common reference values: "minmax", "quartile", "threenum"
#' + 1: x-axis. 2: color/shape. 3: facets.
#' + Numeric variables in positions 2 and 3 are summarized by Tukey's five numbers `?stats::fivenum`.
#' @param rug TRUE displays tick marks on the axes to mark the distribution of raw data.
#' @param gray FALSE grayscale or color plot
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @param newdata When `newdata` is `NULL`, the grid is determined by the `condition` argument. When `newdata` is not `NULL`, the argument behaves in the same way as in the `slopes()` function.
#' @inheritParams slopes
#' @template model_specific_arguments
#' @return A `ggplot2` object
#' @export
#' @examples
#' library(marginaleffects)
#' mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
#' 
#' plot_slopes(mod, variables = "hp", condition = "drat")
#'
#' plot_slopes(mod, variables = "hp", condition = c("drat", "am"))
#' 
#' plot_slopes(mod, variables = "hp", condition = list("am", "drat" = 3:5))
#' 
#' plot_slopes(mod, variables = "am", condition = list("hp", "drat" = range))
#'
#' plot_slopes(mod, variables = "am", condition = list("hp", "drat" = "threenum"))
#' 
plot_slopes <- function(model,
                        variables = NULL,
                        condition = NULL,
                        by = NULL,
                        newdata = NULL,
                        type = "response",
                        vcov = NULL,
                        conf_level = 0.95,
                        wts = NULL,
                        slope = "dydx",
                        rug = FALSE,
                        gray = FALSE,
                        draw = TRUE,
                        ...) {

    dots <- list(...)
    if ("effect" %in% names(dots)) {
        if (is.null(variables)) {
            variables <- dots[["effect"]]
        } else {
            insight::format_error("The `effect` argument has been renamed to `variables`.")
        }
    }

    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    # should probably not be nested too deeply in the call stack since we eval.parent() (not sure about this)
    scall <- substitute(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, model)

    valid <- c("dydx", "eyex", "eydx", "dyex")
    checkmate::assert_choice(slope, choices = valid)

    out <- plot_comparisons(
        model,
        variables = variables,
        condition = condition,
        by = by,
        newdata = newdata,
        type = type,
        vcov = vcov,
        conf_level = conf_level,
        wts = wts,
        draw = draw,
        rug = rug,
        gray = gray,
        comparison = slope,
        ...)

    if (inherits(out, "ggplot")) {
        out <- out + ggplot2::labs(x = condition[1], y = "Slope")
    }

    return(out)
}


#' `plot_slopes()` is an alias to `plot_slopes()`
#'
#' This alias is kept for backward compatibility.
#' @inherit plot_predictions
#' @keywords internal
#' @export
plot_cme <- plot_slopes