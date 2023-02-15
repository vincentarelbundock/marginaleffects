#' Plot Conditional or Marginal Slopes
#'
#' @description
#' Plot slopes on the y-axis against values of one or more predictors (x-axis, colors, line types, and facets).
#'
#' The `by` argument is used to plot marginal slopes, that is, slopes made on the original data, but averaged by subgroups. This is analogous to using the `by` argument in the `slopes()` function.
#'
#' The `condition` argument is used to plot conditional slopes, that is, slopes made on a user-specified grid. This is analogous to using the `newdata` argument and `datagrid()` function in a `slopes()` call. Unspecified variables are held at their mean or mode.
#' 
#' See the "Plots" vignette and website for tutorials and information on how to customize plots:
#'
#' * https://vincentarelbundock.github.io/marginaleffects/articles/plot.html
#' * https://vincentarelbundock.github.io/marginaleffects
#' 
#' @param x a model or object produced by the `slopes()` or `comparisons()` functions.
#' @param variables Name of the variable whose marginal effect (slope) we want to plot on the y-axis.
#' @param condition character vector or named list of length smaller than 3. Character vectors must be the names of the predictor variables to display. The names of the list must The first element is displayed on the x-axis. The second element determines the colors. The third element creates facets. Unspecified variables are held at their means or modes. Lists can include these types of values (see Examples section below):
#' * Numeric vector
#' * Function which returns a numeric vector or a set of unique categorical values 
#' * Shortcut strings for common reference values: "minmax", "quartile", "threenum"
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams slopes
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
                        type = "response",
                        vcov = NULL,
                        conf_level = 0.95,
                        slope = "dydx",
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

    valid <- c("dydx", "eyex", "eydx", "dyex")
    checkmate::assert_choice(slope, choices = valid)

    out <- plot_comparisons(
        model,
        variables = variables,
        condition = condition,
        by = by,
        type = type,
        vcov = vcov,
        conf_level = conf_level,
        draw = draw,
        transform_pre = slope,
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