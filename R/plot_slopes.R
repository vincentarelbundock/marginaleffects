#' Plot Slopes
#'
#' Plot slopes (aka marginal effects or trends) on the y-axis against values of one or more predictors (x-axis, colors, and facets). Plot average slopes.
#'
#' @param x a model or object produced by the `slopes()` or `comparisons()` functions.
#' @param effect Name of the variable whose contrast we want to plot on the y-axis. If `NULL`, a plot of average slopes is returned.
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
#' plot_slopes(mod, effect = "hp", condition = "drat")
#'
#' plot_slopes(mod, effect = "hp", condition = c("drat", "am"))
#' 
#' plot_slopes(mod, effect = "hp", condition = list("am", "drat" = 3:5))
#' 
#' plot_slopes(mod, effect = "am", condition = list("hp", "drat" = range))
#'
#' plot_slopes(mod, effect = "am", condition = list("hp", "drat" = "threenum"))
#' 
plot_slopes <- function(x,
                        effect = NULL,
                        condition = NULL,
                        type = "response",
                        vcov = NULL,
                        conf_level = 0.95,
                        draw = TRUE,
                        ...) {

    if (is.null(effect) && is.null(condition) && !inherits(x, "slopes")) {
        model <- slopes(
            x,
            by = TRUE,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            ...)
    }

    out <- plot_comparisons(
        x,
        effect = effect,
        condition = condition,
        type = type,
        vcov = vcov,
        conf_level = conf_level,
        draw = draw,
        transform_pre = "dydx",
        ...)

    if (inherits(out, "ggplot")) {
        out <- out + ggplot2::labs(
            x = condition[1],
            y = sprintf("Marginal effect of %s on %s", effect, insight::find_response(x)[[1]]))
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



################### Backward compatibility for deprecated methods. Also nice to keep.
#' @export
#' @noRd
plot.slopes <- plot_slopes