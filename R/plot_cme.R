#' Plot Conditional Marginal Effects
#'
#' This function plots marginal effects (y-axis) against values of predictor(s)
#' variable(s) (x-axis and colors). This is especially useful in models with
#' interactions, where the values of marginal effects depend on the values of
#' "condition" variables.
#'
#' @param effect Name of the variable whose marginal effect we want to plot on the y-axis
#' @param condition character vector or named list of length smaller than 3. Character vectors must be the names of the predictor variables to display. The names of the list must The first element is displayed on the x-axis. The second element determines the colors. The third element creates facets. Unspecified variables are held at their means or modes. Lists can include these types of values (see Examples section below):
#' * Numeric vector
#' * Function which returns a numeric vector or a set of unique categorical values 
#' * Shortcut strings for common reference values: "minmax", "quartile", "threenum"
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams plot.marginaleffects
#' @inheritParams marginaleffects
#' @return A `ggplot2` object
#' @family plot
#' @export
#' @examples
#' library(marginaleffects)
#' mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
#' 
#' plot_cme(mod, effect = "hp", condition = "drat")
#'
#' plot_cme(mod, effect = "hp", condition = c("drat", "am"))
#' 
#' plot_cme(mod, effect = "hp", condition = list("am", "drat" = 3:5))
#' 
#' plot_cme(mod, effect = "am", condition = list("hp", "drat" = range))
#'
#' plot_cme(mod, effect = "am", condition = list("hp", "drat" = "threenum"))
#' 
plot_cme <- function(model,
                     effect = NULL,
                     condition = NULL,
                     type = "response",
                     vcov = NULL,
                     conf_level = 0.95,
                     draw = TRUE,
                     ...) {

    out <- plot_cco(
        model = model,
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
            y = sprintf("Marginal effect of %s on %s", effect, insight::find_response(model)[[1]]))
    }

    return(out)
}
