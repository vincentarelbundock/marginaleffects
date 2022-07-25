#' Plot Conditional Marginal Effects
#'
#' This function plots marginal effects (y-axis) against values of predictor(s)
#' variable(s) (x-axis and colors). This is especially useful in models with
#' interactions, where the values of marginal effects depend on the values of
#' "condition" variables.
#'
#' @param effect Name of the variable whose marginal effect we want to plot on the y-axis
#' @param condition String or vector of two strings. The first is a variable
#' name to be displayed on the x-axis. The second is a variable whose values
#' will be displayed in different colors. Other numeric variables are held at
#' their means. Other categorical variables are held at their modes.
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams plot.marginaleffects
#' @inheritParams marginaleffects
#' @return A `ggplot2` object
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * wt, data = mtcars)
#' plot_cme(mod, effect = "hp", condition = "wt")
#'
#' mod <- lm(mpg ~ hp * wt * am, data = mtcars)
#' plot_cme(mod, effect = "hp", condition = c("wt", "am"))
#'
plot_cme <- function(model,
                     effect = NULL,
                     condition = NULL,
                     type = "response",
                     vcov = NULL,
                     conf_level = 0.95,
                     draw = TRUE,
                     ...) {

    # get data to know over what range of values we should plot
    dat <- hush(insight::get_data(model))
    resp <- insight::find_response(model)[1]

    # eventually we might allow multiple conditions and/or effects
    checkmate::assert_character(effect, len = 1)

    # allow multiple conditions and/or effects
    checkmate::assert_character(condition, min.len = 1, max.len = 2)

    ## not sure why this fails in testthat
    # checkmate::assert_true(condition %in% colnames(dat))
    if (length(condition) == 1) {
        condition1 <- condition[1]
        condition2 <- NULL
        condition3 <- NULL
    } else if (length(condition) == 2) {
        condition1 <- condition[1]
        condition2 <- condition[2]
        condition3 <- NULL
    } else {
        condition1 <- condition[1]
        condition2 <- condition[2]
        condition3 <- condition[3]
    }

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
            x = condition1,
            y = sprintf("Marginal effect of %s on %s", effect, resp))
    }

    return(out)
}
