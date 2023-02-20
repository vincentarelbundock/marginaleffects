#' Plot Conditional or Marginal Comparisons
#'
#' @description
#' Plot comparisons on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).
#'
#' The `by` argument is used to plot marginal comparisons, that is, comparisons made on the original data, but averaged by subgroups. This is analogous to using the `by` argument in the `comparisons()` function.
#'
#' The `condition` argument is used to plot conditional comparisons, that is, comparisons made on a user-specified grid. This is analogous to using the `newdata` argument and `datagrid()` function in a `comparisons()` call. Unspecified variables are held at their mean or mode.
#' 
#' See the "Plots" vignette and website for tutorials and information on how to customize plots:
#'
#' * https://vincentarelbundock.github.io/marginaleffects/articles/plot.html
#' * https://vincentarelbundock.github.io/marginaleffects
#' 
#' @param variables Name of the variable whose contrast we want to plot on the y-axis.
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams comparisons
#' @inheritParams plot_slopes
#' @inheritParams slopes
#' @return A `ggplot2` object
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
#' 
#' plot_comparisons(mod, variables = "hp", condition = "drat")
#'
#' plot_comparisons(mod, variables = "hp", condition = c("drat", "am"))
#' 
#' plot_comparisons(mod, variables = "hp", condition = list("am", "drat" = 3:5))
#' 
#' plot_comparisons(mod, variables = "am", condition = list("hp", "drat" = range))
#' 
#' plot_comparisons(mod, variables = "am", condition = list("hp", "drat" = "threenum"))
plot_comparisons <- function(model,
                             variables = NULL,
                             condition = NULL,
                             by = NULL,
                             type = "response",
                             vcov = NULL,
                             conf_level = 0.95,
                             transform_pre = "difference",
                             transform_post = NULL,
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
    
    # sanity check
    checkmate::assert(
        checkmate::check_character(variables, names = "unnamed"),
        checkmate::check_list(variables, names = "unique"),
        .var.name = "variables")

    checkmate::assert_character(by, null.ok = TRUE, max.len = 3, min.len = 1, names = "unnamed")
    if ((!is.null(condition) && !is.null(by)) || (is.null(condition) && is.null(by))) {
        msg <- "One of the `condition` and `by` arguments must be supplied, but not both."
        insight::format_error(msg)
    }

    # conditional
    if (!is.null(condition)) {
        modeldata <- get_modeldata(model, additional_variables = names(condition$condition))
        condition <- sanitize_condition(model, condition, variables, modeldata = modeldata)
        v_x <- condition$condition1
        v_color <- condition$condition2
        v_facet <- condition$condition3
        datplot <- comparisons(
            model,
            newdata = condition$newdata,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            by = FALSE,
            wts = NULL,
            variables = variables,
            transform_pre = transform_pre,
            transform_post = transform_post,
            cross = FALSE,
            modeldata = modeldata,
            ...)
    }

    # marginal
    if (!is.null(by)) {
        modeldata <- get_modeldata(model, additional_variables = by)
        datplot <- comparisons(
            model,
            by = by,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            variables = variables,
            wts = NULL,
            transform_pre = transform_pre,
            transform_post = transform_post,
            cross = FALSE,
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
    p <- plot_build(datplot, v_x = v_x, v_color = v_color, v_facet = v_facet, gray = gray, rug = rug, modeldata = modeldata)
    p <- p + ggplot2::labs(x = v_x, y = sprintf("Comparison"))

    return(p)
}


#' `plot_comparisons()` is an alias to `plot_comparisons()`
#'
#' This alias is kept for backward compatibility.
#' @inherit plot_predictions
#' @keywords internal
#' @export
plot_cco <- plot_comparisons