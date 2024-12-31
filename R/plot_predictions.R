#' Plot Conditional or Marginal Predictions
#'
#' @description
#' Plot predictions on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).
#'
#' The `by` argument is used to plot marginal predictions, that is, predictions made on the original data, but averaged by subgroups. This is analogous to using the `by` argument in the `predictions()` function.
#'
#' The `condition` argument is used to plot conditional predictions, that is, predictions made on a user-specified grid. This is analogous to using the `newdata` argument and `datagrid()` function in a `predictions()` call. All variables whose values are not specified explicitly are treated as usual by `datagrid()`, that is, they are held at their mean or mode (or rounded mean for integers). This includes grouping variables in mixed-effects models, so analysts who fit such models may want to specify the groups of interest using the `condition` argument, or supply model-specific arguments to compute population-level estimates. See details below.
#'
#' See the "Plots" vignette and website for tutorials and information on how to customize plots:
#'
#' * https://marginaleffects.com/bonus/plot.html
#' * https://marginaleffects.com
#'
#' @param condition Conditional predictions
#' + Character vector (max length 4): Names of the predictors to display.
#' + Named list (max length 4): List names correspond to predictors. List elements can be:
#'   - Numeric vector
#'   - Function which returns a numeric vector or a set of unique categorical values
#'   - Shortcut strings for common reference values: "minmax", "quartile", "threenum"
#' + 1: x-axis. 2: color/shape. 3: facet (wrap if no fourth variable, otherwise cols of grid). 4: facet (rows of grid).
#' + Numeric variables in positions 2 and 3 are summarized by Tukey's five numbers `?stats::fivenum`
#' @param by Marginal predictions
#' + Character vector (max length 3): Names of the categorical predictors to marginalize across.
#' + 1: x-axis. 2: color. 3: facets.
#' @param newdata When `newdata` is `NULL`, the grid is determined by the `condition` argument. When `newdata` is not `NULL`, the argument behaves in the same way as in the `predictions()` function.
#' @param points Number between 0 and 1 which controls the transparency of raw data points. 0 (default) does not display any points. Warning: The points displayed are raw data, so the resulting plot is not a "partial residual plot."
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams plot_slopes
#' @inheritParams predictions
#' @template model_specific_arguments
#' @template type
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
#' # marginal predictions
#' mod <- lm(mpg ~ hp * am, data = mtcars)
#' plot_predictions(mod, by = "am") 
#'
#' # marginal predictions on a counterfactual grid
#' plot_predictions(mod, by = "am",
#'    newdata = datagrid(am = 0:1, grid_type = "counterfactual")) 
#'
plot_predictions <- function(model,
                             condition = NULL,
                             by = NULL,
                             newdata = NULL,
                             type = NULL,
                             vcov = NULL,
                             conf_level = 0.95,
                             wts = FALSE,
                             transform = NULL,
                             points = 0,
                             rug = FALSE,
                             gray = getOption("marginaleffects_plot_gray", default = FALSE),
                             draw = TRUE,
                             ...) {
  dots <- list(...)

  checkmate::assert_number(points, lower = 0, upper = 1)

  if ("variables" %in% names(dots)) {
    insight::format_error("The `variables` argument is not supported by this function.")
  }
  if ("effect" %in% names(dots)) {
    insight::format_error("The `effect` argument is not supported by this function.")
  }
  if ("transform_post" %in% names(dots)) { # backward compatibility
    transform <- dots[["transform_post"]]
  }

  if (inherits(model, "mira") && is.null(newdata)) {
    msg <- "Please supply a data frame to the `newdata` argument explicitly."
    insight::format_error(msg)
  }

  # order of the first few paragraphs is important
  scall <- rlang::enquo(newdata)
  newdata <- sanitize_newdata_call(scall, newdata, model, by = by)
  if (!isFALSE(wts) && is.null(by)) {
    insight::format_error("The `wts` argument requires a `by` argument.")
  }
  checkmate::assert_character(by, null.ok = TRUE)

  # sanity check
  checkmate::assert_character(by, null.ok = TRUE, max.len = 4, min.len = 1, names = "unnamed")
  if ((!is.null(condition) && !is.null(by)) || (is.null(condition) && is.null(by))) {
    msg <- "One of the `condition` and `by` arguments must be supplied, but not both."
    insight::format_error(msg)
  }

  modeldata <- get_modeldata(
    model,
    additional_variables = c(names(condition), by),
    wts = wts)

  # mlr3 and tidymodels
  if (is.null(modeldata) || nrow(modeldata) == 0) {
    modeldata <- newdata
  }

  # conditional
  if (!is.null(condition)) {
    condition <- sanitize_condition(model, condition, variables = NULL, modeldata = modeldata)
    v_x <- condition$condition1
    v_color <- condition$condition2
    v_facet_1 <- condition$condition3
    v_facet_2 <- condition$condition4
    datplot <- predictions(
      model,
      newdata = condition$newdata,
      type = type,
      vcov = vcov,
      conf_level = conf_level,
      transform = transform,
      modeldata = modeldata,
      wts = wts,
      ...)
  }

  # marginal
  if (!isFALSE(by) && !is.null(by)) { # switched from NULL above
    condition <- NULL

    newdata <- sanitize_newdata(
      model = model,
      newdata = newdata,
      modeldata = modeldata,
      by = by,
      wts = wts)

    # tidymodels & mlr3
    if (is.null(modeldata)) {
      modeldata <- newdata
    }

    datplot <- predictions(
      model,
      by = by,
      type = type,
      vcov = vcov,
      conf_level = conf_level,
      wts = wts,
      transform = transform,
      newdata = newdata,
      modeldata = modeldata,
      ...)
    v_x <- by[[1]]
    v_color <- hush(by[[2]])
    v_facet_1 <- hush(by[[3]])
    v_facet_2 <- hush(by[[4]])
  }

  dv <- unlist(insight::find_response(model, combine = TRUE), use.names = FALSE)[1]
  datplot <- plot_preprocess(datplot, v_x = v_x, v_color = v_color, v_facet_1 = v_facet_1, v_facet_2 = v_facet_2, condition = condition, modeldata = modeldata)

  # return immediately if the user doesn't want a plot
  if (isFALSE(draw)) {
    out <- as.data.frame(datplot)
    attr(out, "posterior_draws") <- attr(datplot, "posterior_draws")
    return(out)
  }

  # ggplot2
  insight::check_if_installed("ggplot2")
  p <- plot_build(datplot,
    v_x = v_x,
    v_color = v_color,
    v_facet_1 = v_facet_1,
    v_facet_2 = v_facet_2,
    points = points,
    modeldata = modeldata,
    dv = dv,
    rug = rug,
    gray = gray)

  p <- p + ggplot2::labs(
    x = v_x,
    y = dv,
    color = v_color,
    fill = v_color,
    linetype = v_color)

  # attach model data for each of use
  attr(p, "modeldata") <- modeldata

  return(p)
}
