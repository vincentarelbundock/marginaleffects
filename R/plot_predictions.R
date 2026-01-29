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
#' @param condition Conditional predictions.
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
#' @param newdata When `newdata` is `NULL`, the grid is determined by the `condition` argument. When `newdata` is not `NULL`, the argument behaves in the same way as in the `predictions()` function. Note that the `condition` argument builds its own grid, so the `newdata` argument is ignored if the `condition` argument is supplied.
#' @param points Number between 0 and 1 which controls the transparency of raw data points. 0 (default) does not display any points. Warning: The points displayed are raw data, so the resulting plot is not a "partial residual plot."
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams plot_slopes
#' @inheritParams predictions
#' @template model_specific_arguments
#' @template type
#' @return A `ggplot2` object or data frame (if `draw=FALSE`)
#' @export
#' @examplesIf interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")
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
#' plot_predictions(mod,
#'     by = "am",
#'     newdata = datagrid(am = 0:1, grid_type = "counterfactual")
#' )
#'
plot_predictions <- function(
  model,
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
  ...
) {
    checkmate::assert_number(points, lower = 0, upper = 1)

    if (inherits(model, c("mira", "amest"))) {
        msg <- "This function does not support multiple imputation. Call `predictions()` or `avg_predictions()` instead. These functions return easy to plot data frames."
        stop_sprintf(msg)
    }


    # init
    call <- construct_call(model, "comparisons")
    model <- sanitize_model(model, call = call, newdata = newdata, wts = wts, vcov = vcov, by = by, ...)
    mfx <- new_marginaleffects_internal(
        call = call,
        model = model
    )

    if (inherits(mfx@model, "mira") && is.null(newdata)) {
        msg <- "Please supply a data frame to the `newdata` argument explicitly."
        stop_sprintf(msg)
    }

    # order of the first few paragraphs is important
    scall <- rlang::enquo(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, mfx = mfx, by = by)
    if (!isFALSE(wts) && is.null(by)) {
        stop_sprintf("The `wts` argument requires a `by` argument.")
    }
    checkmate::assert_character(by, null.ok = TRUE)

    # sanity check
    checkmate::assert_character(
        by,
        null.ok = TRUE,
        max.len = 4,
        min.len = 1,
        names = "unnamed"
    )
    if ((!is.null(condition) && !is.null(by)) || (is.null(condition) && is.null(by))) {
        msg <- "One of the `condition` and `by` arguments must be supplied, but not both."
        stop_sprintf(msg)
    }

    # mlr3 and tidymodels
    if (is.null(mfx@modeldata) || nrow(mfx@modeldata) == 0) {
        mfx@modeldata <- newdata
    }

    # conditional
    if (!is.null(condition)) {
        condition <- sanitize_condition(
            mfx = mfx,
            condition = condition,
            variables = NULL
        )
        v_x <- condition$condition1
        v_color <- condition$condition2
        v_facet_1 <- condition$condition3
        v_facet_2 <- condition$condition4
        datplot <- predictions(
            mfx@model,
            newdata = condition$newdata,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            transform = transform,
            modeldata = mfx@modeldata,
            wts = wts,
            ...
        )
    }

    # marginal
    if (!isFALSE(by) && !is.null(by)) {
        # switched from NULL above
        condition <- NULL

        newdata <- sanitize_newdata(
            mfx = mfx,
            newdata = newdata,
            by = by,
            wts = wts
        )

        # tidymodels & mlr3
        if (is.null(mfx@modeldata)) {
            mfx@modeldata <- newdata
        }

        datplot <- predictions(
            mfx@model,
            by = by,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            wts = wts,
            transform = transform,
            newdata = newdata,
            modeldata = mfx@modeldata,
            ...
        )
        v_x <- by[[1]]
        v_color <- hush(by[[2]])
        v_facet_1 <- hush(by[[3]])
        v_facet_2 <- hush(by[[4]])
    }

    datplot <- plot_preprocess(
        datplot,
        v_x = v_x,
        v_color = v_color,
        v_facet_1 = v_facet_1,
        v_facet_2 = v_facet_2,
        condition = condition,
        mfx = mfx
    )

    dv <- mfx@variable_names_response
    dv_label <- dv
    if (!is.null(mfx@modeldata) && length(dv) == 1 && dv %in% colnames(mfx@modeldata)) {
        dv_label <- attr(mfx@modeldata[[dv]], "label")
    } else {
        dv_label <- "Prediction"
    }
    attr(datplot$estimate, "label") <- dv_label

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        out <- as.data.frame(datplot)
        return(out)
    }

    # ggplot2
    insight::check_if_installed("ggplot2", minimum_version = "4.0.0")
    p <- plot_build(
        datplot,
        v_x = v_x,
        v_color = v_color,
        v_facet_1 = v_facet_1,
        v_facet_2 = v_facet_2,
        points = points,
        dv = dv,
        rug = rug,
        gray = gray,
        mfx = mfx
    )

    return(p)
}
