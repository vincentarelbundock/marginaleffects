#' Plot Conditional or Marginal Comparisons
#'
#' @description
#' Plot comparisons on the y-axis against values of one or more predictors (x-axis, colors/shapes, and facets).
#'
#' The `by` argument is used to plot marginal comparisons, that is, comparisons made on the original data, but averaged by subgroups. This is analogous to using the `by` argument in the `comparisons()` function.
#'
#' The `condition` argument is used to plot conditional comparisons, that is, comparisons made on a user-specified grid. This is analogous to using the `newdata` argument and `datagrid()` function in a `comparisons()` call. All variables whose values are not specified explicitly are treated as usual by `datagrid()`, that is, they are held at their mean or mode (or rounded mean for integers). This includes grouping variables in mixed-effects models, so analysts who fit such models may want to specify the groups of interest using the `condition` argument, or supply model-specific arguments to compute population-level estimates. See details below.
#'
#' See the "Plots" vignette and website for tutorials and information on how to customize plots:
#'
#' * https://marginaleffects.com/bonus/plot.html
#' * https://marginaleffects.com
#'
#' @param variables Name of the variable whose contrast we want to plot on the y-axis.
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @param newdata When `newdata` is `NULL`, the grid is determined by the `condition` argument. When `newdata` is not `NULL`, the argument behaves in the same way as in the `predictions()` function. Note that the `condition` argument builds its own grid, so the `newdata` argument is ignored if the `condition` argument is supplied.
#' @inheritParams comparisons
#' @inheritParams plot_slopes
#' @inheritParams slopes
#' @template model_specific_arguments
#' @return A `ggplot2` object
#' @export
#' @examplesIf interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")
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
#'
#' # marginal comparisons
#' plot_comparisons(mod, variables = "hp", by = "am")
#'
#' # marginal comparisons on a counterfactual grid
#' plot_comparisons(mod,
#'     variables = "hp",
#'     by = "am",
#'     newdata = datagrid(am = 0:1, grid_type = "counterfactual")
#' )
#'
plot_comparisons <- function(
    model,
    variables = NULL,
    condition = NULL,
    by = NULL,
    newdata = NULL,
    type = NULL,
    vcov = NULL,
    conf_level = 0.95,
    wts = FALSE,
    comparison = "difference",
    transform = NULL,
    rug = FALSE,
    gray = getOption("marginaleffects_plot_gray", default = FALSE),
    draw = TRUE,
    ...) {
    # init
    call <- construct_call(model, "comparisons")
    model <- sanitize_model(model, call = call, newdata = newdata, wts = wts, vcov = vcov, by = by, ...)
    mfx <- new_marginaleffects_internal(
        call = call,
        model = model
    )

    if (inherits(mfx@model, "mira") && is.null(newdata)) {
        msg <- "Please supply a data frame to the `newdata` argument explicitly."
        insight::format_error(msg)
    }

    # order of the first few paragraphs is important
    scall <- rlang::enquo(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, mfx = mfx, by = by)
    if (!isFALSE(wts) && is.null(by)) {
        insight::format_error("The `wts` argument requires a `by` argument.")
    }

    checkmate::assert_character(
        by,
        null.ok = TRUE,
        max.len = 3,
        min.len = 1,
        names = "unnamed"
    )
    if ((!is.null(condition) && !is.null(by)) || (is.null(condition) && is.null(by))) {
        msg <- "One of the `condition` and `by` arguments must be supplied, but not both."
        insight::format_error(msg)
    }

    # sanity check
    checkmate::assert(
        checkmate::check_character(variables, names = "unnamed"),
        checkmate::check_list(variables, names = "unique"),
        .var.name = "variables"
    )

    # mlr3 and tidymodels
    if (is.null(mfx@modeldata) || nrow(mfx@modeldata) == 0) {
        mfx@modeldata <- newdata
    }

    # conditional
    if (!is.null(condition)) {
        condition <- sanitize_condition(
            mfx@model,
            condition,
            variables,
            modeldata = mfx@modeldata
        )
        v_x <- condition$condition1
        v_color <- condition$condition2
        v_facet_1 <- condition$condition3
        v_facet_2 <- condition$condition4
        datplot <- comparisons(
            mfx@model,
            newdata = condition$newdata,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            by = FALSE,
            wts = wts,
            variables = variables,
            comparison = comparison,
            transform = transform,
            cross = FALSE,
            modeldata = mfx@modeldata,
            ...
        )
    }

    # marginal
    if (!is.null(by)) {
        newdata <- sanitize_newdata(
            mfx = mfx,
            newdata = newdata,
            by = by,
            wts = wts
        )
        datplot <- comparisons(
            mfx@model,
            by = by,
            newdata = newdata,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            variables = variables,
            wts = wts,
            comparison = comparison,
            transform = transform,
            cross = FALSE,
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
        modeldata = mfx@modeldata
    )

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        out <- as.data.frame(datplot)
        attr(out, "posterior_draws") <- attr(datplot, "posterior_draws")
        return(out)
    }

    # ggplot2
    insight::check_if_installed("ggplot2")
    p <- plot_build(
        datplot,
        v_x = v_x,
        v_color = v_color,
        v_facet_1 = v_facet_1,
        v_facet_2 = v_facet_2,
        gray = gray,
        rug = rug,
        modeldata = mfx@modeldata
    )
    p <- p + ggplot2::labs(x = v_x, y = sprintf("Comparison"))

    return(p)
}
