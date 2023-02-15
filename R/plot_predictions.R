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

    # shared code with plot_comparisons()
    tmp <- sanitize_condition(model, condition)
    dat <- tmp$modeldata
    nd <- tmp$newdata
    condition <- tmp$condition
    condition1 <- tmp$condition1
    condition2 <- tmp$condition2
    condition3 <- tmp$condition3
    resp <- tmp$resp
    respname <- tmp$respname

    datplot <- predictions(model,
        newdata = nd,
        type = type,
        vcov = vcov,
        conf_level = conf_level,
        transform_post = transform_post,
        modeldata = dat,
        ...)

    colnames(datplot)[colnames(datplot) == condition1] <- "condition1"
    colnames(datplot)[colnames(datplot) == condition2] <- "condition2"
    colnames(datplot)[colnames(datplot) == condition3] <- "condition3"

    # shortcut labels
    for (i in seq_along(condition)) {
        v <- paste0("condition", i)
        fun <- function(x, lab) {
            idx <- match(x, sort(unique(x)))
            factor(lab[idx], levels = lab)
        }
        if (identical(condition[[i]], "threenum")) {
            datplot[[v]] <- fun(datplot[[v]], c("-SD", "Mean", "+SD"))
        } else if (identical(condition[[i]], "minmax")) {
            datplot[[v]] <- fun(datplot[[v]], c("Min", "Max"))
        } else if (identical(condition[[i]], "quartile")) {
            datplot[[v]] <- fun(datplot[[v]], c("Q1", "Q2", "Q3"))
        }
    }

    # colors and facets are categorical attributes
    if ("condition2" %in% colnames(datplot)) datplot$condition2 <- factor(datplot$condition2)
    if ("condition3" %in% colnames(datplot)) datplot$condition3 <- factor(datplot$condition3)

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        for (i in seq_along(condition)) {
            colnames(datplot)[colnames(datplot) == paste0("condition", i)] <- names(condition)[i]
        }
        return(as.data.frame(datplot))
    } else {
        insight::check_if_installed("ggplot2")
    }

    # ggplot2
    p <- ggplot2::ggplot()

    # condition 1: continuous x-axis
    if (is.numeric(datplot$condition1)) {
        if (!isTRUE(vcov) && "conf.low" %in% colnames(datplot)) {
            p <- p + ggplot2::geom_ribbon(
                data = datplot,
                ggplot2::aes(
                    x = condition1,
                    y = estimate,
                    ymin = conf.low,
                    ymax = conf.high,
                    fill = condition2),
                alpha = .1)
        }
        p <- p + ggplot2::geom_line(
            data = datplot,
            ggplot2::aes(
                x = condition1,
                y = estimate,
                color = condition2))

    # categorical x-axis
    } else {
        if (!isTRUE(vcov) && "conf.low" %in% colnames(datplot)) {
            if (is.null(condition2)) {
                p <- p + ggplot2::geom_pointrange(
                    data = datplot,
                    ggplot2::aes(
                        x = condition1,
                        y = estimate,
                        ymin = conf.low,
                        ymax = conf.high,
                        color = condition2))
            } else {
                p <- p + ggplot2::geom_pointrange(
                    data = datplot,
                    position = ggplot2::position_dodge(.15),
                    ggplot2::aes(
                        x = condition1,
                        y = estimate,
                        ymin = conf.low,
                        ymax = conf.high,
                        color = condition2))
            }
        } else {
            p <- p + ggplot2::geom_point(
                data = datplot,
                ggplot2::aes(
                    x = condition1,
                    y = estimate,
                    color = condition2))
        }
    }

    # condition 3: facets
    if (!is.null(condition3)) {
        p <- p + ggplot2::facet_wrap(~condition3)
    }

    p <- p + ggplot2::labs(
        x = condition1,
        y = respname,
        color = condition2,
        fill = condition2,
        linetype = condition3)

    # attach model data for each of use
    attr(p, "modeldata") <- dat

    return(p)
}







#' `plot_predictions()` is an alias to `plot_predictions()`
#'
#' This alias is kept for backward compatibility.
#' @inherit plot_predictions
#' @keywords internal
#' @export
plot_cap <- plot_predictions
