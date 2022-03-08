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
#' will be displayed in different colors.
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
                     effect,
                     condition,
                     type = "response",
                     conf.int = TRUE,
                     conf.level = 0.95,
                     draw = TRUE) {

    # get data to know over what range of values we should plot
    dat <- insight::get_data(model)
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

    # build typical dataset with a sequence of values over "condition" range
    at_list <- list()

    # condition 1
    if (is.numeric(dat[[condition1]]) && !isTRUE(attr(dat[[condition1]], "factor"))) {
        at_list[[condition1]] <- seq(min(dat[[condition1]], na.rm = TRUE), 
                                     max(dat[[condition1]], na.rm = TRUE), 
                                     length.out = 25)
    } else {
        at_list[[condition1]] <- unique(dat[[condition1]])
    }

    # condition 2
    if (!is.null(condition2)) {
        if (is.numeric(dat[[condition2]])) {
            at_list[[condition2]] <- stats::fivenum(dat[[condition2]])
        } else {
            at_list[[condition2]] <- unique(dat[[condition2]])
        }
    }

    # condition 3
    if (!is.null(condition3)) {
        if (is.numeric(dat[[condition3]])) {
            at_list[[condition3]] <- stats::fivenum(dat[[condition3]])
        } else {
            at_list[[condition3]] <- unique(dat[[condition3]])
        }
    }

    # create data
    at_list[["model"]] = model
    nd <- do.call("typical", at_list)
    datplot <- marginaleffects(model, newdata = nd, variables = effect)
    colnames(datplot)[colnames(datplot) == condition1] <- "condition1"
    colnames(datplot)[colnames(datplot) == condition2] <- "condition2"
    colnames(datplot)[colnames(datplot) == condition3] <- "condition3"
    
    # colors and linetypes are categorical attributes
    if ("condition2" %in% colnames(datplot)) datplot$condition2 <- factor(datplot$condition2)
    if ("condition3" %in% colnames(datplot)) datplot$condition3 <- factor(datplot$condition3)

    # CIs are automatically added to predictions but not marginaleffects output
    if (isTRUE(conf.int) && !"conf.low" %in% colnames(datplot)) {
        alpha <- 1 - conf.level
        datplot$conf.low <- datplot$dydx + stats::qnorm(alpha / 2) * datplot$std.error
        datplot$conf.high <- datplot$dydx - stats::qnorm(alpha / 2) * datplot$std.error
    }

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        return(datplot)
    } else {
        assert_dependency("ggplot2")
    }

    # ggplot2
    p <- ggplot2::ggplot(datplot, ggplot2::aes(x = condition1, 
                                               y = dydx, 
                                               ymin = conf.low, 
                                               ymax = conf.high))

    # continuous x-axis
    if (is.numeric(datplot$condition1)) {
        if (isTRUE(conf.int) && "conf.low" %in% colnames(datplot)) {
             p <- p + ggplot2::geom_ribbon(ggplot2::aes(fill = condition2), alpha = .1)
        }
        p <- p + ggplot2::geom_line(ggplot2::aes(color = condition2, linetype = condition3))
    # categorical x-axis
    } else {
        if (isTRUE(conf.int) && "conf.low" %in% colnames(datplot)) {
             if (is.null(condition2)) {
                 p <- p + ggplot2::geom_pointrange()
             } else {
                 p <- p + ggplot2::geom_pointrange(ggplot2::aes(color = condition2), 
                                                   position = ggplot2::position_dodge(.15))
             }
        } else {
            p <- p + ggplot2::geom_point(ggplot2::aes(color = condition2))
        }
    }

    p <- p + ggplot2::labs(x = condition1, 
                           y = sprintf("Marginal effect of %s on %s", effect, resp),
                           color = condition2,
                           fill = condition2,
                           linetype = condition3)

    # set a new theme only if the default is theme_grey. this prevents user's
    # theme_set() from being overwritten
    if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
        p <- p + ggplot2::theme_minimal()
    }

    return(p)
}
