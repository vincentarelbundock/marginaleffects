#' Conditional marginal means plot
#' 
#' This function plots the marginal mean of the outcome variable (y-axis)
#' against values of one or more predictors.
#' 
#' @param condition Name of the variable to plot on the x-axis
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams plot.marginaleffects
#' @inheritParams plot_cme
#' @return A `ggplot2` object
#' @export
plot_cmm <- function(model, 
                     condition,
                     conf.int = TRUE,
                     conf.level = 0.95,
                     draw = TRUE) {

    # get data to know over what range of values we should plot
    dat <- insight::get_data(model)
    resp <- insight::find_response(model)[1]

    # allow multiple conditions and/or effects
    checkmate::assert_character(condition, min.len = 1, max.len = 3)

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
    if (is.numeric(dat[[condition1]])) {
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
    datplot <- marginalmeans(model, newdata = nd)
    colnames(datplot)[colnames(datplot) == condition1] <- "condition1"
    colnames(datplot)[colnames(datplot) == condition2] <- "condition2"
    colnames(datplot)[colnames(datplot) == condition3] <- "condition3"

    model.matrix(model, newdata = nd)

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        return(datplot)
    } else {
        assert_dependency("ggplot2")
    }

    # colors and linetypes are categorical attributes
    if ("condition2" %in% colnames(dat)) dat$condition2 <- factor(dat$condition2)
    if ("condition3" %in% colnames(dat)) dat$condition3 <- factor(dat$condition3)

    # ggplot2
    p <- ggplot2::ggplot(datplot, ggplot2::aes(x = condition1, 
                                               y = predicted, 
                                               ymin = conf.low, 
                                               ymax = conf.high))
    if (isTRUE(conf.int) && "conf.low" %in% colnames(datplot)) {
         p <- p + ggplot2::geom_ribbon(ggplot2::aes(fill = condition2), alpha = .1)
    }

    p <- p + ggplot2::geom_line(ggplot2::aes(color = condition2, linetype = condition3)) + 
             ggplot2::labs(x = condition1, 
                           y = sprintf("Marginal mean of %s", resp),
                           color = condition2,
                           fill = condition2,
                           linetype = condition3)

    # theme and return
    p <- p + ggplot2::theme_minimal()
    return(p) 
}
