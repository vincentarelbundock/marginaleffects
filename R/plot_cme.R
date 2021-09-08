#' @title Conditional marginal effects plot
#' @export
plot_cme <- function(model, 
                     effect,
                     condition,
                     conf.int = TRUE,
                     conf.level = 0.95,
                     draw = TRUE) {

    # get data to know over what range of values we should plot
    dat <- insight::get_data(model)

    # eventually we might allow two conditions
    checkmate::assert_character(condition, len = 1)
    checkmate::assert_true(condition %in% colnames(dat))
    condition1 <- condition[1]

    # build typical dataset with a sequence of values over "condition" range
    at_list <- list()
    at_list[[condition1]] <- seq(min(dat[[condition1]], na.rm = TRUE), 
                                 max(dat[[condition1]], na.rm = TRUE), 
                                 length.out = 25)
    nd <- typical(model, at = at_list)
    datplot <- marginaleffects(model, variables = effect, newdata = nd)
    colnames(datplot)[colnames(datplot) == condition1] <- "condition1"

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        return(datplot)
    } else {
        assert_dependency("ggplot2")
    }

    # ggplot2
    if ("std.error" %in% colnames(datplot) && isTRUE(conf.int)) {
        alpha <- 1 - conf.level
        datplot$conf.low <- datplot$dydx + qnorm(alpha / 2) * datplot$std.error
        datplot$conf.high <- datplot$dydx - qnorm(alpha / 2) * datplot$std.error
        p <- ggplot2::ggplot(datplot, ggplot2::aes(x = condition1, y = dydx, ymin = conf.low, ymax = conf.high)) +
             ggplot2::geom_ribbon(alpha = .1) +
             ggplot2::geom_line() + 
             ggplot2::labs(x = condition1, y = "Average marginal effects")
    } else {
        if ("group" %in% colnames(datplot)) {
            p <- ggplot2::ggplot(datplot, ggplot2::aes(y = term, x = estimate, color = group))
        } else {
            p <- ggplot2::ggplot(datplot, ggplot2::aes(y = term, x = estimate))
        }
        p <- p +
             ggplot2::geom_point() +
             ggplot2::labs(x = m, y = "Average marginal effects")
    }

    if ("group" %in% colnames(datplot)) {
        p + ggplot2::facet_wrap(~group)
    }

    # theme and return
    p <- p + ggplot2::theme_minimal()
    return(p) 
}
