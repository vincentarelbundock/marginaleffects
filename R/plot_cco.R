#' Plot Conditional Contrasts
#'
#' This function plots contrasts (y-axis) against values of predictor(s)
#' variable(s) (x-axis and colors). This is especially useful in models with
#' interactions, where the values of contrasts depend on the values of
#' "condition" variables.
#'
#' @param effect Name of the variable whose contrast we want to plot on the y-axis
#' @param condition String or vector of two strings. The first is a variable
#' name to be displayed on the x-axis. The second is a variable whose values
#' will be displayed in different colors. Other numeric variables are held at
#' their means. Other categorical variables are held at their modes.
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams comparisons
#' @inheritParams plot_cme
#' @inheritParams plot.marginaleffects
#' @inheritParams marginaleffects
#' @return A `ggplot2` object
#' @family plot
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * wt, data = mtcars)
#' plot_cco(mod, effect = "hp", condition = "wt")
#'
#' mod <- lm(mpg ~ hp * wt * am, data = mtcars)
#' plot_cco(mod, effect = "hp", condition = c("wt", "am"))
#'
plot_cco <- function(model,
                     effect = NULL,
                     condition = NULL,
                     type = "response",
                     vcov = NULL,
                     conf_level = 0.95,
                     transform_pre = "difference",
                     transform_post = NULL,
                     draw = TRUE,
                     ...) {

    # get data to know over what range of values we should plot
    dat <- hush(insight::get_data(model))
    resp <- insight::find_response(model)[1]

    # eventually we might allow multiple conditions and/or effects
    checkmate::assert_true(length(effect) == 1)
    if (isTRUE(checkmate::check_list(effect)) &&
        any(c("sd", "2sd", "minmax", "iqr") %in% effect)) {
        msg <- format_msg(
        'The "sd", "2sd", "minmax", and "iqr" options are not available in the
        `effect` argument of this plotting function. You can specify custom
        contrasts by using a numeric vector. Ex:

        model <- lm(mpg ~ hp * wt, data = mtcars)
        plot_cco(model, effect = list("hp" = c(110, 130)), condition = "wt")
        ')
        stop(msg, call. = FALSE)
    }

    # allow multiple conditions and/or effects
    checkmate::assert_character(condition, min.len = 1, max.len = 2)

    ## not sure why this fails in testthat
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
                                     length.out = 100)
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
    datplot <- comparisons(
        model,
        newdata = nd,
        type = type,
        vcov = vcov,
        conf_level = conf_level,
        variables = effect,
        transform_pre = transform_pre,
        transform_post = transform_post,
        interaction = FALSE,
        ...)


    checkmate::assert_true(all(condition %in% colnames(datplot)))

    draws <- attr(datplot, "posterior_draws")
    colnames(datplot)[colnames(datplot) == condition1] <- "condition1"
    colnames(datplot)[colnames(datplot) == condition2] <- "condition2"
    colnames(datplot)[colnames(datplot) == condition3] <- "condition3"

    # colors and linetypes are categorical attributes
    if ("condition2" %in% colnames(datplot)) datplot$condition2 <- factor(datplot$condition2)
    if ("condition3" %in% colnames(datplot)) datplot$condition3 <- factor(datplot$condition3)

    # CIs are automatically added to predictions but (maybe) not marginaleffects output
    if (!"conf.low" %in% colnames(datplot)) {
        alpha <- 1 - conf_level
        datplot$conf.low <- datplot$comparison + stats::qnorm(alpha / 2) * datplot$std.error
        datplot$conf.high <- datplot$comparison - stats::qnorm(alpha / 2) * datplot$std.error
    }

    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        attr(datplot, "posterior_draws") <- draws
        return(datplot)
    } else {
        assert_dependency("ggplot2")
    }


    # ggplot2
    p <- ggplot2::ggplot()

    # continuous x-axis
    if (is.numeric(datplot$condition1)) {
        if ("conf.low" %in% colnames(datplot)) {
             p <- p + ggplot2::geom_ribbon(
                data = datplot,
                alpha = .1,
                ggplot2::aes(
                    x = condition1,
                    y = comparison,
                    ymin = conf.low,
                    ymax = conf.high,
                    fill = condition2))
        }
        p <- p + ggplot2::geom_line(
            data = datplot,
            ggplot2::aes(
                    x = condition1,
                    y = comparison,
                    color = condition2,
                    linetype = condition3))

    # categorical x-axis
    } else {
        if ("conf.low" %in% colnames(datplot)) {
             if (is.null(condition2)) {
                 p <- p + ggplot2::geom_pointrange(
                     data = datplot,
                     ggplot2::aes(
                        x = condition1,
                        y = comparison,
                        ymin = conf.low,
                        ymax = conf.high,
                        color = condition2))
             } else {
                 p <- p + ggplot2::geom_pointrange(
                    data = datplot,
                    position = ggplot2::position_dodge(.15),
                    ggplot2::aes(
                        x = condition1,
                        y = comparison,
                        ymin = conf.low,
                        ymax = conf.high,
                        color = condition2))
             }
        } else {
            p <- p + ggplot2::geom_point(
                data = datplot,
                ggplot2::aes(
                    x = condition1,
                    y = comparison,
                    color = condition2))
        }
    }

    if (is.null(names(effect))) {
        p <- p + ggplot2::labs(
            x = condition1,
            y = sprintf("Contrast in %s on %s", effect, resp))
    } else {
        p <- p + ggplot2::labs(
            x = condition1,
            y = sprintf("Contrast in %s on %s", names(effect), resp))
    }

    # `effect` is a categorical variable. We plot them in different facets
    contrast_cols <- grep("^contrast$|^contrast_", colnames(datplot), value = TRUE)
    for (con in contrast_cols) {
        if (length(unique(datplot[[con]])) == 1) {
            contrast_cols <- setdiff(contrast_cols, con)
        }
    }
    if (length(contrast_cols) > 0) {
        f <- paste("~", paste(contrast_cols, collapse = "+"))
        p <- p + ggplot2::facet_wrap(f)
    }

    # set a new theme only if the default is theme_grey. this prevents user's
    # theme_set() from being overwritten
    if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
        p <- p +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.title = ggplot2::element_blank())
    }

    attr(p, "data") <- dat

    return(p)
}
