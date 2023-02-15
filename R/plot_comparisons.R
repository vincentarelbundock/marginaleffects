#' Plot Comparisons
#'
#' Plot comparisons on the y-axis against values of one or more predictor variables (x-axis, colors, and facets). Plot average predictions.
#'
#' @param effect Name of the variable whose contrast we want to plot on the y-axis. If `NULL`, a plot of average comparisons is returned.
#' @param draw `TRUE` returns a `ggplot2` plot. `FALSE` returns a `data.frame` of the underlying data.
#' @inheritParams comparisons
#' @inheritParams plot_slopes
#' @inheritParams slopes
#' @return A `ggplot2` object
#' @export
#' @examples
#' mod <- lm(mpg ~ hp * drat * factor(am), data = mtcars)
#' 
#' plot_comparisons(mod, effect = "hp", condition = "drat")
#'
#' plot_comparisons(mod, effect = "hp", condition = c("drat", "am"))
#' 
#' plot_comparisons(mod, effect = "hp", condition = list("am", "drat" = 3:5))
#' 
#' plot_comparisons(mod, effect = "am", condition = list("hp", "drat" = range))
#' 
#' plot_comparisons(mod, effect = "am", condition = list("hp", "drat" = "threenum"))
plot_comparisons <- function(x,
                             effect = NULL,
                             condition = NULL,
                             by = NULL,
                             type = "response",
                             vcov = NULL,
                             conf_level = 0.95,
                             transform_pre = "difference",
                             transform_post = NULL,
                             draw = TRUE,
                             ...) {

    
    # sanity check
    checkmate::assert_character(by, null.ok = TRUE, max.len = 3, min.len = 1, names = "unnamed")
    checkmate::assert(
        checkmate::check_string(effect),
        checkmate::check_list(effect, names = "unique", len = 1))

    if ((!is.null(condition) && !is.null(by)) || (is.null(condition) && is.null(by))) {
        msg <- "One of the `condition` and `by` arguments must be supplied, but not both."
        insight::format_error(msg)
    }

    # conditional
    if (!is.null(condition)) {
        condition <- sanitize_condition(x, condition, effect)
        modeldata <- get_modeldata(x, additional_variables = names(condition$condition))
        v_x <- condition$condition1
        v_color <- condition$condition2
        v_facet <- condition$condition3
        datplot <- comparisons(
            x,
            newdata = condition$newdata,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            by = FALSE,
            wts = NULL,
            variables = effect,
            transform_pre = transform_pre,
            transform_post = transform_post,
            cross = FALSE,
            modeldata = modeldata,
            ...)
    }

    # marginal
    if (!is.null(by)) {
        modeldata <- get_modeldata(x, additional_variables = by)
        datplot <- comparisons(
            x,
            by = by,
            type = type,
            vcov = vcov,
            conf_level = conf_level,
            variables = effect,
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

    # colors, linetypes, and facets are categorical attributes
    if (isTRUE(v_color %in% colnames(datplot))) {
        datplot[[v_color]] <- factor(datplot[[v_color]])
    }
    if (isTRUE(v_facet %in% colnames(datplot))) {
        datplot[[v_facet]] <- factor(datplot[[v_facet]])
    }

    # shortcut labels: loop skips naturally when `condition=NULL`
    for (i in seq_along(condition)) {
        v <- paste0("condition", i)
        fun <- function(x, lab) {
            idx <- match(x, sort(unique(x)))
            factor(lab[idx], labels = lab)
        }
        if (identical(condition[[i]], "threenum")) {
            datplot[[v]] <- fun(datplot[[v]], c("-SD", "Mean", "+SD"))
        } else if (identical(condition[[i]], "minmax")) {
            datplot[[v]] <- fun(datplot[[v]], c("Min", "Max"))
        } else if (identical(condition[[i]], "quartile")) {
            datplot[[v]] <- fun(datplot[[v]], c("Q1", "Q2", "Q3"))
        }
    }
    
    # return immediately if the user doesn't want a plot
    if (isFALSE(draw)) {
        attr(datplot, "posterior_draws") <- attr(datplot, "posterior_draws")
        return(data.frame(datplot))
    } else {
        insight::check_if_installed("ggplot2")
        p <- plot_build(datplot, v_x = v_x, v_color = v_color, v_facet = v_facet)
    }

    if (is.null(names(effect))) {
        p <- p + ggplot2::labs(
            x = v_x,
            y = sprintf("Contrast in %s on %s", effect, {{condition$respname}}))
    } else {
        p <- p + ggplot2::labs(
            x = v_x,
            y = sprintf("Contrast in %s on %s", names(effect), {{condition$respname}}))
    }

    # `effect` is a categorical variable. We plot them in different facets
    contrast_cols <- grep("^contrast$|^contrast_", colnames(datplot), value = TRUE)
    for (con in contrast_cols) {
        if (length(unique(datplot[[con]])) == 1) {
            contrast_cols <- setdiff(contrast_cols, con)
        }
    }
    if (length(contrast_cols) > 0) {
        if (is.null(v_facet)) {
            fo <- sprintf("~ %s", paste(contrast_cols, collapse = "+"))
            p <- p + ggplot2::facet_wrap(fo)
        } else {
            fo <- sprintf("v_facet ~ %s", paste(contrast_cols, collapse = "+"))
            p <- p + ggplot2::facet_grid(fo)
        }
    } else if (!is.null(v_facet)) {
        fo <- ~v_facet
        p <- p + ggplot2::facet_wrap(fo)
    }

    return(p)
}


#' `plot_comparisons()` is an alias to `plot_comparisons()`
#'
#' This alias is kept for backward compatibility.
#' @inherit plot_predictions
#' @keywords internal
#' @export
plot_cco <- plot_comparisons


################### Backward compatibility for deprecated methods. Also nice to keep.
#' @export
#' @noRd
plot.comparisons <- plot_comparisons
