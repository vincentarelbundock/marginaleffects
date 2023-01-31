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
                             type = "response",
                             vcov = NULL,
                             conf_level = 0.95,
                             transform_post = NULL,
                             draw = TRUE,
                             ...) {
    # shared code with plot_comparisons()
    tmp <- get_plot_newdata(model, condition)
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

    # set a new theme only if the default is theme_grey. this prevents user's
    # theme_set() from being overwritten
    if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
        p <- p + ggplot2::theme_minimal()
    }

    # attach model data for each of use
    attr(p, "modeldata") <- dat

    return(p)
}


condition_shortcuts <- function(x, tr, shortcuts) {
    if (identical(tr, "threenum")) {
        m <- mean(x, na.rm = TRUE)
        s <- stats::sd(x, na.rm = TRUE)
        out <- c(m - s, m, m + s)

    } else if (identical(tr, "fivenum")) {
        out <- stats::fivenum(x, na.rm = TRUE)

    } else if (identical(tr, "minmax")) {
        out <- c(
            min(x, na.rm = TRUE),
            max(x, na.rm = TRUE))

    } else if (identical(tr, "quartile")) {
        out <- stats::quantile(x, probs = c(.25, .5, .75), na.rm = TRUE)
    }

    return(out)
}


get_plot_newdata <- function(model, condition, effect = NULL) {

    # allow multiple conditions and/or effects
    checkmate::assert(
        checkmate::check_character(condition, min.len = 1, max.len = 3),
        checkmate::check_list(condition, min.len = 1, max.len = 3)
    )

    # c("a", "b") or list("a", "b") -> named list of NULLs
    flag1 <- isTRUE(checkmate::check_character(condition))
    flag2 <- isTRUE(checkmate::check_list(condition, names = "unnamed")) &&
             all(sapply(condition, function(x) isTRUE(checkmate::check_string(x))))
    if (flag1 || flag2) {
        condition <- stats::setNames(rep(list(NULL), length(condition)), unlist(condition))
    }

    # validity of the list
    for (i in seq_along(condition)) {
        if (identical(names(condition)[i], "")) {
            if (!isTRUE(checkmate::check_character(condition[[i]], len = 1))) {
                msg <- "The `condition` argument must be a character vector or a named list."
                insight::format_error(msg)
            } else {
                names(condition)[i] <- condition[[i]]
                tmp <- stats::setNames(list(NULL), names(condition)[i])
                condition <- utils::modifyList(condition, tmp, keep.null = TRUE)
            }
        }
    }

    # get data to know over what range of values we should plot
    dat <- get_modeldata(model)
    resp <- insight::get_response(model)
    respname <- insight::find_response(model)

    checkmate::assert_true(isTRUE(all(names(condition) %in% c(colnames(dat), "group"))))

    # condition names
    condition1 <- names(condition)[[1]]
    condition2 <- hush(names(condition)[[2]])
    condition3 <- hush(names(condition)[[3]])

    # build typical dataset with a sequence of values over "condition" range
    at_list <- list()

    shortcuts <- c("threenum", "fivenum", "minmax", "quartile")

    # condition 1: x-axis
    if (is.null(condition[[1]])) {
        if (is.numeric(dat[[condition1]]) && !isTRUE(attributes(dat)$marginaleffects_variable_class[[condition1]] == "factor")) {
            at_list[[condition1]] <- seq(
                min(dat[[condition1]], na.rm = TRUE),
                max(dat[[condition1]], na.rm = TRUE),
                length.out = 25)
        } else {
            at_list[[condition1]] <- unique(dat[[condition1]])
        }
    } else {
        if (isTRUE(checkmate::check_choice(condition[[1]], shortcuts))) {
            at_list[[condition1]] <- condition_shortcuts(dat[[condition1]], condition[[1]], shortcuts)
        } else {
            at_list[[condition1]] <- condition[[1]]
        }
    }

    # condition 2: color
    if (length(condition) > 1) {
        if (is.null(condition[[2]])) {
            if (is.numeric(dat[[condition2]])) {
                at_list[[condition2]] <- stats::fivenum(dat[[condition2]])
            } else {
                at_list[[condition2]] <- unique(dat[[condition2]])
            }
        } else {
            if (isTRUE(checkmate::check_choice(condition[[2]], shortcuts))) {
                at_list[[condition2]] <- condition_shortcuts(dat[[condition2]], condition[[2]], shortcuts)
            } else {
                at_list[[condition2]] <- condition[[2]]
            }
        }
    }

    # condition 3: facet
    if (length(condition) > 2) {
        if (is.null(condition[[3]])) {
            if (is.numeric(dat[[condition3]])) {
                at_list[[condition3]] <- stats::fivenum(dat[[condition3]])
            } else {
                at_list[[condition3]] <- unique(dat[[condition3]])
            }
        } else {
            if (isTRUE(checkmate::check_choice(condition[[3]], shortcuts))) {
                at_list[[condition3]] <- condition_shortcuts(dat[[condition3]], condition[[3]], shortcuts)
            } else {
                at_list[[condition3]] <- condition[[3]]
            }
        }
    }

    at_list[["model"]] <- model

    if (!is.null(effect)) {
        # sometimes we use the same condition as effect (e.g., GAM vignette),
        # but otherwise we don't want it at all
        if (isTRUE(checkmate::check_character(effect))) {
            if (!effect %in% names(condition)) {
                at_list[[effect]] <- NULL
            }
        } else {
            at_list[[names(effect)]] <- NULL
        }
    }

    # create data
    nd <- do.call("datagrid", at_list)

    out <- list(
        "modeldata" = dat,
        "newdata" = nd, 
        "resp" = resp,
        "respname" = respname,
        "condition" = condition, 
        "condition1" = condition1, 
        "condition2" = condition2, 
        "condition3" = condition3) 
    return(out)
}



#' `plot_predictions()` is an alias to `plot_predictions()`
#'
#' This alias is kept for backward compatibility.
#' @inherit plot_predictions
#' @keywords internal
#' @export
plot_cap <- plot_predictions
