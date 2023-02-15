plot_preprocess <- function(dat, v_x, v_color = NULL, v_facet = NULL, condition = NULL, modeldata = NULL) {
    for (v in names(condition$condition)) {
        fun <- function(x, lab) {
            idx <- match(x, sort(unique(x)))
            factor(lab[idx], levels = lab)
        }
        if (identical(condition$condition[[v]], "threenum")) {
            dat[[v]] <- fun(dat[[v]], c("-SD", "Mean", "+SD"))
        } else if (identical(condition$condition[[v]], "minmax")) {
            dat[[v]] <- fun(dat[[v]], c("Min", "Max"))
        } else if (identical(condition$condition[[v]], "quartile")) {
            dat[[v]] <- fun(dat[[v]], c("Q1", "Q2", "Q3"))
        }
    }
    if (get_variable_class(modeldata, v_x, "categorical")) {
        dat[[v_x]] <- factor(dat[[v_x]])
    }
    # colors, linetypes, and facets are categorical attributes
    if (isTRUE(v_color %in% colnames(dat))) {
        dat[[v_color]] <- factor(dat[[v_color]])
    }
    if (isTRUE(v_facet %in% colnames(dat))) {
        dat[[v_facet]] <- factor(dat[[v_facet]])
    }
    return(dat)

}

plot_build <- function(dat, v_x, v_color = NULL, v_facet = NULL) {
    p <- ggplot2::ggplot()

    # discrete x-axis
    if (is.factor(dat[[v_x]])) {
        ggpoint <- ggplot2::geom_point(data = dat,
            ggplot2::aes(x = .data[[v_x]], y = estimate))
        ggpointcol <- ggplot2::geom_point(data = dat,
            ggplot2::aes(x = .data[[v_x]], y = estimate, color = .data[[v_color]]))
        ggpointshape <- ggplot2::geom_point(data = dat,
            ggplot2::aes(x = .data[[v_x]], y = estimate, shape = .data[[v_color]]))
        ggpointrange <- ggplot2::geom_pointrange(data = dat,
            ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high))
        ggpointrangecol <- ggplot2::geom_pointrange(data = dat,
            ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, color = .data[[v_color]]),
            position = ggplot2::position_dodge(.15))
        ggpointrangeshape <- ggplot2::geom_pointrange(data = dat,
            ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, shape = .data[[v_color]]),
            position = ggplot2::position_dodge(.15))
        if ("conf.low" %in% colnames(dat)) {
            if (is.null(v_color)) {
                p <- p + ggpointrange
            } else {
                p <- p + ggpointrangecol
            }
        } else {
            if (is.null(v_color)) {
                p <- p + ggpointcol
            } else {
                p <- p + ggpoint
            }
        }

    # continuous x-axis
    } else {
        ggrib <- ggplot2::geom_ribbon(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high), alpha = .1)
        ggribcol <- ggplot2::geom_ribbon(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, fill = .data[[v_color]]), alpha = .1)
        ggribtype <- ggplot2::geom_ribbon(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, group = .data[[v_color]]), alpha = .1)
        ggline <- ggplot2::geom_line(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate))
        gglinecol <- ggplot2::geom_line(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, color = .data[[v_color]]))
        gglinetype <- ggplot2::geom_line(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, linetype = .data[[v_color]]))
        if ("conf.low" %in% colnames(dat)) {
            if (is.null(v_color)) {
                p <- p + ggrib
            } else {
                p <- p + ggribcol
            }
        }
        if (is.null(v_color)) {
            p <- p + ggline
        } else {
            p <- p + gglinecol
        }
    }

    return(p)
}