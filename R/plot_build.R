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


plot_build <- function(
    dat,
    v_x,
    v_color = NULL,
    v_facet = NULL,
    dv = NULL,
    modeldata = NULL,
    points = 0,
    rug = FALSE,
    gray = FALSE) {
        
    checkmate::assert_flag(rug)
    checkmate::assert_flag(gray)

    # create index before building ggplot to make sure it is available
    dat$marginaleffects_term_index <- get_unique_index(dat, term_only = TRUE)
    multi_variables <- isTRUE(length(unique(dat$marginaleffects_term_index)) > 1)

    p <- ggplot2::ggplot()

    if (points > 0 &&
        !get_variable_class(modeldata, v_x, "categorical") &&
        !get_variable_class(modeldata, dv, "categorical")) {
        if (!is.null(v_color) && get_variable_class(modeldata, v_color, "categorical")) {
            p <- p + ggplot2::geom_point(
                data = modeldata, alpha = points,
                ggplot2::aes(x = .data[[v_x]], y = .data[[dv]], color = factor(.data[[v_color]])))
        } else {
            p <- p + ggplot2::geom_point(
                data = modeldata, alpha = points,
                ggplot2::aes(x = .data[[v_x]], y = .data[[dv]]))
        }
    }
    
    if (isTRUE(rug)) {
        p <- p + ggplot2::geom_rug(data = modeldata, ggplot2::aes(x = .data[[v_x]]))
        if (!is.null(dv)) {
            p <- p + ggplot2::geom_rug(data = modeldata, ggplot2::aes(y = .data[[dv]]))
        }
    }

    # discrete x-axis
    if (is.factor(dat[[v_x]])) {
        ggpoint <- ggplot2::geom_point(data = dat,
            ggplot2::aes(x = .data[[v_x]], y = estimate))
        ggpointrange <- ggplot2::geom_pointrange(data = dat,
            ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high))
        if (gray) {
            ggpointcol <- ggplot2::geom_point(
                data = dat,
                ggplot2::aes(x = .data[[v_x]], y = estimate, shape = .data[[v_color]]))
            ggpointrangecol <- ggplot2::geom_pointrange(data = dat,
                ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, shape = .data[[v_color]]),
                position = ggplot2::position_dodge(.15))
        } else {
            ggpointcol <- ggplot2::geom_point(
                data = dat,
                ggplot2::aes(x = .data[[v_x]], y = estimate, color = .data[[v_color]]))
            ggpointrangecol <- ggplot2::geom_pointrange(data = dat,
                ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, color = .data[[v_color]]),
                position = ggplot2::position_dodge(.15))
        }

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
        ggline <- ggplot2::geom_line(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate))
        if (gray) {
            gglinecol <- ggplot2::geom_line(data = dat, ggplot2::aes(x = .data[[v_x]], y = estimate, linetype = .data[[v_color]]))
            ggribcol <- ggplot2::geom_ribbon(data = dat,
                ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, group = .data[[v_color]]), alpha = .1)
        } else {
            gglinecol <- ggplot2::geom_line(data = dat,
                ggplot2::aes(x = .data[[v_x]], y = estimate, color = .data[[v_color]]))
            ggribcol <- ggplot2::geom_ribbon(data = dat,
                ggplot2::aes(x = .data[[v_x]], y = estimate, ymin = conf.low, ymax = conf.high, fill = .data[[v_color]]), alpha = .1)
        }

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

    # facets: 3rd variable and/or multiple effects
    
    if (multi_variables && !is.null(v_facet)) {
        fo <- stats::as.formula(paste("~ marginaleffects_term_index +", v_facet))
        p <- p + ggplot2::facet_wrap(fo, scales = "free")
        
    } else if (multi_variables) {
        fo <- stats::as.formula("~ marginaleffects_term_index")
        p <- p + ggplot2::facet_wrap(fo, scales = "free")
        
    } else if (!is.null(v_facet)) {
        fo <- stats::as.formula(paste("~", v_facet))
        p <- p + ggplot2::facet_wrap(fo, scales = "free")
    }

    return(p)
}