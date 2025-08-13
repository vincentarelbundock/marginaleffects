plot_preprocess <- function(
    dat,
    v_x,
    v_color = NULL,
    v_facet_1 = NULL,
    v_facet_2 = NULL,
    condition = NULL,
    mfx = NULL
) {
    for (v in names(condition$condition)) {
        fun <- function(x, lab) {
            idx <- match(x, sort(unique(x)))
            factor(lab[idx], levels = lab)
        }
        if (identical(condition$condition[[v]], "threenum")) {
            dat[[v]] <- fun(dat[[v]], c("-SD", "Mean", "+SD"))
        } else if (identical(condition$condition[[v]], "fivenum")) {
            labs <- stats::setNames(
                sort(unique(dat[[v]])),
                format(sort(unique(dat[[v]])), digits = 2)
            )
            dat[[v]] <- fun(dat[[v]], names(labs))
        } else if (identical(condition$condition[[v]], "minmax")) {
            dat[[v]] <- fun(dat[[v]], c("Min", "Max"))
        } else if (identical(condition$condition[[v]], "quartile")) {
            dat[[v]] <- fun(dat[[v]], c("Q1", "Q2", "Q3"))
        }
    }
    if (get_variable_class(mfx, v_x, "categorical")) {
        dat[[v_x]] <- factor(dat[[v_x]])
    }
    # colors, linetypes, and facets are categorical attributes
    if (isTRUE(v_color %in% colnames(dat))) {
        dat[[v_color]] <- factor(dat[[v_color]])
    }
    if (isTRUE(v_facet_1 %in% colnames(dat))) {
        dat[[v_facet_1]] <- factor(dat[[v_facet_1]])
    }
    if (isTRUE(v_facet_2 %in% colnames(dat))) {
        dat[[v_facet_2]] <- factor(dat[[v_facet_2]])
    }
    return(dat)
}


plot_build <- function(
    dat,
    v_x,
    v_color = NULL,
    v_facet_1 = NULL,
    v_facet_2 = NULL,
    dv = NULL,
    points = 0,
    rug = FALSE,
    gray = FALSE,
    mfx = NULL
) {
    checkmate::assert_flag(rug)
    checkmate::assert_flag(gray)

    # create index before building ggplot to make sure it is available
    dat$marginaleffects_term_index <- get_unique_index(dat, term_only = TRUE)
    multi_variables <- isTRUE(length(unique(dat$marginaleffects_term_index)) > 1)

    p <- ggplot2::ggplot(data = dat)

    if (
        points > 0 &&
            !get_variable_class(mfx, v_x, "categorical") &&
            !get_variable_class(mfx, dv, "categorical")
    ) {
        if (!is.null(v_color) && get_variable_class(mfx, v_color, "categorical")) {
            if (isTRUE(gray)) {
                p <- p +
                    ggplot2::geom_point(
                        data = mfx@modeldata,
                        alpha = points,
                        ggplot2::aes(
                            x = .data[[v_x]],
                            y = .data[[dv]],
                            shape = factor(.data[[v_color]])
                        )
                    )
            } else {
                p <- p +
                    ggplot2::geom_point(
                        data = mfx@modeldata,
                        alpha = points,
                        ggplot2::aes(
                            x = .data[[v_x]],
                            y = .data[[dv]],
                            color = factor(.data[[v_color]])
                        )
                    )
            }
        } else {
            p <- p +
                ggplot2::geom_point(
                    data = mfx@modeldata,
                    alpha = points,
                    ggplot2::aes(x = .data[[v_x]], y = .data[[dv]])
                )
        }
    }

    if (isTRUE(rug)) {
        p <- p + ggplot2::geom_rug(data = mfx@modeldata, ggplot2::aes(x = .data[[v_x]]))
        if (!is.null(dv)) {
            p <- p +
                ggplot2::geom_rug(data = mfx@modeldata, ggplot2::aes(y = .data[[dv]]))
        }
    }

    aes_args <- list(
        x = substitute(.data[[v_x]]),
        y = substitute(estimate)
    )

    if ("conf.low" %in% colnames(dat)) {
        aes_args$ymin <- substitute(conf.low)
        aes_args$ymax <- substitute(conf.high)
    }

    aes_args_ribbon <- aes_args
    aes_args_ribbon$fill <- aes_args$color
    aes_args_ribbon$color <- NULL

    # discrete x-axis
    if (is.factor(dat[[v_x]])) {
        if (!is.null(v_color)) {
            if (gray) {
                aes_args$shape <- substitute(factor(.data[[v_color]]))
            } else {
                aes_args$color <- substitute(factor(.data[[v_color]]))
            }
        }
        aes_obj <- do.call(ggplot2::aes, aes_args)
        if ("conf.low" %in% colnames(dat)) {
            p <- p +
                ggplot2::geom_pointrange(
                    mapping = aes_obj,
                    position = ggplot2::position_dodge(0.15)
                )
        } else {
            p <- p +
                ggplot2::geom_point(
                    mapping = aes_obj,
                    position = ggplot2::position_dodge(0.15)
                )
        }

        # continuous x-axis
    } else {
        if (!is.null(v_color)) {
            if (gray) {
                aes_args$linetype <- substitute(factor(.data[[v_color]]))
                aes_args_ribbon$linetype <- substitute(factor(.data[[v_color]]))
            } else {
                aes_args$color <- substitute(factor(.data[[v_color]]))
                aes_args_ribbon$fill <- substitute(factor(.data[[v_color]]))
            }
        }
        aes_obj_ribbon <- do.call(ggplot2::aes, aes_args_ribbon)
        aes_args$ymin <- aes_args$ymax <- NULL
        aes_obj <- do.call(ggplot2::aes, aes_args)
        if ("conf.low" %in% colnames(dat)) {
            p <- p + ggplot2::geom_ribbon(aes_obj_ribbon, alpha = 0.1)
            p <- p + ggplot2::geom_line(aes_obj)
        }
        p <- p + ggplot2::geom_line(aes_obj)
    }

    # facets: 3rd and 4th variable and/or multiple effects
    ## If pass two facets then make facet grid
    if (!is.null(v_facet_1) && !is.null(v_facet_2)) {
        fo <- stats::as.formula(paste(
            v_facet_2,
            "~",
            ifelse(multi_variables, "marginaleffects_term_index +", ""),
            v_facet_1
        ))
        p <- p +
            ggplot2::facet_grid(fo, scales = "free", labeller = function(x) {
                lapply(
                    ggplot2::label_both(x),
                    gsub,
                    pattern = "marginaleffects_term_index: ",
                    replacement = ""
                )
            })
        ## if pass only 1 facet then facet_wrap
    } else if (!is.null(v_facet_1) && is.null(v_facet_2)) {
        fo <- stats::as.formula(paste(
            "~",
            ifelse(multi_variables, "marginaleffects_term_index +", ""),
            v_facet_1
        ))
        p <- p + ggplot2::facet_wrap(fo, scales = "free")
    } else if (multi_variables) {
        fo <- stats::as.formula("~ marginaleffects_term_index")
        p <- p + ggplot2::facet_wrap(fo, scales = "free")
    }

    return(p)
}
