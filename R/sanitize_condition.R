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
        out <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    }
    return(out)
}


sanitize_condition <- function(model, condition, variables = NULL, modeldata = NULL) {
    # allow multiple conditions and/or effects
    checkmate::assert(
        checkmate::check_character(condition, min.len = 1, max.len = 4),
        checkmate::check_list(condition, min.len = 1, max.len = 4))

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
    if (is.null(modeldata) && isTRUE(checkmate::check_character(condition))) {
        dat <- get_modeldata(model, additional_variables = condition)
    } else if (is.null(modeldata) && isTRUE(checkmate::check_list(condition))) {
        dat <- get_modeldata(model, additional_variables = names(condition))
    } else {
        dat <- modeldata
    }
    resp <- insight::get_response(model)
    respname <- insight::find_response(model)

    flag <-  checkmate::check_true(all(names(condition) %in% c(colnames(dat), "group")))
    if (!isTRUE(flag)) {
        msg <- sprintf("Entries in the `condition` argument must be element of: %s",
                       toString(colnames(dat)))
        insight::format_error(msg)
    }


    # condition names
    condition1 <- names(condition)[[1]]
    condition2 <- hush(names(condition)[[2]])
    condition3 <- hush(names(condition)[[3]])
    condition4 <- hush(names(condition)[[4]])

    # build typical dataset with a sequence of values over "condition" range
    at_list <- list()

    shortcuts <- c("threenum", "fivenum", "minmax", "quartile")

    # condition 1: x-axis
    if (is.null(condition[[1]])) {
        if (get_variable_class(dat, condition1, "binary")) {
            at_list[[condition1]] <- 0:1
        } else if (is.numeric(dat[[condition1]]) && !get_variable_class(dat, condition1, "categorical")) {
            at_list[[condition1]] <- seq(
                min(dat[[condition1]], na.rm = TRUE),
                max(dat[[condition1]], na.rm = TRUE),
                length.out = 50)
        } else {
            at_list[[condition1]] <- factor(unique(dat[[condition1]]))
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
        # defaults
        if (is.null(condition[[2]])) {
            #binary
            if (get_variable_class(dat, condition2, "binary")) {
              at_list[[condition2]] <- condition[[2]] <- 0:1
            # numeric default = Tukey's 5 numbers
            } else if (is.numeric(dat[[condition2]])) {
              condition[[2]] <- "fivenum"
            # other default = unique values
            } else if (condition2 %in% colnames(dat)) {
              condition[[2]] <- unique(dat[[condition2]])
            }
        }
        # known string shortcuts
        if (isTRUE(checkmate::check_choice(condition[[2]], shortcuts))) {
            at_list[[condition2]] <- condition_shortcuts(
                dat[[condition2]], condition[[2]], shortcuts
            )
        # user-supplied
        } else {
            at_list[[condition2]] <- condition[[2]]
        }
    }

    # condition 3: facet_1
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

    # condition 4: facet_2
    if (length(condition) > 3) {
      if (is.null(condition[[4]])) {
        if (is.numeric(dat[[condition4]])) {
          at_list[[condition4]] <- stats::fivenum(dat[[condition4]])
        } else {
          at_list[[condition4]] <- unique(dat[[condition4]])
        }
      } else {
        if (isTRUE(checkmate::check_choice(condition[[4]], shortcuts))) {
          at_list[[condition4]] <- condition_shortcuts(dat[[condition4]], condition[[4]], shortcuts)
        } else {
          at_list[[condition4]] <- condition[[4]]
        }
      }
    }

    at_list[["model"]] <- model
    at_list[["newdata"]] <- dat

    if (isTRUE(checkmate::check_list(variables))) {
        flag <- all(names(variables) %in% names(condition))
    } else {
        flag <- all(variables %in% names(condition))
    }
    if (!flag) {
        # sometimes we use the same condition as effect (e.g., GAM vignette),
        # but otherwise we don't want it at all
        if (isTRUE(checkmate::check_character(variables))) {
            dups <- setdiff(variables, names(condition))
            for (d in dups) {
                at_list[[d]] <- NULL
            }
        } else {
            at_list[[names(variables)]] <- NULL
        }
    }

    # mlr3 and tidymodels are not supported by `insight::find_variables()`, so we need to create a grid based on all the variables supplied in `newdata`
    if (inherits(at_list$model, "Learner") ||
        inherits(at_list$model, "model_fit") ||
        inherits(at_list$model, "workflow") ) {
        at_list$model <- NULL
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
        "condition3" = condition3,
        "condition4" = condition4)
    return(out)
}
