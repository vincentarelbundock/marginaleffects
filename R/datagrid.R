#' Data grids
#'
#' @description
#' Generate a data grid of user-specified values for use in the `newdata` argument of the `predictions()`, `comparisons()`, and `slopes()` functions. This is useful to define where in the predictor space we want to evaluate the quantities of interest. Ex: the predicted outcome or slope for a 37 year old college graduate.
#'
#' @param ... named arguments with vectors of values or functions for user-specified variables.
#' + Functions are applied to the variable in the `model` dataset or `newdata`, and must return a vector of the appropriate type.
#' + Character vectors are automatically transformed to factors if necessary.
#' +The output will include all combinations of these variables (see Examples below.)
#' @param model Model object
#' @param newdata data.frame (one and only one of the `model` and `newdata` arguments can be used.)
#' @param by character vector with grouping variables within which `FUN_*` functions are applied to create "sub-grids" with unspecified variables.
#' @param response Logical should the response variable be included in the grid, even if it is not specified explicitly.
#' @param FUN_character the function to be applied to character variables.
#' @param FUN_factor the function to be applied to factor variables. This only applies if the variable in the original data is a factor. For variables converted to factor in a model-fitting formula, for example, `FUN_character` is used.
#' @param FUN_logical the function to be applied to logical variables.
#' @param FUN_integer the function to be applied to integer variables.
#' @param FUN_binary the function to be applied to binary variables.
#' @param FUN_numeric the function to be applied to numeric variables.
#' @param FUN_other the function to be applied to other variable types.
#' @param grid_type character. Determines the functions to apply to each variable. The defaults can be overridden by defining individual variables explicitly in `...`, or by supplying a function to one of the `FUN_*` arguments.
#'   * "mean_or_mode": Character, factor, logical, and binary variables are set to their modes. Numeric, integer, and other variables are set to their means.
#'   * "balanced": Each unique level of character, factor, logical, and binary variables are preserved. Numeric, integer, and other variables are set to their means. Warning: When there are many variables and many levels per variable, a balanced grid can be very large. In those cases, it is better to use `grid_type="mean_or_mode"` and to specify the unique levels of a subset of named variables explicitly.
#'   * "counterfactual": the entire dataset is duplicated for each combination of the variable values specified in `...`. Variables not explicitly supplied to `datagrid()` are set to their observed values in the original dataset.
#' @details
#' If `datagrid` is used in a `predictions()`, `comparisons()`, or `slopes()` call as the
#' `newdata` argument, the model is automatically inserted in the `model` argument of `datagrid()`
#' call, and users do not need to specify either the `model` or `newdata` arguments. The same behavior will occur when the value supplied to `newdata=` is a function call which starts with "datagrid". This is intended to allow users to create convenience shortcuts like:
#'
#' **Warning about hierarchical grouping variables:** When using the default `grid_type = "mean_or_mode"` with hierarchical models (such as mixed models with nested grouping factors), `datagrid()` may create invalid combinations of grouping variables. For example, if you have students nested within schools, or countries nested within regions, the modal values of each grouping variable may not correspond to valid nested relationships in the data. This can cause prediction errors. To avoid this issue, explicitly specify valid combinations of hierarchical grouping variables in the `datagrid()` call, or use `grid_type = "counterfactual"` to preserve the original data structure.
#'
#' \preformatted{
#' library(marginaleffects)
#' mod <- lm(mpg ~ am + vs + factor(cyl) + hp, mtcars)
#' datagrid_bal <- function(...) datagrid(..., grid_type = "balanced")
#' predictions(model, newdata = datagrid_bal(cyl = 4))
#' }
#'
#'
#' If users supply a model, the data used to fit that model is retrieved using
#' the `insight::get_data` function.
#' @return
#' A `data.frame` in which each row corresponds to one combination of the named
#' predictors supplied by the user via the `...` dots. Variables which are not
#' explicitly defined are held at their mean or mode.
#' @export
#' @examples
#' # The output only has 2 rows, and all the variables except `hp` are at their
#' # mean or mode.
#' datagrid(newdata = mtcars, hp = c(100, 110))
#'
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp, mtcars)
#' datagrid(model = mod, hp = c(100, 110))
#'
#' # Use in `marginaleffects` to compute "Typical Marginal Effects". When used
#' # in `slopes()` or `predictions()` we do not need to specify the
#' #`model` or `newdata` arguments.
#' slopes(mod, newdata = datagrid(hp = c(100, 110)))
#'
#' # datagrid accepts functions
#' datagrid(hp = range, cyl = unique, newdata = mtcars)
#' comparisons(mod, newdata = datagrid(hp = fivenum))
#'
#' # The full dataset is duplicated with each observation given counterfactual
#' # values of 100 and 110 for the `hp` variable. The original `mtcars` includes
#' # 32 rows, so the resulting dataset includes 64 rows.
#' dg <- datagrid(newdata = mtcars, hp = c(100, 110), grid_type = "counterfactual")
#' nrow(dg)
#'
#' # We get the same result by feeding a model instead of a data.frame
#' mod <- lm(mpg ~ hp, mtcars)
#' dg <- datagrid(model = mod, hp = c(100, 110), grid_type = "counterfactual")
#' nrow(dg)
datagrid <- function(
    ...,
    model = NULL,
    newdata = NULL,
    by = NULL,
    grid_type = "mean_or_mode",
    response = FALSE,
    FUN_character = NULL,
    FUN_factor = NULL,
    FUN_logical = NULL,
    FUN_numeric = NULL,
    FUN_integer = NULL,
    FUN_binary = NULL,
    FUN_other = NULL
) {
    # backward compatibility: 20231220
    if (identical(grid_type, "typical")) {
        grid_type <- "mean_or_mode"
    }

    # sanity
    checkmate::assert_choice(
        grid_type,
        choices = c("mean_or_mode", "balanced", "counterfactual")
    )
    checkmate::assert_function(FUN_character, null.ok = TRUE)
    checkmate::assert_function(FUN_factor, null.ok = TRUE)
    checkmate::assert_function(FUN_logical, null.ok = TRUE)
    checkmate::assert_function(FUN_binary, null.ok = TRUE)
    checkmate::assert_function(FUN_integer, null.ok = TRUE)
    checkmate::assert_function(FUN_numeric, null.ok = TRUE)
    checkmate::assert_function(FUN_other, null.ok = TRUE)
    checkmate::assert_character(by, null.ok = TRUE)
    checkmate::assert_data_frame(newdata, null.ok = TRUE)
    checkmate::assert_flag(response)

    explicit <- c(...names(), by)

    mean_i <- function(x) as.integer(round(mean(x, na.rm = TRUE)))
    mean_na <- function(x) mean(x, na.rm = TRUE)
    unique_s <- function(x) sort(unique(x))
    if (grid_type == "mean_or_mode") {
        if (is.null(FUN_character)) {
            FUN_character <- get_mode
        }
        if (is.null(FUN_logical)) {
            FUN_logical <- get_mode
        }
        if (is.null(FUN_factor)) {
            FUN_factor <- get_mode
        }
        if (is.null(FUN_binary)) {
            FUN_binary <- get_mode
        }
        if (is.null(FUN_numeric)) {
            FUN_numeric <- mean_na
        }
        if (is.null(FUN_other)) {
            FUN_other <- mean_na
        }
        if (is.null(FUN_integer)) FUN_integer <- mean_i
    } else if (grid_type == "balanced") {
        # decided not to sort strings because the levels are not explicit
        # as in factors, and perhaps the order of rows is intentional.
        # not a very strong argument either way, so we do not break backward compatibility
        if (is.null(FUN_character)) {
            FUN_character <- unique_s
        }
        if (is.null(FUN_logical)) {
            FUN_logical <- unique_s
        }
        if (is.null(FUN_factor)) {
            FUN_factor <- unique_s
        }
        if (is.null(FUN_binary)) {
            FUN_binary <- unique_s
        }
        if (is.null(FUN_numeric)) {
            FUN_numeric <- mean_na
        }
        if (is.null(FUN_other)) {
            FUN_other <- mean_na
        }
        if (is.null(FUN_integer)) FUN_integer <- mean_i
    } else if (grid_type == "counterfactual") {
        if (!is.null(by)) {
            insight::format_error(
                "The `by` argument is not supported for counterfactual grids."
            )
        }
        args <- list(
            model = model,
            newdata = newdata
        )
        args <- c(list(...), args)
        out <- do.call("datagridcf_internal", args)
        return(out)
    }

    if (!is.null(by)) {
        if (is.null(newdata) && is.null(model)) {
            insight::format_error(
                "One of `newdata` and `model` must not be `NULL`."
            )
        }
        if (is.null(newdata)) {
            newdata <- get_modeldata(model, additional_variables = by)
        }
        if (!all(by %in% colnames(newdata))) {
            insight::format_error(
                "All elements of `by` must match column names in `newdata`."
            )
        }
        data.table::setDT(newdata)
        idx <- subset(newdata, select = by)
        newdata_list <- split(newdata, idx, keep.by = TRUE)
        for (i in seq_along(newdata_list)) {
            args <- c(
                list(...),
                list(
                    model = model,
                    newdata = newdata_list[[i]],
                    response = response,
                    FUN_character = FUN_character,
                    FUN_factor = FUN_factor,
                    FUN_logical = FUN_logical,
                    FUN_binary = FUN_binary,
                    FUN_numeric = FUN_numeric,
                    FUN_integer = FUN_integer,
                    FUN_other = FUN_other,
                    by = by
                )
            )
            for (b in by) {
                args[[b]] <- unique_s
            }
            newdata_list[[i]] <- do.call(datagrid_engine, args)
        }

        out <- data.table::rbindlist(newdata_list)
        data.table::setDF(out)

        attr(out, "explicit") <- explicit

        return(out)
    }

    out <- datagrid_engine(
        ...,
        model = model,
        newdata = newdata,
        response = response,
        FUN_character = FUN_character,
        FUN_factor = FUN_factor,
        FUN_logical = FUN_logical,
        FUN_binary = FUN_binary,
        FUN_numeric = FUN_numeric,
        FUN_integer = FUN_integer,
        FUN_other = FUN_other
    )

    if (!"rowid" %in% colnames(out)) {
        out$rowid <- seq_len(nrow(out))
    }

    attr(out, "explicit") <- explicit
    return(out)
}


datagrid_engine <- function(
    ...,
    model = NULL,
    newdata = NULL,
    response = response,
    FUN_character = get_mode,
    # need to be explicit for numeric variables transfered to factor in model formula
    FUN_factor = get_mode,
    FUN_logical = get_mode,
    FUN_binary = get_mode,
    FUN_numeric = function(x) mean(x, na.rm = TRUE),
    FUN_integer = function(x) round(mean(x, na.rm = TRUE)),
    FUN_other = function(x) mean(x, na.rm = TRUE),
    by = NULL
) {
    tmp <- prep_datagrid(..., model = model, newdata = newdata, by = by)

    at <- tmp$at
    dat <- tmp$newdata
    variables_all <- tmp$all
    variables_manual <- names(at)
    variables_automatic <- tmp$automatic

    # usually we don't want the response in the grid, but
    # sometimes there are two responses and we need one of them:
    # brms::brm(y | trials(n) ~ x + w + z)
    if (!is.null(model) && isFALSE(response)) {
        resp <- insight::find_response(model)
        if (inherits(model, "brmsfit")) {
            fl <- as.character(stats::formula(model))
            matches <- regexpr("trials\\(.*?\\)", fl)
            extracted <- regmatches(fl, matches)[1]
            if (isFALSE(is.na(extracted[1]))) {
                extracted <- gsub("trials\\((.*)\\)", "\\1", extracted)
            } else {
                extracted <- NULL
            }
            extracted <- unlist(extracted)
            resp <- setdiff(resp, extracted)
        }

        variables_automatic <- setdiff(variables_automatic, resp)
    }

    if (length(variables_automatic) > 0) {
        idx <- intersect(variables_automatic, colnames(dat))
        dat_automatic <- dat[, ..idx, drop = FALSE]
        dat_automatic <- stats::na.omit(dat_automatic)
        out <- list()
        # na.omit destroys attributes, and we need the "factor" attribute
        # created by insight::get_data
        for (n in names(dat_automatic)) {
            if (
                get_variable_class(dat, n, c("factor", "strata", "cluster")) ||
                    n %in% tmp[["cluster"]]
            ) {
                if (is.factor(dat_automatic[[n]])) {
                    out[[n]] <- FUN_factor(dat_automatic[[n]])
                } else {
                    out[[n]] <- FUN_character(dat_automatic[[n]])
                }
            } else if (get_variable_class(dat, n, "binary")) {
                out[[n]] <- FUN_binary(dat_automatic[[n]])
            } else if (get_variable_class(dat, n, "logical")) {
                out[[n]] <- FUN_logical(dat_automatic[[n]])
            } else if (get_variable_class(dat, n, "character")) {
                out[[n]] <- FUN_character(dat_automatic[[n]])
            } else if (get_variable_class(dat, n, "numeric")) {
                if (is.integer(dat_automatic[[n]])) {
                    out[[n]] <- FUN_integer(dat_automatic[[n]])
                } else {
                    out[[n]] <- FUN_numeric(dat_automatic[[n]])
                }
            } else {
                out[[n]] <- FUN_other(dat_automatic[[n]])
            }
        }
    } else {
        out <- list()
    }

    if (!is.null(at)) {
        for (n in names(at)) {
            out[n] <- at[n]
        }
    }

    # unique before counting
    out <- lapply(out, unique)

    # warn on very large prediction grid
    num <- as.numeric(lengths(out)) # avoid integer overflow
    num <- Reduce(f = "*", num)
    if (isTRUE(num > 1e9)) {
        stop(
            "You are trying to create a prediction grid with more than 1 billion rows, which is likely to exceed the memory and computational power available on your local machine. Presumably this is because you are considering many variables with many levels. All of the functions in the `marginaleffects` package include arguments to specify a restricted list of variables over which to create a prediction grid.",
            call. = FALSE
        )
    }

    fun <- data.table::CJ
    args <- c(out, list(sorted = FALSE))
    out <- do.call("fun", args)

    # better to assume "standard" class as output
    data.table::setDF(out)

    return(out)
}


datagridcf_internal <- function(
    ...,
    model = NULL,
    newdata = NULL
) {
    if (...length() == 0) {
        insight::format_error(
            "Users must specify variable values when `grid_type='counterfactual'"
        )
    }

    tmp <- prep_datagrid(..., model = model, newdata = newdata)
    at <- tmp$at
    dat <- tmp$newdata
    variables_all <- tmp$all
    variables_manual <- names(at)
    variables_automatic <- c(
        tmp$automatic,
        "marginaleffects_wts_internal",
        "rowid_dedup"
    )

    # `at` -> `data.frame`
    at <- lapply(at, unique)

    fun <- data.table::CJ
    args <- c(at, list(sorted = FALSE))
    at <- do.call("fun", args)

    rowid <- data.frame(rowidcf = seq_len(nrow(dat)))
    if (length(variables_automatic) > 0) {
        idx <- intersect(variables_automatic, colnames(dat))
        dat_automatic <- dat[, ..idx, drop = FALSE]
        dat_automatic$rowidcf <- rowid$rowidcf
        setcolorder(
            dat_automatic,
            c("rowidcf", setdiff(names(dat_automatic), "rowidcf"))
        )
        # cross-join 2 data.tables, faster than merging two dataframes
        out <- cjdt(list(dat_automatic, at))
    } else {
        out <- merge(rowid, at, all = TRUE)
    }

    data.table::setDF(out)

    attr(out, "variables_datagrid") <- names(out)

    return(out)
}


prep_datagrid <- function(..., model = NULL, newdata = NULL, by = NULL) {
    checkmate::assert_data_frame(newdata, null.ok = TRUE)

    at <- list(...)

    # e.g., mlogit vignette we plot by group, but group is of length 0 because
    # we don't know how many groups there are until we make the first
    # prediction.
    for (i in seq_along(at)) {
        if (length(at[[i]]) == 0) {
            at[[i]] <- NULL
        }
    }

    # if (!is.null(model) & !is.null(newdata)) {
    #     msg <- "One of the `model` or `newdata` arguments must be `NULL`."
    #     stop(msg, call. = FALSE)
    # }

    if (is.null(model) & is.null(newdata)) {
        msg <- "The `model` and `newdata` arguments are both `NULL`. When calling `datagrid()` *inside* the `slopes()` or `comparisons()` functions, the `model` and `newdata` arguments can both be omitted. However, when calling `datagrid()` on its own, users must specify either the `model` or the `newdata` argument (but not both)."
        insight::format_error(msg)
    }

    # newdata before model: if user supplies newdata explicitly, they might want
    # all columns for something like `hypothesis = function()`
    if (!is.null(newdata)) {
        variables_list <- NULL
        variables_all <- colnames(newdata)
        newdata <- set_variable_class(modeldata = newdata, model = model)
    } else if (!is.null(model)) {
        variables_list <- insight::find_variables(model, verbose = FALSE)
        variables_all <- unlist(variables_list, recursive = TRUE)
        # weights are not extracted by default
        variables_all <- c(variables_all, insight::find_weights(model))
    }

    variables_manual <- names(at)
    variables_automatic <- setdiff(variables_all, variables_manual)

    # fill in missing data after sanity checks
    if (is.null(newdata)) {
        newdata <- get_modeldata(model, additional_variables = FALSE)
    }

    attr_variable_classes <- attr(newdata, "marginaleffects_variable_class")

    # subset columns, otherwise it can be ultra expensive to compute summaries for every variable. But do the expensive thing anyway if `newdata` is supplied explicitly by the user, or in counterfactual grids.
    if (!is.null(model) && is.null(newdata)) {
        variables_sub <- c(
            hush(insight::find_variables(
                model,
                flatten = TRUE,
                verbose = FALSE
            )),
            hush(unlist(insight::find_weights(model), use.names = FALSE))
        ) # glmmTMB needs weights column for predictions
        variables_sub <- c(variables_sub, variables_manual)
        variables_sub <- c(
            variables_sub,
            c("marginaleffects_wts_internal", "rowid_dedup")
        )
        variables_sub <- intersect(colnames(newdata), variables_sub)
        if (length(variables_sub) > 0) {
            newdata <- subset(newdata, select = variables_sub)
        }
    }

    # check `at` names
    variables_missing <- setdiff(names(at), c(variables_all, "group", by))
    if (length(variables_missing) > 0) {
        warning(
            sprintf(
                "Some of the variable names are missing from the model data: %s",
                toString(variables_missing)
            ),
            call. = FALSE
        )
    }

    idx <- vapply(newdata, is.matrix, logical(1L))
    if (any(idx)) {
        if (any(names(newdata)[idx] %in% variables_all)) {
            warn_sprintf(
                "Matrix columns are not supported as predictors and are therefore omitted. This may prevent computation of the quantities of interest. You can construct your own prediction dataset and supply it explicitly to the `newdata` argument. This issue can happen when using functions like `scale()`, which returns matrices rather than vectors. Convert your column to a vector before fitting you model."
            )
        }
        newdata <- newdata[, !idx, drop = FALSE]
    }

    # restore attributes after subsetting
    attr(newdata, "marginaleffects_variable_class") <- attr_variable_classes

    # check `at` elements and convert them to factor as needed
    for (n in names(at)) {
        # functions first otherwise we try to coerce functions to character
        if (is.function(at[[n]])) {
            modeldata <- attr(newdata, "newdata_modeldata")
            if (!is.null(modeldata) && n %in% colnames(modeldata)) {
                at[[n]] <- at[[n]](modeldata[[n]])
            } else {
                at[[n]] <- at[[n]](newdata[[n]])
            }
        }

        # not an "else" situation because we want to process the output of functions too
        if (
            is.factor(newdata[[n]]) ||
                isTRUE(get_variable_class(newdata, n, "factor"))
        ) {
            if (is.factor(newdata[[n]])) {
                levs <- levels(newdata[[n]])
            } else {
                levs <- as.character(sort(unique(newdata[[n]])))
            }
            at[[n]] <- as.character(at[[n]])
            if (!all(at[[n]] %in% c(levs, NA))) {
                msg <- sprintf(
                    'The "%s" element of the `at` list corresponds to a factor variable. The values entered in the `at` list must be one of the factor levels: %s.',
                    n,
                    toString(dQuote(levels(newdata[[n]]), NULL))
                )
                stop(msg, call. = FALSE)
            }

            at[[n]] <- factor(at[[n]], levels = levs)
        }
    }

    # cluster identifiers will eventually be treated as factors
    if (!is.null(model)) {
        v <- insight::find_variables(model, verbose = FALSE)
        v <- unlist(v[names(v) %in% c("cluster", "strata")], recursive = TRUE)
        variables_cluster <- c(v, insight::find_random(model, flatten = TRUE))
    } else {
        variables_cluster <- NULL
    }

    data.table::setDT(newdata)

    out <- list(
        "newdata" = newdata,
        "at" = at,
        "all" = variables_all,
        "manual" = variables_manual,
        "automatic" = variables_automatic,
        "cluster" = variables_cluster
    )
    return(out)
}
