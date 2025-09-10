mean_i <- function(x) as.integer(round(mean(x, na.rm = TRUE)))
mean_na <- function(x) mean(x, na.rm = TRUE)
unique_s <- function(x) sort(unique(x))


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
#' @param FUN a function to be applied to all variables in the grid. This is useful when you want to apply the same function to all variables, such as `mean` or `median`. If you specify `FUN`, it will override the `grid_type` defaults, but not other `FUN_*` arguments below.
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
#'   * "dataframe": Similar to "mean_or_mode" but creates a data frame by binding columns element-wise rather than taking the cross-product. All explicitly specified vectors must have the same length (or length 1), and the result has as many rows as the longest vector. This differs from other grid types which use `expand.grid()` or `data.table::CJ()` to create all combinations.
#'   * "counterfactual": the entire dataset is duplicated for each combination of the variable values specified in `...`. Variables not explicitly supplied to `datagrid()` are set to their observed values in the original dataset.
#' @details
#' If `datagrid` is used in a `predictions()`, `comparisons()`, or `slopes()` call as the
#' `newdata` argument, the model is automatically inserted in the `model` argument of `datagrid()`
#' call, and users do not need to specify either the `model` or `newdata` arguments. The same behavior will occur when the value supplied to `newdata=` is a function call which starts with "datagrid". This is intended to allow users to create convenience shortcuts like:
#'
#' **Warning about hierarchical grouping variables:** When using the default `grid_type = "mean_or_mode"` with hierarchical models (such as mixed models with nested grouping factors), `datagrid()` may create invalid combinations of grouping variables. For example, if you have students nested within schools, or countries nested within regions, the modal values of each grouping variable may not correspond to valid nested relationships in the data. This can cause prediction errors. To avoid this issue, explicitly specify valid combinations of hierarchical grouping variables in the `datagrid()` call, or use `grid_type = "counterfactual"` to preserve the original data structure.
#'
#' \preformatted{
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
#' # `model` or `newdata` arguments.
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
#'
#' # Use `by` to hold variables at group-specific values
#' mod2 <- lm(mpg ~ hp + cyl, mtcars)
#' datagrid(model = mod2, hp = mean, by = "cyl")
#'
#' # Use `FUN` to apply function to all variables
#' datagrid(model = mod2, FUN = median)
#'
#' # Use `grid_type="dataframe"` for column-wise binding instead of cross-product
#' datagrid(model = mod2, hp = c(100, 200), cyl = c(4, 6), grid_type = "dataframe")
datagrid <- function(
    ...,
    model = NULL,
    newdata = NULL,
    by = NULL,
    grid_type = "mean_or_mode",
    response = FALSE,
    FUN = NULL,
    FUN_character = NULL,
    FUN_factor = NULL,
    FUN_logical = NULL,
    FUN_numeric = NULL,
    FUN_integer = NULL,
    FUN_binary = NULL,
    FUN_other = NULL) {
    checkmate::assert_choice(
        grid_type,
        choices = c("mean_or_mode", "balanced", "counterfactual", "dataframe")
    )
    checkmate::assert_function(FUN, null.ok = TRUE)
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

    if (is.null(newdata) && is.null(model)) {
        stop_sprintf("One of `newdata` and `model` must not be `NULL`.")
    }

    # internal model settings are useful if available
    if ("marginaleffects_internal" %in% ...names()) {
        mfx <- ...get("marginaleffects_internal")
    } else {
        mfx <- new_marginaleffects_internal(
            model = model,
            call = call("predictions"))
    }

    # priority: newdata. fallback: modeldata
    if (is.null(newdata)) {
        newdata <- mfx@modeldata
    }

    # sanity checks
    if (!is.null(by)) {
        checkmate::assert_true(all(by %in% colnames(newdata)))
    }

    # prune data, otherwise it can be very expensive to summarize all columns
    # hack because prune works on final marginaleffects objects
    if (!is.null(mfx@model)) {
        variable_names <- c(
            hush(insight::find_variables(mfx@model, flatten = TRUE)),
            hush(insight::find_weights(mfx@model, flatten = TRUE)),
            by)
        variable_names <- intersect(sort(unique(variable_names)), colnames(newdata))
        if (length(variable_names) > 0) {
            newdata <- subset(newdata, select = variable_names)
        }
    } else {
        variable_names <- colnames(newdata)
    }

    variable_class <- detect_variable_class(newdata, model = model)

    if (is.null(by)) {
        idx <- data.frame()
    } else {
        idx <- newdata[, by, drop = FALSE]
    }
    if (ncol(idx) == 0) {
        newdata_split <- list(newdata)
    } else {
        newdata_split <- split(newdata, idx, drop = TRUE)
    }

    values_split <- lapply(newdata_split, function(x) {
        datagrid_newdata_to_list(
            ...,
            newdata = x,
            model = model,
            variable_class = variable_class,
            grid_type = grid_type,
            FUN = FUN,
            FUN_character = FUN_character,
            FUN_factor = FUN_factor,
            FUN_logical = FUN_logical,
            FUN_numeric = FUN_numeric,
            FUN_integer = FUN_integer,
            FUN_binary = FUN_binary,
            FUN_other = FUN_other
        )
    })

    if (grid_type %in% c("balanced", "mean_or_mode")) {
        out_split <- lapply(seq_along(values_split), function(i) {
            x <- c(values_split[[i]][[1]], values_split[[i]][[2]], use.names = TRUE)
            len <- setdiff(unique(lengths(x)), 1)
            do.call(data.table::CJ, x)
        })
    } else if (grid_type == "dataframe") {
        out_split <- lapply(seq_along(values_split), function(i) {
            x <- c(values_split[[i]][[1]], values_split[[i]][[2]], use.names = TRUE)
            len <- setdiff(unique(lengths(x)), 1)
            if (length(len) > 1) {
                stop_sprintf('With `grid_type="dataframe"`, the length of each vector must be 1 or be the same for every variable.')
            }
            as.data.frame(x, check.names = FALSE)
        })
    } else if (grid_type == "counterfactual") {
        out_split <- lapply(seq_along(values_split), function(i) {
            explicit <- do.call(data.table::CJ, values_split[[i]]$explicit)
            implicit <- newdata_split[[i]]
            data.table::setDF(implicit)
            data.table::setDF(explicit)
            implicit <- implicit[, setdiff(names(implicit), names(explicit)),
                drop = FALSE]
            implicit <- cbind(data.frame(rowidcf = seq_len(nrow(implicit))), implicit)
            merge(implicit, explicit, all = NULL, sort = FALSE)
        })
    } else {
        stop_sprintf("Unknown grid_type '%s'.", grid_type)
    }

    out <- data.table::rbindlist(out_split)

    out <- data.frame(out, check.names = FALSE)

    if (!"rowid" %in% colnames(out) && nrow(out) > 0) {
        out <- cbind(data.frame(rowid = seq_len(nrow(out))), out)
    }

    attr(out, "variable_names_datagrid") <- names(list(...))


    return(out)
}


datagrid_newdata_to_list <- function(
    ...,
    newdata,
    model,
    variable_class,
    grid_type,
    FUN = NULL,
    FUN_character = NULL,
    FUN_factor = NULL,
    FUN_logical = NULL,
    FUN_numeric = NULL,
    FUN_integer = NULL,
    FUN_binary = NULL,
    FUN_other = NULL) {
    FUN_balanced <- list(
        ".FUN_character" = unique_s,
        ".FUN_factor" = unique_s,
        ".FUN_logical" = unique_s,
        ".FUN_binary" = unique_s,
        ".FUN_numeric" = mean_na,
        ".FUN_integer" = mean_i,
        ".FUN_other" = mean_na
    )

    FUN_mean_or_mode <- list(
        ".FUN_character" = get_mode,
        ".FUN_factor" = get_mode,
        ".FUN_logical" = get_mode,
        ".FUN_binary" = get_mode,
        ".FUN_numeric" = mean_na,
        ".FUN_integer" = mean_i,
        ".FUN_other" = mean_na
    )

    FUN_countefactual <- stats::setNames(
        rep(list(identity), length(FUN_balanced)),
        names(FUN_balanced)
    )

    if (grid_type == "balanced") {
        FUN_list <- FUN_balanced
    } else if (grid_type == "mean_or_mode") {
        FUN_list <- FUN_mean_or_mode
    } else if (grid_type == "dataframe") {
        FUN_list <- FUN_mean_or_mode
    } else if (grid_type == "counterfactual") {
        FUN_list <- FUN_countefactual
    }

    # FUN overrides defaults from grid_type but not manual specifications
    # order matters
    if (!is.null(FUN)) {
        FUN_list <- stats::setNames(rep(list(FUN), length(FUN_list)), names(FUN_list))
    }

    list2env(FUN_list, envir = environment())
    if (!is.null(FUN_character)) .FUN_character <- FUN_character
    if (!is.null(FUN_factor)) .FUN_factor <- FUN_factor
    if (!is.null(FUN_logical)) .FUN_logical <- FUN_logical
    if (!is.null(FUN_numeric)) .FUN_numeric <- FUN_numeric
    if (!is.null(FUN_integer)) .FUN_integer <- FUN_integer
    if (!is.null(FUN_binary)) .FUN_binary <- FUN_binary
    if (!is.null(FUN_other)) .FUN_other <- FUN_other

    values <- list()

    explicit_values <- list()
    explicit <- list(...)
    for (e in names(explicit)) {
        if (is.function(explicit[[e]])) {
            if (!e %in% colnames(newdata)) {
                warn_sprintf("The variable '%s' is not in the newdata.", e)
            }
            explicit_values[[e]] <- explicit[[e]](newdata[[e]])
        } else if (isTRUE(checkmate::check_atomic_vector(explicit[[e]]))) {
            explicit_values[[e]] <- unique(explicit[[e]])
        } else {
            stop_sprintf("The value for '%s' must be a vector or a function.", e)
        }
    }

    implicit_values <- list()
    implicit <- setdiff(colnames(newdata), names(explicit))

    # balanced grid should not balance on response otherwise we multiple the rows
    if (grid_type == "balanced") {
        implicit <- setdiff(implicit, hush(insight::find_response(model, flatten = TRUE)))
    }

    # some packages require the response variable to be included in the grid for predict()
    # implicit <- setdiff(implicit, hush(insight::find_response(model, flatten = TRUE)))
    for (i in implicit) {
        if (check_variable_class(variable_class, i, "binary")) {
            implicit_values[[i]] <- .FUN_binary(newdata[[i]])
        } else if (check_variable_class(variable_class, i, "character")) {
            implicit_values[[i]] <- .FUN_character(newdata[[i]])
        } else if (check_variable_class(variable_class, i, "logical")) {
            implicit_values[[i]] <- .FUN_logical(newdata[[i]])
        } else if (check_variable_class(variable_class, i, "categorical")) {
            implicit_values[[i]] <- .FUN_factor(newdata[[i]])
        } else if (check_variable_class(variable_class, i, "numeric")) {
            implicit_values[[i]] <- .FUN_numeric(newdata[[i]])
        } else if (check_variable_class(variable_class, i, "integer")) {
            implicit_values[[i]] <- .FUN_integer(newdata[[i]])
        } else {
            implicit_values[[i]] <- .FUN_other(newdata[[i]])
        }
    }

    # Process factors in explicit values
    for (e in names(explicit_values)) {
        explicit_values[[e]] <- sanitize_datagrid_factor(explicit_values[[e]], newdata[[e]], variable_class, e)
    }

    out <- list(implicit = implicit_values, explicit = explicit_values)
    return(out)
}


sanitize_datagrid_factor <- function(values, newdata_col, variable_class, var_name) {
    if (
        is.factor(newdata_col) ||
            isTRUE(check_variable_class(variable_class, var_name, "factor"))
    ) {
        if (is.factor(newdata_col)) {
            levs <- levels(newdata_col)
        } else {
            levs <- as.character(sort(unique(newdata_col)))
        }
        values <- as.character(values)
        if (!all(values %in% c(levs, NA))) {
            msg <- sprintf(
                'The "%s" element of the `at` list corresponds to a factor variable. The values entered in the `at` list must be one of the factor levels: %s.',
                var_name,
                toString(dQuote(levs, NULL))
            )
            stop(msg, call. = FALSE)
        }

        values <- factor(values, levels = levs)
    }
    return(values)
}
