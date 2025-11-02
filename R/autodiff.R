# Global variable to avoid R CMD check note
utils::globalVariables("mAD")

eval_fun_with_numpy_arrays <- function(FUN, ...) {
    dots <- list(...)
    # Handle special cases for JAX indexing
    for (i in seq_along(dots)) {
        if (names(dots)[i] %in% c("num_groups", "link_type", "family_type", "comparison_type")) {
            # Keep num_groups, link_type, family_type as is (integer scalars)
        } else if (names(dots)[i] == "groups") {
            # Convert groups to integer array explicitly
            dots[[i]] <- reticulate::np_array(dots[[i]], dtype = "int32")
        } else {
            # Convert to numpy array
            dots[[i]] <- reticulate::np_array(dots[[i]])
        }
    }
    # Call Python function - use reticulate's $ operator which handles Python callables
    # Build the call dynamically
    result <- rlang::exec(FUN, !!!dots)
    return(result)
}


autodiff_warning <- function(feature) {
    msg <- "Automatic differentiation with JAX does not support %s. Reverting to finite difference."
    warning(sprintf(msg, feature), call. = FALSE)
}


#' @keywords internal
#' @noRd
get_autodiff_args <- function(model, mfx) {
    UseMethod("get_autodiff_args")
}


#' @rdname get_autodiff_args
#' @keywords internal
#' @noRd
get_autodiff_args.default <- function(model, mfx) {
    return(NULL)
}



jax_align_group_J <- function(jac_fun, mfx, original, estimates, X, X_hi, X_lo) {
    if (isTRUE(grepl("_byG", jac_fun))) {
        # Case 1: comparisons with by=character uses pre-assigned group IDs
        if (!is.null(original) && "marginaleffects_group_id" %in% colnames(original)) {
            groups <- original$marginaleffects_group_id - 1L  # Convert to 0-indexed
            num_groups <- max(groups) + 1L
        } else {
            # Case 2: predictions with by=character, or comparisons with by=TRUE
            # Need to create groups on the fly

            # Determine grouping columns
            if (is.character(mfx@by)) {
                # predictions with by=character: use user-specified columns from newdata
                bycols <- mfx@by
                if (inherits(mfx@newdata, "data.table")) {
                    groups_data <- mfx@newdata[, ..bycols]
                } else {
                    groups_data <- mfx@newdata[, bycols, drop = FALSE]
                }
            } else if (!is.null(original)) {
                # comparisons with by=TRUE: use term/contrast from original
                bycols <- intersect(c("term", "contrast"), colnames(original))
                groups_data <- subset(original, select = bycols)
            } else {
                bycols <- NULL
            }

            if (length(bycols) > 0) {
                # Sort data by group columns to ensure consistent ordering
                idx <- do.call(order, groups_data)
                groups_data <- groups_data[idx, , drop = FALSE]
                if (!is.null(X)) X <- X[idx, , drop = FALSE]
                if (!is.null(X_hi)) X_hi <- X_hi[idx, , drop = FALSE]
                if (!is.null(X_lo)) X_lo <- X_lo[idx, , drop = FALSE]
                # Create group IDs
                groups_combined <- apply(groups_data, 1, function(x) paste0(x, collapse = "_"))
                groups <- as.integer(as.factor(groups_combined)) - 1L
                num_groups <- max(groups) + 1L
            } else {
                groups <- num_groups <- NULL
            }
        }
    } else {
        groups <- num_groups <- NULL
    }

    return(list(groups = groups, num_groups = num_groups, X = X, X_hi = X_hi, X_lo = X_lo))
}



#' Add model matrix attribute to a data frame
#' @keywords internal
#' @noRd
add_model_matrix_attribute_data <- function(mfx, data) {
    # Temporarily set as newdata to get model matrix
    original_newdata <- mfx@newdata
    mfx@newdata <- data
    data_with_mm <- add_model_matrix_attribute(mfx)
    return(data_with_mm)
}


#' Compute predictions with JAX autodiff (estimates, SE, jacobian)
#' @return List with estimate, std.error, jacobian, or NULL if unsupported
#' @keywords internal
#' @noRd
jax_predictions <- function(mfx, vcov_matrix, ...) {
    mAD <- settings_get("mAD")

    # Validate model support
    autodiff_args <- get_autodiff_args(mfx@model, mfx)
    if (is.null(autodiff_args)) return(NULL)

    # Check for unsupported features
    if (!is.null(mfx@hypothesis)) {
        autodiff_warning("the `hypothesis` argument")
        return(NULL)
    }

    if (!isTRUE(mfx@by) && !isFALSE(mfx@by) && !is.character(mfx@by)) {
        autodiff_warning("values of `by` other than TRUE, FALSE, or a character vector")
        return(NULL)
    }

    # Get model matrix
    X <- attr(mfx@newdata, "marginaleffects_model_matrix")
    if (is.null(X)) return(NULL)

    # Get coefficients
    coefs <- get_coef(mfx@model, ...)

    # Check for NA coefficients (e.g., from aliased terms)
    if (anyNA(coefs)) {
        autodiff_warning("models with NA coefficients (aliased terms)")
        return(NULL)
    }

    # Determine aggregation function
    if (isFALSE(mfx@by)) {
        fun_name <- "predictions"
        groups <- NULL
        num_groups <- NULL
    } else if (isTRUE(mfx@by)) {
        fun_name <- "predictions_byT"
        groups <- NULL
        num_groups <- NULL
    } else if (is.character(mfx@by)) {
        fun_name <- "predictions_byG"
        # Prepare group indices
        group_result <- jax_align_group_J("jacobian_byG", mfx, NULL, NULL, X, NULL, NULL)
        groups <- group_result$groups
        num_groups <- group_result$num_groups
        X <- group_result$X

        # If groups couldn't be created, fall back to finite differences
        if (is.null(groups) || is.null(num_groups)) {
            return(NULL)
        }
    }

    # Select autodiff function
    # e.g., mAD$linear$predictions is a module containing predictions(), predictions_byT(), predictions_byG()
    # The base module name (predictions or comparisons)
    base_module_name <- if (grepl("predictions", fun_name)) "predictions" else "comparisons"
    module <- mAD[[autodiff_args$model_type]][[base_module_name]]
    FUN <- module[[fun_name]]  # e.g., module$predictions or module$predictions_byT

    # Build arguments (without FUN)
    args <- list(
        beta = coefs,
        X = X,
        vcov = vcov_matrix,
        groups = groups,
        num_groups = num_groups,
        family_type = autodiff_args$family_type,
        link_type = autodiff_args$link_type
    )
    args <- Filter(function(x) !is.null(x), args)

    # Call Python function using eval_fun_with_numpy_arrays
    result <- do.call(eval_fun_with_numpy_arrays, c(list(FUN = FUN), args))

    # Convert to R objects
    J <- as.matrix(result[["jacobian"]])

    # Ensure jacobian is (n_predictions x n_coefs), transpose if needed
    # Only transpose if we have (n_coefs x 1) instead of (1 x n_coefs)
    if (nrow(J) == length(coefs) && ncol(J) == 1) {
        J <- t(J)
    }

    out <- list(
        estimate = as.vector(result[["estimate"]]),
        std.error = as.vector(result[["std_error"]]),
        jacobian = J
    )

    # Add column names to jacobian
    if (!is.null(names(coefs)) && ncol(out$jacobian) == length(coefs)) {
        colnames(out$jacobian) <- names(coefs)
    }

    if (isTRUE(getOption("marginaleffects_autodiff_message", default = FALSE))) {
        message("\nJAX is fast!")
    }

    return(out)
}


#' Compute comparisons with JAX autodiff (estimates, SE, jacobian)
#' @return List with estimate, std.error, jacobian, or NULL if unsupported
#' @keywords internal
#' @noRd
jax_comparisons <- function(mfx, vcov_matrix, hi, lo, original, ...) {
    mAD <- settings_get("mAD")

    # Validate
    autodiff_args <- get_autodiff_args(mfx@model, mfx)
    if (is.null(autodiff_args)) return(NULL)

    # Check unsupported features
    if (!is.null(mfx@hypothesis)) {
        autodiff_warning("the `hypothesis` argument")
        return(NULL)
    }

    if (!is.character(mfx@comparison) || !mfx@comparison %in% c("difference", "ratio")) {
        comp_str <- if (is.character(mfx@comparison)) mfx@comparison else "custom function"
        autodiff_warning(sprintf("`comparison='%s'` (only 'difference' and 'ratio' supported)", comp_str))
        return(NULL)
    }

    # Ratio comparisons with by=TRUE compute ratio-then-average instead of average-then-ratio
    if (isTRUE(mfx@by) && mfx@comparison == "ratio") {
        autodiff_warning("`comparison='ratio'` with `by=TRUE` (averaging order differs from finite differences)")
        return(NULL)
    }

    if (!isTRUE(mfx@by) && !isFALSE(mfx@by) && !is.character(mfx@by)) {
        autodiff_warning("values of `by` other than TRUE, FALSE, or a character vector")
        return(NULL)
    }

    # Get model matrices
    X_hi <- attr(hi, "marginaleffects_model_matrix")
    X_lo <- attr(lo, "marginaleffects_model_matrix")
    if (is.null(X_hi) || is.null(X_lo)) return(NULL)

    # Get coefficients
    coefs <- get_coef(mfx@model, ...)

    # Check for NA coefficients (e.g., from aliased terms)
    if (anyNA(coefs)) {
        autodiff_warning("models with NA coefficients (aliased terms)")
        return(NULL)
    }

    # Map comparison type
    comparison_type <- switch(mfx@comparison,
        difference = mAD$comparisons$ComparisonType$DIFFERENCE,
        ratio = mAD$comparisons$ComparisonType$RATIO
    )

    # Determine aggregation function
    if (isFALSE(mfx@by)) {
        fun_name <- "comparisons"
        groups <- NULL
        num_groups <- NULL
    } else {
        # Both by=TRUE and by=character use comparisons_byG with grouping
        # by=TRUE aggregates by term/contrast, by=character adds user-specified variables
        fun_name <- "comparisons_byG"
        # Prepare group indices (includes contrast, term, and optionally user variables)
        group_result <- jax_align_group_J("jacobian_byG", mfx, original, NULL, NULL, X_hi, X_lo)
        groups <- group_result$groups
        num_groups <- group_result$num_groups
        X_hi <- group_result$X_hi
        X_lo <- group_result$X_lo

        # If groups couldn't be created, fall back to finite differences
        if (is.null(groups) || is.null(num_groups)) {
            return(NULL)
        }
    }

    # Select autodiff function
    # e.g., mAD$linear$comparisons is a module containing comparisons(), comparisons_byT(), comparisons_byG()
    # The base module name (predictions or comparisons)
    base_module_name <- if (grepl("predictions", fun_name)) "predictions" else "comparisons"
    module <- mAD[[autodiff_args$model_type]][[base_module_name]]
    FUN <- module[[fun_name]]  # e.g., module$comparisons or module$comparisons_byT

    # Build arguments (without FUN)
    args <- list(
        beta = coefs,
        X_hi = X_hi,
        X_lo = X_lo,
        vcov = vcov_matrix,
        comparison_type = comparison_type,
        groups = groups,
        num_groups = num_groups,
        family_type = autodiff_args$family_type,
        link_type = autodiff_args$link_type
    )
    args <- Filter(function(x) !is.null(x), args)

    # Call Python function using eval_fun_with_numpy_arrays
    result <- do.call(eval_fun_with_numpy_arrays, c(list(FUN = FUN), args))

    # Convert to R
    J <- as.matrix(result[["jacobian"]])

    # Ensure jacobian is (n_comparisons x n_coefs), transpose if needed
    # Only transpose if we have (n_coefs x 1) instead of (1 x n_coefs)
    if (nrow(J) == length(coefs) && ncol(J) == 1) {
        J <- t(J)
    }

    out <- list(
        estimate = as.vector(result[["estimate"]]),
        std.error = as.vector(result[["std_error"]]),
        jacobian = J
    )

    if (!is.null(names(coefs)) && ncol(out$jacobian) == length(coefs)) {
        colnames(out$jacobian) <- names(coefs)
    }

    if (isTRUE(getOption("marginaleffects_autodiff_message", default = FALSE))) {
        message("\nJAX is fast!")
    }

    return(out)
}


#' EXPERIMENTAL -- Enable Automatic Differentiation with JAX
#'
#' This function enables or disables automatic differentiation using the JAX
#' package in Python, which can considerably speed up and increase the accuracy
#' of standard errors when a model includes many parameters.
#'
#' @param autodiff Logical flag. If `TRUE`, enables automatic differentiation
#'   with JAX. If `FALSE` (default), disables automatic differentiation and
#'   reverts to finite difference methods.
#' @param install Logical flag. If `TRUE`, installs the `marginaleffects`
#'   Python package via `reticulate::py_install()`. Default is `FALSE`. This is
#'   only necessary if you are self-managing a Python installation.
#'
#' @details
#'
#' Automatic differentiation needs to be enabled once per session.
#'
#' When `autodiff = TRUE`, this function:
#' - Imports the `marginaleffects.autodiff` Python module via [reticulate::import()]
#' - Sets the internal jacobian function to use JAX-based automatic differentiation
#' - Provides faster and more accurate gradient computation for supported models
#' - Falls back on the default finite difference method for unsupported models and calls.
#'
#' Currently supports:
#' - Model types: `lm`, `glm`, `ols`, `lrm`
#' - Functions: [predictions()] and [comparisons()], along with `avg_` and `plot_` variants.
#' - `type`: "response" or "link"
#' - `by`: `TRUE`, `FALSE`, or character vector.
#' - `comparison`: "difference" and "ratio"
#'
#' For unsupported models or options, the function automatically falls back to
#' finite difference methods with a warning.
#'
#' # Python Configuration
#'
#' By default, no manual configuration of Python should be necessary. On most
#' machines, unless you have explicitly configured `reticulate`, reticulate
#' defaults to an automatically managed ephemeral virtual environment with all
#' Python requirements declared via `reticulate::py_require()`.
#'
#' If you prefer to use a manually managed Python installation, you can direct
#' `reticulate` and specify which Python executable or environment to use.
#' `reticulate` selects a Python installation using its [Order of
#' Discovery](https://rstudio.github.io/reticulate/articles/versions.html#order-of-discovery).
#' As a convenience `autodiff(install=TRUE)` will install the `marginaleffects` Python
#' package in a self-managed virtual environment.
#'
#' To specify an alternate Python version:
#' ```r
#' library(reticulate)
#' use_python("/usr/local/bin/python")
#' ```
#'
#' To use a virtual environment:
#' ```r
#' use_virtualenv("myenv")
#' ```
#'
#' These configuration commands should be called before calling `autodiff()`.
#'
#' @return No return value. Called for side effects of enabling/disabling
#'   automatic differentiation.
#'
#' @examples
#' \dontrun{
#' # Install the Python package (only needed once)
#' autodiff(install = TRUE)
#'
#' # Enable automatic differentiation
#' autodiff(TRUE)
#'
#' # Fit a model and compute marginal effects
#' mod <- glm(am ~ hp + wt, data = mtcars, family = binomial)
#' avg_comparisons(mod) # Will use JAX for faster computation
#'
#' # Disable automatic differentiation
#' autodiff(FALSE)
#' }
#'
#' @export
autodiff <- function(autodiff = NULL, install = FALSE) {
    checkmate::assert_flag(autodiff, null.ok = TRUE)
    checkmate::assert_flag(install)
    insight::check_if_installed("reticulate")
    if (isTRUE(install)) {
        reticulate::py_install("marginaleffects")
    }
    if (isFALSE(autodiff)) {
        settings_set("autodiff", FALSE)
    } else if (isTRUE(autodiff)) {
        mAD <- reticulate::import("marginaleffects.autodiff", delay_load = FALSE)
        settings_set("mAD", mAD)
        settings_set("autodiff", TRUE)
    }
}
