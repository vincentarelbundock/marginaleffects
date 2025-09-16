# Global variable to avoid R CMD check note
utils::globalVariables("mAD")

eval_fun_with_numpy_arrays <- function(FUN, ...) {
    dots <- list(...)
    # Handle special cases for JAX indexing
    mAD <- settings_get("mAD")
    for (i in seq_along(dots)) {
        if (names(dots)[i] %in% c("num_groups", "link_type", "family_type", "comparison_type")) {
            # Keep num_groups, link_type, family_type as is (integer scalars)
        } else if (names(dots)[i] == "groups") {
            # Convert groups to integer array explicitly
            dots[[i]] <- reticulate::np_array(dots[[i]], dtype = "int32")
        } else {
            dots[[i]] <- mAD$array(dots[[i]])
        }
    }
    J <- do.call(FUN, dots)
    J <- mAD$array(J)
    return(J)
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


get_jax_by <- function(mfx, original = NULL) {
    if (isTRUE(mfx@by)) {
        if (!is.null(original)) {
            # comparisons() aggregates by `contrast`, `term`, etc.
            out <- "jacobian_byG"
        } else {
            # predictions() gives global aggregation
            out <- "jacobian_byT"
        }
    } else if (isFALSE(mfx@by)) {
        out <- "jacobian"
    } else if (is.character(mfx@by)) {
        out <- "jacobian_byG"
    } else {
        autodiff_warning("values of `by` other than TRUE, FALSE, or a character vector of grouping variable names.")
        out <- NULL
    }
    return(out)
}


jax_align_group_J <- function(jac_fun, mfx, original, estimates, X, X_hi, X_lo) {
    if (isTRUE(grepl("_byG", jac_fun))) {
        bycols <- NULL
        # comparisons aggregates by contrast
        # the order must match the order in marginaleffects::comparisons()
        if (is.character(mfx@by)) {
            bycols <- c(bycols, mfx@by)
        }
        if (!is.null(original)) {
            bycols <- c(bycols, grep("^contrast|^term$|^group$", colnames(original), value = TRUE))
        }

        # Use the ordering from the final estimates object which has already been processed by get_by()
        if (!is.null(estimates) && !is.null(original)) {
            # Create a mapping from original data to final estimates groups
            # The estimates object has the final groups in the correct order
            if (length(bycols) > 0) {
                # Get group info from estimates (final order)
                estimates_groups <- estimates[, ..bycols, drop = FALSE]
                estimates_combined <- apply(estimates_groups, 1, function(x) paste0(x, collapse = "_"))

                # Get group info from original data (input order)
                original_groups <- original[, ..bycols, drop = FALSE]
                original_combined <- apply(original_groups, 1, function(x) paste0(x, collapse = "_"))

                # Map original rows to estimates group indices
                groups <- match(original_combined, estimates_combined) - 1L
                num_groups <- length(estimates_combined)
            } else {
                groups <- num_groups <- NULL
            }
        } else {
            # Fallback to original logic if estimates not provided
            if (!is.null(original)) {
                groups <- subset(original, select = bycols)
            } else {
                groups <- subset(mfx@newdata, select = bycols)
            }
            idx <- do.call(order, groups)
            groups <- groups[idx, , drop = FALSE]
            if (!is.null(X)) X <- X[idx, , drop = FALSE]
            if (!is.null(X_hi)) X_hi <- X_hi[idx, , drop = FALSE]
            if (!is.null(X_lo)) X_lo <- X_lo[idx, , drop = FALSE]
            groups <- apply(groups, 1, function(x) paste0(x, collapse = "_"))
            groups <- as.integer(as.factor(groups)) - 1L
            num_groups <- max(groups) + 1L
        }
    } else {
        groups <- num_groups <- NULL
    }

    return(list(groups = groups, num_groups = num_groups, X = X, X_hi = X_hi, X_lo = X_lo))
}


jax_jacobian <- function(coefs, mfx, hi = NULL, lo = NULL, original = NULL, estimates = NULL, ...) {
    mAD <- settings_get("mAD")

    if (isTRUE(getOption("marginaleffects_autodiff_message", default = FALSE))) {
        message("\nJAX is fast!")
    }

    # Check arguments not supported by any model
    if (!isTRUE(mfx@by) && !isFALSE(mfx@by) && !is.character(mfx@by)) {
        autodiff_warning("values of `by` other than TRUE, FALSE, or a character vector of grouping variable names.")
        return(NULL)
    }

    if (!is.null(mfx@hypothesis)) {
        autodiff_warning("the `hypothesis` argument")
        return(NULL)
    }

    if (!mfx@calling_function %in% c("predictions", "comparisons")) {
        autodiff_warning("other functions than predictions() or comparisons()")
        return(NULL)
    }

    if (identical(mfx@calling_function, "comparisons")) {
        if (!is.character(mfx@comparison) || !mfx@comparison %in% c("difference", "ratio")) {
            autodiff_warning("`comparison` values other than 'difference' or 'ratio'")
            return(NULL)
        }
        comparison_type <- switch(mfx@comparison,
            difference = mAD$comparisons$ComparisonType$DIFFERENCE,
            ratio = mAD$comparisons$ComparisonType$RATIO
        )
    } else {
        comparison_type <- NULL
    }

    # Check arguments for specific models
    autodiff_args <- get_autodiff_args(mfx@model, mfx)
    if (is.null(autodiff_args)) {
        return(NULL)
    }


    # Extract information from autodiff_args
    jac_fun <- get_jax_by(mfx = mfx, original = original)
    if (is.null(jac_fun)) {
        return(NULL)
    }

    X <- attr(mfx@newdata, "marginaleffects_model_matrix")
    X_hi <- attr(hi, "marginaleffects_model_matrix")
    X_lo <- attr(lo, "marginaleffects_model_matrix")

    if (mfx@calling_function == "predictions" && is.null(X)) {
        return(NULL)
    } else if (mfx@calling_function == "comparisons" && is.null(X_hi)) {
        return(NULL)
    }

    # Use the extracted function to handle group alignment
    group_result <- jax_align_group_J(jac_fun, mfx, original, estimates, X, X_hi, X_lo)
    groups <- group_result$groups
    num_groups <- group_result$num_groups
    X <- group_result$X
    X_hi <- group_result$X_hi
    X_lo <- group_result$X_lo

    # Get the appropriate function based on model type
    FUN <- mAD[[autodiff_args$model_type]][[mfx@calling_function]][[jac_fun]]

    args <- list(
        FUN = FUN,
        beta = coefs,
        X = X,
        X_hi = X_hi,
        X_lo = X_lo,
        groups = groups,
        num_groups = num_groups,
        comparison_type = comparison_type,
        family_type = autodiff_args$family_type,
        link_type = autodiff_args$link_type
    )
    args <- Filter(function(x) !is.null(x), args)
    J <- do.call(eval_fun_with_numpy_arrays, args)
    if (length(dim(J)) == 1) {
        J <- matrix(as.vector(J), nrow = 1)
    }
    if (!is.null(names(coefs)) && length(coefs) == ncol(J)) {
        colnames(J) <- names(coefs)
    }
    return(J)
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
#' @param install Logical flag. If `TRUE`, installs the `marginaleffectsAD`
#'   Python package via `reticulate::py_install()`. Default is `FALSE`. This is
#'   only necessary if you are self-managing a Python installation.
#'
#' @details
#'
#' Automatic differentiation needs to be enabled once per session.
#'
#' When `autodiff = TRUE`, this function:
#' - Imports the `marginaleffectsAD` Python package via [reticulate::py_install()]
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
#' As a convenience `autodiff(install=TRUE)` will install `marginaleffectsAD` in
#' a self-managed virtual environment.
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
        reticulate::py_install("marginaleffectsAD")
    }
    if (isFALSE(autodiff)) {
        settings_set("autodiff", FALSE)
        settings_set("jacobian_function", NULL)
    } else if (isTRUE(autodiff)) {
        mAD <- reticulate::import("marginaleffectsAD", delay_load = FALSE)
        settings_set("mAD", mAD)
        settings_set("autodiff", TRUE)
        settings_set("jacobian_function", jax_jacobian)
    }
}
