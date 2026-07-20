# Global variable to avoid R CMD check note
utils::globalVariables("mAD")

autodiff_warning <- function(feature) {
    msg <- "Automatic differentiation with JAX does not support %s. Reverting to finite difference."
    warning(sprintf(msg, feature), call. = FALSE)
}

autodiff_pipeline_call <- function(spec, coefs) {
    mAD <- settings_get("mAD")

    py_ops <- NULL
    if (!is.null(spec$ops)) {
        py_ops <- lapply(spec$ops, function(op) {
            reticulate::dict(
                op = op$op,
                n = as.integer(op$n),
                w = if (is.null(op$w)) NULL else reticulate::np_array(op$w, dtype = "float64")
            )
        })
    }

    kwargs <- list(
        beta = reticulate::np_array(coefs, dtype = "float64"),
        model_type = spec$model$model_type,
        family = spec$model$family,
        link = spec$model$link,
        X = if (is.null(spec$X)) NULL else reticulate::np_array(spec$X, dtype = "float64"),
        X_hi = if (is.null(spec$X_hi)) NULL else reticulate::np_array(spec$X_hi, dtype = "float64"),
        X_lo = if (is.null(spec$X_lo)) NULL else reticulate::np_array(spec$X_lo, dtype = "float64"),
        ops = py_ops,
        est_keep = if (is.null(spec$est_keep)) NULL else reticulate::np_array(as.integer(spec$est_keep - 1L), dtype = "int32"),
        agg_segments = if (is.null(spec$agg)) NULL else reticulate::np_array(as.integer(spec$agg$segments - 1L), dtype = "int32"),
        agg_num_segments = if (is.null(spec$agg)) NULL else as.integer(spec$agg$num_segments),
        agg_weights = if (is.null(spec$agg) || is.null(spec$agg$weights)) NULL else reticulate::np_array(spec$agg$weights, dtype = "float64"),
        H = if (is.null(spec$hyp)) NULL else reticulate::np_array(spec$hyp, dtype = "float64")
    )

    result <- do_call(mAD$pipeline$compute, kwargs)
    J <- as.matrix(result[["jacobian"]])
    if (ncol(J) == length(coefs)) {
        colnames(J) <- names(coefs)
    }

    list(
        estimate = as.vector(result[["estimate"]]),
        jacobian = J
    )
}

#' @keywords internal
#' @noRd
get_autodiff_args <- function(model, mfx, type) {
    UseMethod("get_autodiff_args")
}


#' @rdname get_autodiff_args
#' @keywords internal
#' @noRd
#' @export
get_autodiff_args.default <- function(model, mfx, type) {
    return(NULL)
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


#' EXPERIMENTAL -- Enable Automatic Differentiation with JAX
#'
#' This function enables or disables automatic differentiation using the JAX
#' package in Python, which can considerably speed up and increase the accuracy
#' of standard errors when a model includes many parameters.
#'
#' @param autodiff Logical flag. If `TRUE`, enables automatic differentiation
#'   with JAX. If `FALSE`, disables automatic differentiation and
#'   reverts to finite difference methods. If `NULL` (default), the function
#'   simply returns the current autodiff setting without changing it.
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
#' - Model types: `lm`, `glm`, `ols`
#' - Functions: [predictions()] and [comparisons()], along with `avg_` and `plot_` variants.
#' - `type`: "response" or "link"
#' - `by`: `TRUE`, `FALSE`, character vector, or data frame.
#' - `wts`: weights for averages and grouped estimates.
#' - `hypothesis`: numeric vectors and matrices.
#' - `comparison`: "difference" and "ratio", including `comparison = "ratio"` with `by = TRUE`.
#'
#' For unsupported models or options, the function automatically falls back to
#' the default finite difference method.
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
#' @return When `autodiff` is `NULL`, returns `TRUE` if autodiff is enabled and
#'   `FALSE` otherwise. Otherwise called for side effects of enabling or
#'   disabling automatic differentiation or installing the Python package.
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
    if (isTRUE(install) || isTRUE(autodiff)) {
        insight::check_if_installed("reticulate")
    }
    if (isTRUE(install)) {
        reticulate::py_install("marginaleffects")
    }
    if (is.null(autodiff)) {
        state <- isTRUE(settings_get("autodiff"))
        if (state) {
            cat("Autodiff is enabled.\n")
        } else {
            cat("Autodiff is disabled.\n")
        }
        return(invisible(state))
    }
    if (isFALSE(autodiff)) {
        settings_set("autodiff", FALSE)
    } else if (isTRUE(autodiff)) {
        mAD <- reticulate::import("marginaleffects.autodiff", delay_load = FALSE)
        settings_set("mAD", mAD)
        settings_set("autodiff", TRUE)
    }
}
