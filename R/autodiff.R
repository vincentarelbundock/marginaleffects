eval_fun_with_numpy_arrays <- function(FUN, ...) {
    dots <- list(...)
    # Handle special cases for JAX indexing
    for (i in seq_along(dots)) {
        if (names(dots)[i] == "num_groups") {
            # Keep num_groups as is (integer scalar)
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


sanity_jax_hypothesis <- function(mfx) {
    if (!is.null(mfx@hypothesis)) {
        autodiff_warning("the `hypothesis` argument")
        return(FALSE)
    }
    return(TRUE)
}


sanity_jax_type <- function(mfx) {
    if (!identical(mfx@type, "response")) {
        autodiff_warning("`type` other than 'response'")
        return(FALSE)
    }
    return(TRUE)
}


get_jax_function <- function(mfx) {
    if (mfx@calling_function == "predictions") {
        return("predictions")
    } else if (mfx@calling_function == "comparisons") {
        return("comparisons")
    } else {
        autodiff_warning("other functions than predictions() or comparisons()")
        return(NULL)
    }
}


get_jax_model <- function(mfx) {
    model <- mfx@model
    if (class(model)[1] == "lm") {
        return("linear")
    } else if (class(model)[1] == "glm") {
        if (model$family$family == "binomial" && model$family$link == "logit") {
            return("logit")
        } else if (model$family$family == "binomial" && model$family$link == "probit") {
            return("probit")
        } else if (model$family$family == "poisson" && model$family$link == "log") {
            return("poisson")
        }
    }
    autodiff_warning(paste("models of class", class(model)[1]))
    return(NULL)
}


get_jax_by <- function(mfx, hi = NULL) {
    if (isTRUE(mfx@by)) {
        if (!is.null(hi)) {
            # comparisons() aggregates by `contrast`, `term`, etc.
            return("_byG")
        } else {
            # predictions() gives global aggregation
            return("_byT")
        }
    } else if (isFALSE(mfx@by)) {
        return("")
    } else if (is.character(mfx@by)) {
        return("_byG")
    } else {
        autodiff_warning("values of `by` other than TRUE, FALSE, or a character vector of grouping variable names.")
        return(NULL)
    }
    return(estimand)
}


get_jax_estimand <- function(mfx) {
    if (mfx@calling_function == "predictions") {
        return("jacobian")
    } else if (mfx@calling_function == "comparisons") {
        if (mfx@comparison == "difference") {
            return("jacobian_difference")
        } else if (mfx@comparison == "ratio") {
            return("jacobian_ratio")
        }
    }
    autodiff_warning("other functions than `predictions()` or `comparisons()`, with `comparisons='ifference'` or `'ratio'`")
    return(NULL)
}


jax_jacobian <- function(coefs, mfx, hi = NULL, lo = NULL, ...) {
    message("\nJAX is fast!")

    f <- get_jax_function(mfx = mfx)
    m <- get_jax_model(mfx = mfx)
    b <- get_jax_by(mfx = mfx, hi = hi)
    e <- get_jax_estimand(mfx = mfx)

    if (identical(b, "_byG")) {
        bycols <- NULL
        # comparisons aggregates by contrast
        if (!is.null(hi)) {
            bycols <- c(bycols, grep("^contrast|^term$|^group$", colnames(hi), value = TRUE))
        }
        if (is.character(mfx@by)) {
            bycols <- c(bycols, mfx@by)
        }
        if (!is.null(hi)) {
            groups <- hi[, ..bycols, drop = FALSE]
        } else {
            groups <- mfx@newdata[, ..bycols, drop = FALSE]
        }
        groups <- apply(groups, 1, function(x) paste0(x, collapse = "_"))
        groups <- as.integer(as.factor(groups)) - 1L
        num_groups <- as.integer(max(groups) + 1L)
    } else {
        groups <- num_groups <- NULL
    }

    if (is.null(f)) {
        return(NULL)
    }
    if (is.null(m)) {
        return(NULL)
    }
    if (is.null(b)) {
        return(NULL)
    }
    if (is.null(e)) {
        return(NULL)
    }
    if (isFALSE(sanity_jax_hypothesis(mfx))) {
        return(NULL)
    }
    if (isFALSE(sanity_jax_type(mfx))) {
        return(NULL)
    }

    args <- list(
        FUN = mAD[[m]][[f]][[paste0(e, b)]],
        beta = coefs,
        X = attr(mfx@newdata, "marginaleffects_model_matrix"),
        X_hi = attr(hi, "marginaleffects_model_matrix"),
        X_lo = attr(lo, "marginaleffects_model_matrix"),
        groups = groups,
        num_groups = num_groups
    )
    args <- Filter(function(x) !is.null(x), args)
    J <- do.call(eval_fun_with_numpy_arrays, args)
    if (length(dim(J)) == 1) {
        J <- matrix(as.vector(J), nrow = 1)
    }
    return(J)
}


#' [EXPERIMENTAL] Enable Automatic Differentiation with JAX
#'
#' This function enables or disables automatic differentiation using the JAX
#' package in Python, which can considerably speed up and increase the accuracy
#' of standard errors when a model includes many parameters.
#'
#' @param autodiff Logical flag. If `TRUE`, enables automatic differentiation
#'   with JAX. If `FALSE` (default), disables automatic differentiation and
#'   reverts to finite difference methods.
#' @param install Logical flag. If `TRUE`, installs the `marginaleffectsAD`
#'   Python package via `reticulate::py_install()`. Default is `FALSE`.
#'
#' @details
#' When `autodiff = TRUE`, this function:
#' - Imports the `marginaleffectsAD` Python package via `reticulate`
#' - Sets the internal jacobian function to use JAX-based automatic differentiation
#' - Provides faster and more accurate gradient computation for supported models
#'
#' Currently supports:
#' - Model types: `lm`, `glm` with binomial (logit/probit) and Poisson families
#' - Functions: `predictions()` and `comparisons()`
#' - Comparison types: "difference" and "ratio"
#' - Only `type = "response"` predictions
#' - `by = TRUE` or `by = FALSE` grouping
#'
#' For unsupported models or options, the function automatically falls back to
#' finite difference methods with a warning.
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
#' autodiff(autodiff = TRUE)
#'
#' # Fit a model and compute marginal effects
#' mod <- glm(am ~ hp + wt, data = mtcars, family = binomial)
#' slopes(mod) # Will use JAX for faster computation
#'
#' # Disable automatic differentiation
#' autodiff(autodiff = FALSE)
#' }
#'
#' @seealso [settings_set()], [predictions()], [comparisons()]
#'
#' @export
autodiff <- function(autodiff = FALSE, install = FALSE) {
    checkmate::assert_flag(autodiff)
    checkmate::assert_flag(install)
    if (isTRUE(install)) {
        reticulate::py_install("marginaleffectsAD")
    }
    if (isFALSE(autodiff)) {
        settings_set("jacobian_function", NULL)
    } else {
        insight::check_if_installed("reticulate")
        mAD <<- reticulate::import("marginaleffectsAD", delay_load = TRUE)
        settings_set("jacobian_function", jax_jacobian)
    }
}
