eval_fun_with_numpy_arrays <- function(FUN, ...) {
    dots <- list(...)
    # Handle special cases for JAX indexing
    for (i in seq_along(dots)) {
        if (names(dots)[i] %in% c("num_groups", "link_type", "family_type")) {
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


sanity_jax_hypothesis <- function(mfx) {
    if (!is.null(mfx@hypothesis)) {
        autodiff_warning("the `hypothesis` argument")
        return(FALSE)
    }
    return(TRUE)
}


sanity_jax_type <- function(mfx) {
    if (!mfx@type %in% c("response", "link", "invlink(link)")) {
        autodiff_warning(sprintf("`type='%s'`", mfx@type))
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
    supported <- c("lm", "glm")
    if (!class(model)[1] %in% supported) {
        autodiff_warning(paste("models of class", class(model)[1]))
        return(NULL)
    }

    if (identical(class(model)[1], "lm") || mfx@type %in% c("link", "invlink(link)")) {
        return("linear")
    } else if (class(model)[1] == "glm") {
        # Check if the GLM family/link combination is supported
        link_info <- get_jax_link_type(model)
        if (!is.null(link_info)) {
            return("glm")
        } else {
            autodiff_warning("unsupported GLM family/link combinations")
        }
    }
    return(NULL)
}


get_jax_link_type <- function(model) {
    if (class(model)[1] != "glm") {
        return(NULL)
    }

    # Import Family and Link enums from Python
    Family <- mAD$glm$families$Family
    Link <- mAD$glm$families$Link

    # Family types using Python enum
    family_type <- switch(model$family$family,
        "gaussian" = Family$GAUSSIAN,
        "binomial" = Family$BINOMIAL,
        "poisson" = Family$POISSON,
        "Gamma" = Family$GAMMA,
        NULL
    )

    # Link types using Python enum
    link_type <- switch(model$family$link,
        "identity" = Link$IDENTITY,
        "log" = Link$LOG,
        "logit" = Link$LOGIT,
        "probit" = Link$PROBIT,
        "inverse" = Link$INVERSE,
        "sqrt" = Link$SQRT,
        "cloglog" = Link$CLOGLOG,
        NULL
    )

    if (is.null(family_type) || is.null(link_type)) {
        return(NULL)
    }

    return(list(family_type = family_type, link_type = link_type))
}


get_jax_by <- function(mfx, original = NULL) {
    if (isTRUE(mfx@by)) {
        if (!is.null(original)) {
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


jax_jacobian <- function(coefs, mfx, hi = NULL, lo = NULL, original = NULL, estimates = NULL, ...) {
    if (isTRUE(getOption("marginaleffects_autodiff_message", default = FALSE))) {
        message("\nJAX is fast!")
    }

    f <- get_jax_function(mfx = mfx)
    m <- get_jax_model(mfx = mfx)
    b <- get_jax_by(mfx = mfx, original = original)
    e <- get_jax_estimand(mfx = mfx)

    X <- attr(mfx@newdata, "marginaleffects_model_matrix")
    X_hi <- attr(hi, "marginaleffects_model_matrix")
    X_lo <- attr(lo, "marginaleffects_model_matrix")

    # Check if we have the required matrices for the selected path
    if (identical(b, "_byG") && (is.null(X_hi) || is.null(X_lo))) {
        return(NULL) # Fall back to finite difference method
    }

    if (identical(b, "_byG")) {
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
                groups <- original[, ..bycols, drop = FALSE]
            } else {
                groups <- mfx@newdata[, ..bycols, drop = FALSE]
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

    if (is.null(f) || is.null(m) || is.null(b) || is.null(e)) {
        return(NULL)
    }

    if (isFALSE(sanity_jax_hypothesis(mfx)) || isFALSE(sanity_jax_type(mfx))) {
        return(NULL)
    }

    # Get family_type and link_type for GLM models
    family_type <- NULL
    link_type <- NULL
    if (m == "glm") {
        link_info <- get_jax_link_type(mfx@model)
        if (!is.null(link_info)) {
            family_type <- link_info$family_type
            link_type <- link_info$link_type
        }
    }

    # Get the appropriate function based on model type
    FUN <- mAD[[m]][[f]][[paste0(e, b)]]

    args <- list(
        FUN = FUN,
        beta = coefs,
        X = X,
        X_hi = X_hi,
        X_lo = X_lo,
        groups = groups,
        num_groups = num_groups,
        family_type = family_type,
        link_type = link_type
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
