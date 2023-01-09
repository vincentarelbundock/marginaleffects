# #' Summarize a `marginaleffects` object
# #'
# #' @param object An object produced by the `slopes` function
# #' @inheritParams slopes
# #' @inheritParams averages.comparisons
# #' @template bayesian
# #' @return Data frame of summary statistics for an object produced by the
# #' `slopes` function
# #' @examples
# #' mod <- lm(mpg ~ hp * wt + factor(gear), data = mtcars)
# #' mfx <- slopes(mod)
# #'
# #' # average marginal effects
# #' summary(mfx)
# #' @name summary
# NULL


#' @export
summary.slopes <- function(object, ...) {
    out <- tidy(object, ...)
    class(out) <- c("slopes.summary", class(out))
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")
    return(out)
}


#' @export
summary.marginalmeans <- function(object, ...) {
    out <- tidy(object, ...)
    out$type <- NULL
    class(out) <- c("marginalmeans.summary", class(out))
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")
    attr(out, "variables") <- attr(object, "variables")
    attr(out, "variables_grid") <- attr(object, "variables_grid")
    return(out)
}


#' @export
summary.hypotheses <- function(object, ...) {
    out <- tidy(object, ...)
    class(out) <- c("hypotheses.summary", class(out))
    return(out)
}


#' @export
summary.predictions <- function(object, ...) {
    out <- averages(object, ...)
    class(out) <- c("predictions.summary", class(out))
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")

    return(out)
}


#' @export
summary.comparisons <- function(object, ...) {
    out <- averages(object, ...)
    class(out) <- c("comparisons.summary", class(out))
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")
    attr(out, "transform_pre") <- attr(object, "transform_pre")
    attr(out, "transform_post") <- attr(object, "transform_post")
    attr(out, "transform_post_label") <- attr(object, "transform_post_label")
    attr(out, "transform_pre_label") <- attr(object, "transform_pre_label")

    return(out)
}