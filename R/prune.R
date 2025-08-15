#' @importFrom generics prune
#' @export
generics::prune


#' Prune marginaleffects objects to reduce memory usage
#'
#' Remove large attributes from marginaleffects objects to reduce memory usage.
#' Warning: This will disable many useful post-processing features of `marginaleffects`
#' @param tree A marginaleffects object (predictions, comparisons, slopes, or hypotheses)
#' @param ... Unused
#' @return A pruned marginaleffects object
#' @details ...
#' @export
prune.marginaleffects <- function(tree, ...) {
    mfx <- attr(tree, "mfx")
    if (!is.null(mfx)) {
        mfx@model <- NULL
        mfx@newdata <- NULL
        mfx@modeldata <- NULL
        mfx@call <- NULL
        mfx@jacobian <- matrix()
        attr(tree, "mfx") <- mfx
    }
    essential_attrs <- c("names", "row.names", "class", "mfx")
    for (nm in setdiff(names(attributes(tree)), essential_attrs)) {
        attr(tree, nm) <- NULL
    }
    attr(tree, "lean") <- TRUE
    return(tree)
}

#' @export
prune.predictions <- prune.marginaleffects

#' @export
prune.hypotheses <- prune.marginaleffects

#' @export
prune.slopes <- prune.marginaleffects

#' @export
prune.comparisons <- prune.marginaleffects
