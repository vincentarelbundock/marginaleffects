#' @importFrom generics prune
#' @export
generics::prune


#' Prune marginaleffects objects to reduce memory usage
#'
#' Remove large attributes from marginaleffects objects to reduce memory usage.
#' Warning: This will disable many useful post-processing features of `marginaleffects`
#' @param tree A marginaleffects object (predictions, comparisons, slopes, or hypotheses)
#' @param component A character string indicating which component to prune: "all" or "modeldata".
#' @param ... Unused
#' @return A pruned marginaleffects object
#' @details ...
#' @export
prune.marginaleffects <- function(tree, component = "all", ...) {
    checkmate::assert_choice(component, c("all", "modeldata"))

    mfx <- components(tree, "all")

    if (component == "all") {
        if (!is.null(mfx)) {
            mfx@model <- NULL
            mfx@newdata <- NULL
            mfx@modeldata <- NULL
            mfx@call <- NULL
            mfx@jacobian <- matrix()
        }
        essential_attrs <- c("names", "row.names", "class", "mfx")
        for (nm in setdiff(names(attributes(tree)), essential_attrs)) {
            attr(tree, nm) <- NULL
        }
        attr(tree, "lean") <- TRUE
    } else if (component == "modeldata") {
        fml <- hush(insight::find_formula(mfx@model))
        fml <- unlist(lapply(fml, all.vars))

        keepers <- c(
            fml,
            names(mfx@variables),
            mfx@variable_names_datagrid,
            mfx@variable_names_response,
            mfx@variable_names_wts,
            mfx@variable_names_by,
            mfx@variable_names_by_hypothesis
        )

        # fixest-specific syntax: i.groupid and ~weights
        # before name lookup in modeldata
        if (inherits(mfx@model, "fixest")) {
            keepers <- gsub("^~|^i\\.", "", keepers)
        }

        # preserve order
        keepers <- unique(intersect(names(mfx@modeldata), unique(keepers)))

        if (inherits(mfx@modeldata, "data.table")) {
            mfx@modeldata <- mfx@modeldata[, ..keepers, drop = FALSE]
        } else {
            mfx@modeldata <- mfx@modeldata[, keepers, drop = FALSE]
        }
    }

    attr(tree, "marginaleffects") <- mfx
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
