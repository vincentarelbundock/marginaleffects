get_comparisons_data_character <- function(
    model,
    newdata,
    variable,
    cross,
    first_cross,
    modeldata,
    ...
) {
    # factors store all levels, but characters do not, so we need to extract the
    # original data from the model.
    tmp <- modeldata

    # unsupported by insight (e.g., numpyro)
    if (is.null(tmp)) {
        tmp <- newdata
    }

    levs <- sort(unique(tmp[[variable$name]]))

    contrast_categories(
        variable = variable,
        levs = levs,
        newdata = newdata,
        modeldata = modeldata,
        first_cross = first_cross,
        as_factor = FALSE,
        numeric_pair = function(v) {
            data.table::data.table(lo = as.character(v[1]), hi = as.character(v[2]))
        }
    )
}
