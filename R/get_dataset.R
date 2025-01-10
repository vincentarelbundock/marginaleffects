#' Download and Read Dataset
#'
#' @description Downloads a dataset and reads it into a data frame.
#' @param dataset A string, either "military" or "thornton".
#' @param docs A logical, if TRUE returns a link to the documentation.
#' @return A data frame containing the dataset.
#' @import nanoparquet
#' @export
get_dataset <- function(dataset, docs = FALSE) {
    insight::check_if_installed("nanoparquet")
    checkmate::assert_choice(dataset, c("military", "thornton"))

    if (docs) {
        return(switch(dataset,
            "military" = "https://marginaleffects.com/data/military.html",
            "thornton" = "https://marginaleffects.com/data/thornton.html"
        ))
    }

    url <- switch(dataset,
        "military" = "https://marginaleffects.com/data/military.parquet",
        "thornton" = "https://marginaleffects.com/data/thornton.parquet"
    )

    temp_file <- tempfile(fileext = ".parquet")
    download.file(url, temp_file, mode = "wb", quiet = TRUE)

    data <- nanoparquet::read_parquet(temp_file)
    return(data)
}
