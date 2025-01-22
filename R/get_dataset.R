#' Download and Read Dataset
#'
#' @description Downloads a dataset and reads it into a data frame.
#' @param dataset A string, either "military" or "thornton".
#' @param docs A logical, if TRUE open the documentation using `getOption("viewer")`
#' @return A data frame containing the dataset.
#' @export
get_dataset <- function(
    dataset = "thornton",
    package = "marginaleffects",
    docs = FALSE) {
    checkmate::assert_string(package)
    checkmate::assert_string(dataset)
    checkmate::assert_flag(docs)

    # marginaleffects
    if (identical(package, "marginaleffects")) {
        insight::check_if_installed("nanoparquet")

        checkmate::assert_choice(dataset, c("affairs", "airbnb", "immigration", "military", "thornton"))

        data <- switch(dataset,
            "affairs" = "https://marginaleffects.com/data/affairs.parquet",
            "airbnb" = "https://marginaleffects.com/data/airbnb.parquet",
            "immigration" = "https://marginaleffects.com/data/immigration.parquet",
            "military" = "https://marginaleffects.com/data/military.parquet",
            "thornton" = "https://marginaleffects.com/data/thornton.parquet"
        )
        temp_file <- tempfile(fileext = ".parquet")
        download.file(data, temp_file, mode = "wb", quiet = TRUE)
        data <- nanoparquet::read_parquet(temp_file)

        documentation <- switch(dataset,
            "affairs" = "https://marginaleffects.com/data/affairs.html",
            "airbnb" = "https://marginaleffects.com/data/airbnb.html",
            "immigration" = "https://marginaleffects.com/data/immigration.html",
            "military" = "https://marginaleffects.com/data/military.html",
            "thornton" = "https://marginaleffects.com/data/thornton.html"
        )

        # Rdatasets
    } else {
        data <- "https://vincentarelbundock.github.io/Rdatasets/csv/%s/%s.csv"
        data <- sprintf(data, package, dataset)
        data <- tryCatch(data.table::fread(data, showProgress = FALSE), error = function(e) NULL)
        if (!inherits(data, "data.table")) stop("Unable to download this dataset.", call. = FALSE)
        data.table::setDF(data)

        documentation <- "https://vincentarelbundock.github.io/Rdatasets/doc/%s/%s.html"
        documentation <- sprintf(documentation, package, dataset)
    }

    if (docs) {
        msg <- "Please choose a default browser with a command like: `options(browser = 'firefox')`"
        if (identical(getOption("browser"), "")) stop(msg, call. = FALSE)

        viewer <- getOption("viewer", utils::browseURL)
        if (!is.function(viewer)) stop(msg, call. = FALSE)

        viewer(documentation)
        return(invisible(NULL))
    } else {
        return(data)
    }
}
