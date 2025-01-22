#' Download and Read Datasets from `marginaleffects` or Rdatasets
#'
#' @description
#' Downloads a dataset from the `marginaleffects` or the Rdatasets archives, and return it as a data frame. Opens the documentation as an HTML page. Search available datasets.
#'
#' [https://vincentarelbundock.github.io/Rdatasets/](https://vincentarelbundock.github.io/Rdatasets/)
#'
#' @param dataset String. Name of the dataset to download.
#' @param package String. Package name that originally published the data.
#' @param docs Logical. If TRUE open the documentation using `getOption("viewer")` or the Rstudio viewer.
#' @param search Regular expression. Download the dataset index from Rdatasets; search the "Package", "Item", and "Title" columns; and return the matching rows.
#' @return A data frame containing the dataset.
#' @export
get_dataset <- function(
    dataset = "thornton",
    package = "marginaleffects",
    docs = FALSE,
    search = NULL) {
    checkmate::assert_string(package)
    checkmate::assert_string(dataset)
    checkmate::assert_flag(docs)
    checkmate::assert_string(search, null.ok = TRUE)

    if (!is.null(search)) {
        idx <- settings_get("get_dataset_index")
        if (is.null(idx)) {
            url <- "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/datasets.csv"
            idx <- read.csv(url)
        }
        idx <- idx[grepl(search, idx$Item) | grepl(search, idx$Package) | grepl(search, idx$Title), ,
            drop = FALSE]
        if (nrow(idx) > 0) {
            return(idx)
        } else {
            stop("Not dataset matches this regular expression.", call. = FALSE)
        }
    }

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
        temp_doc <- tempfile(fileext = ".html")
        download.file(documentation, temp_doc, mode = "w", quiet = TRUE)

        if (requireNamespace("rstudioapi")) {
            if (isTRUE(rstudioapi::isAvailable())) {
                rstudioapi::viewer(temp_doc)
            }
        }
        msg <- "Please choose a default browser with a command like: `options(browser = 'firefox')`"
        if (identical(getOption("browser"), "")) stop(msg, call. = FALSE)

        viewer <- getOption("viewer", utils::browseURL)
        if (!is.function(viewer)) stop(msg, call. = FALSE)

        viewer(temp_doc)
        return(invisible(NULL))
    } else {
        return(data)
    }
}
