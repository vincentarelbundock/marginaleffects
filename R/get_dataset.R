#' Download and Read Datasets from `marginaleffects` or Rdatasets
#'
#' @description
#' Downloads a dataset from the `marginaleffects` or the Rdatasets archives, and return it as a data frame. Opens the documentation as an HTML page. Search available datasets.
#'
#' [https://vincentarelbundock.github.io/Rdatasets/](https://vincentarelbundock.github.io/Rdatasets/)
#'
#' @param dataset String. Name of the dataset to download.
#'   - `marginaleffects` archive: `affairs`, `airbnb`, `ces_demographics`, `ces_survey`, `immigration`, `lottery`, `military`, `thornton`, `factorial_01`, `interaction_01`, `interaction_02`, `interaction_03`, `interaction_04`, `polynomial_01`, `polynomial_02`
#'   - Rdatasets archive: The name of a dataset listed on the Rdatasets index. See the website or the `search` argument.
#' @param package String. Package name that originally published the data.
#' @param docs Logical. If TRUE open the documentation using `getOption("viewer")` or the Rstudio viewer.
#' @param search Regular expression. Download the dataset index from Rdatasets; search the "Package", "Item", and "Title" columns; and return the matching rows.
#' @return A data frame containing the dataset.
#' library(marginaleffects)
#' @examples
#' dat <- get_dataset("Titanic", "Stat2Data")
#' head(dat)
#'
#' get_dataset(search = "(?i)titanic")
#'
#' # View documentation in the browser
#' # get_dataset("Titanic", "Stat2Data", docs = TRUE)
#' @export
get_dataset <- function(
    dataset = "thornton",
    package = NULL,
    docs = FALSE,
    search = NULL
) {
    checkmate::assert_string(dataset)
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(docs)
    checkmate::assert_string(search, null.ok = TRUE)

    if (!is.null(search)) {
        return(get_dataset_search(search))
    }

    # Define data dictionary for marginaleffects datasets
    data_dict <- c(
        "affairs" = "https://marginaleffects.com/data/affairs.parquet",
        "airbnb" = "https://marginaleffects.com/data/airbnb.parquet",
        "ces_demographics" = "https://marginaleffects.com/data/ces_demographics.parquet",
        "ces_survey" = "https://marginaleffects.com/data/ces_survey.parquet",
        "immigration" = "https://marginaleffects.com/data/immigration.parquet",
        "lottery" = "https://marginaleffects.com/data/lottery.parquet",
        "military" = "https://marginaleffects.com/data/military.parquet",
        "thornton" = "https://marginaleffects.com/data/thornton.parquet",
        "factorial_01" = "https://marginaleffects.com/data/factorial_01.parquet",
        "interaction_01" = "https://marginaleffects.com/data/interaction_01.parquet",
        "interaction_02" = "https://marginaleffects.com/data/interaction_02.parquet",
        "interaction_03" = "https://marginaleffects.com/data/interaction_03.parquet",
        "interaction_04" = "https://marginaleffects.com/data/interaction_04.parquet",
        "polynomial_01" = "https://marginaleffects.com/data/polynomial_01.parquet",
        "polynomial_02" = "https://marginaleffects.com/data/polynomial_02.parquet"
    )

    # If package is NULL, try to guess the correct source
    if (is.null(package)) {
        # First check if it's a marginaleffects dataset
        if (dataset %in% names(data_dict)) {
            package <- "marginaleffects"
        } else {
            # Try to find exact match in Rdatasets
            matches <- get_dataset_search(paste0("^", dataset, "$"))
            if (nrow(matches) == 1) {
                package <- matches$Package[1]
                dataset <- matches$Item[1]
            } else if (nrow(matches) > 1) {
                msg <- sprintf(
                    "Multiple matches found for dataset '%s'. Please specify the package name.\nAvailable options:\n%s",
                    dataset,
                    paste(
                        sprintf("  - %s::%s", matches$Package, matches$Item),
                        collapse = "\n"
                    )
                )
                stop(msg, call. = FALSE)
            } else {
                msg <- sprintf(
                    "Dataset '%s' not found. Please:\n1. Specify the package name, or\n2. Use get_dataset(search = '...') to search available datasets",
                    dataset
                )
                stop(msg, call. = FALSE)
            }
        }
    }

    # marginaleffects
    if (identical(package, "marginaleffects")) {
        insight::check_if_installed("nanoparquet")

        checkmate::assert_choice(dataset, names(data_dict))

        data <- data_dict[dataset]
        if (grepl("factorial|interaction|polynomial", dataset)) {
            documentation <- "https://marginaleffects.com/data/model_to_meaning_simulated_data.html"
        } else if (grepl("^ces", dataset)) {
            documentation <- "https://marginaleffects.com/data/ces.html"
        } else {
            documentation <- sub("parquet$", "html", data)
        }

        temp_file <- tempfile(fileext = ".parquet")
        utils::download.file(data, temp_file, mode = "wb", quiet = TRUE)
        data <- nanoparquet::read_parquet(temp_file)

        # Rdatasets
    } else {
        insight::check_if_installed("nanoparquet")
        temp_file <- tempfile(fileext = ".parquet")
        data <- "https://vincentarelbundock.github.io/Rdatasets/parquet/%s/%s.parquet"
        data <- sprintf(data, package, dataset)
        utils::download.file(data, temp_file, mode = "wb", quiet = TRUE)
        data <- nanoparquet::read_parquet(temp_file)
        data.table::setDF(data)
        documentation <- "https://vincentarelbundock.github.io/Rdatasets/doc/%s/%s.html"
        documentation <- sprintf(documentation, package, dataset)
    }

    if (docs) {
        temp_doc <- tempfile(fileext = ".html")
        utils::download.file(documentation, temp_doc, mode = "w", quiet = TRUE)

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

get_dataset_search <- function(search) {
    idx <- settings_get("get_dataset_index")
    if (is.null(idx)) {
        url <- "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/datasets.csv"
        idx <- utils::read.csv(url)
    }
    idx <- idx[
        grepl(search, idx$Item) |
            grepl(search, idx$Package) |
            grepl(search, idx$Title),
        ,
        drop = FALSE
    ]
    return(idx)
}
