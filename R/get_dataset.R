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
#' @examplesIf FALSE
#' dat <- get_dataset("Titanic", "Stat2Data")
#' head(dat)
#'
#' get_dataset(search = "(?i)titanic")
#'
#' # View documentation in the browser
#' get_dataset("Titanic", "Stat2Data", docs = TRUE)
#' @export
get_dataset <- function(
    dataset = "thornton",
    package = NULL,
    docs = FALSE,
    search = NULL) {
    checkmate::assert_string(dataset)
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(docs)
    checkmate::assert_string(search, null.ok = TRUE)
    insight::check_if_installed("Rdatasets")

    if (!is.null(search)) {
        return(get_dataset_search(search))
    }

    # Define data dictionary for marginaleffects datasets
    data_dict <- c(
        "affairs" = "https://marginaleffects.com/data/affairs.parquet",
        "airbnb" = "https://marginaleffects.com/data/airbnb.parquet",
        "cameras" = "https://marginaleffects.com/data/cameras.parquet",
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

    # Check if it's a marginaleffects dataset first
    if (is.null(package) && dataset %in% names(data_dict)) {
        package <- "marginaleffects"
    }

    # Handle marginaleffects datasets or delegate to Rdatasets
    if (identical(package, "marginaleffects")) {
        if (docs) {
            get_dataset_docs(dataset, data_dict)
            return(invisible(NULL))
        } else {
            rdpath <- getOption("marginaleffects_website_path", default = NULL)
            if (is.null(rdpath)) {
                return(Rdatasets::rddata(dataset, package))
            }

            insight::check_if_installed("nanoparquet")
            out <- nanoparquet::read_parquet(file.path(
                rdpath, "data", paste0(dataset, ".parquet")))
            return(out)
            return(get_dataset_data(dataset, data_dict))
        }
    } else {
        if (docs) {
            Rdatasets::rddocs(dataset, package)
            return(invisible(NULL))
        } else {
            rdpath <- getOption("marginaleffects_rdataset_path", default = NULL)
            if (is.null(rdpath)) {
                return(Rdatasets::rddata(dataset, package))
            }

            insight::check_if_installed("nanoparquet")
            out <- nanoparquet::read_parquet(file.path(
                rdpath, "parquet", package,
                paste0(dataset, ".parquet")))
            return(out)
        }
    }
}

get_dataset_search <- function(search) {
    # Use Rdatasets package to search all fields
    idx <- Rdatasets::rdsearch(search)
    # Convert column names to match the original format for compatibility
    if (nrow(idx) > 0 && "Dataset" %in% names(idx)) {
        names(idx)[names(idx) == "Dataset"] <- "Item"
    }
    return(idx)
}

get_dataset_data <- function(dataset, data_dict) {
    insight::check_if_installed("nanoparquet")
    checkmate::assert_choice(dataset, names(data_dict))

    data_url <- data_dict[dataset]
    temp_file <- tempfile(fileext = ".parquet")
    utils::download.file(data_url, temp_file, mode = "wb", quiet = TRUE)
    data <- nanoparquet::read_parquet(temp_file)
    return(data)
}

get_dataset_docs <- function(dataset, data_dict) {
    checkmate::assert_choice(dataset, names(data_dict))

    data_url <- data_dict[dataset]
    if (grepl("factorial|interaction|polynomial", dataset)) {
        documentation <- "https://marginaleffects.com/data/model_to_meaning_simulated_data.html"
    } else if (grepl("^ces", dataset)) {
        documentation <- "https://marginaleffects.com/data/ces.html"
    } else {
        documentation <- sub("parquet$", "html", data_url)
    }

    temp_doc <- tempfile(fileext = ".html")
    utils::download.file(documentation, temp_doc, mode = "w", quiet = TRUE)

    if (requireNamespace("rstudioapi")) {
        if (isTRUE(rstudioapi::isAvailable())) {
            rstudioapi::viewer(temp_doc)
            return()
        }
    }

    msg <- "Please choose a default browser with a command like: `options(browser = 'firefox')`"
    if (identical(getOption("browser"), "")) stop(msg, call. = FALSE)

    viewer <- getOption("viewer", utils::browseURL)
    if (!is.function(viewer)) stop(msg, call. = FALSE)

    viewer(temp_doc)
}
