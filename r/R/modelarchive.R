# unexported functions for use in tests

modelarchive_model <- function(name) {
    tmp <- tempfile()
    url <- paste0(
        "https://raw.github.com/vincentarelbundock/modelarchive/main/data/",
        name,
        ".rds"
    )
    try(utils::download.file(url, tmp, quiet = TRUE), silent = TRUE)
    out <- try(readRDS(tmp), silent = TRUE)
    return(out)
}

modelarchive_data <- function(name) {
    dat <- sprintf(
        "https://raw.githubusercontent.com/vincentarelbundock/modelarchive/main/data-raw/%s.csv",
        name
    )
    out <- utils::read.csv(dat)
    return(out)
}
