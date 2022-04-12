download_model <- function(name) {
    tmp <- tempfile()
    url <- paste0("https://raw.github.com/vincentarelbundock/modelarchive/main/data/", name, ".rds")
    download.file(url, tmp, quiet = TRUE)
    out <- readRDS(tmp)
    return(out)
}
