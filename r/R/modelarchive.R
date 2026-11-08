# unexported functions for use in tests

modelarchive_data <- function(name) {
    dat <- sprintf(
        "https://raw.githubusercontent.com/vincentarelbundock/modelarchive/main/data-raw/%s.csv",
        name
    )
    out <- utils::read.csv(dat)
    return(out)
}
