supported_models <- read.csv("data-raw/supported_models.csv", check.names = FALSE)
usethis::use_data(supported_models, overwrite = TRUE)
