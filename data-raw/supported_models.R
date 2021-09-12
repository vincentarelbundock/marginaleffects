supported_models <- read.csv("data-raw/supported_models.csv", check.names = FALSE, encoding = "utf8")
usethis::use_data(supported_models, overwrite = TRUE)
