library(testthat)
library(marginaleffects)
library(margins)

Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
test_check("marginaleffects")
