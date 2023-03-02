rm(list=ls())
rm(list=ls(.GlobalEnv), envir = .GlobalEnv)

EXPENSIVE <- FALSE

options("tinysnapshot_device" = "svglite")
options("tinysnapshot_tol" = 200)

# libraries
requiet <- function(package) {
    void <- capture.output(
    pkg_available <- tryCatch(suppressPackageStartupMessages(suppressWarnings(suppressMessages(tryCatch(
        isTRUE(require(package, warn.conflicts = FALSE, character.only = TRUE)),
        error = function(e) FALSE
    ))))))
    return(pkg_available)
}

requiet("tinytest")
requiet("tinysnapshot")

if (isTRUE(suppressMessages(require("tinytest"))) && packageVersion("tinytest") >= "1.4.0") {
    tinytest::register_tinytest_extension(
        "marginaleffects",
        c("expect_slopes", "expect_predictions", "expect_margins", "expect_marginal_means"))
}

# common names of datasets, often assigned to global environment
common <- c("dat", "tmp", "d", "k", "mod", "tmp1", "tmp2", "test1", "test2", "threenum")
suppressWarnings(rm(list = common, envir = .GlobalEnv))
suppressWarnings(rm(list = common))

# avoids a `timedatectl`` warning
Sys.setenv(TZ="America/New_York") 

# snapshots
options(width = 10000)
options(digits = 5)

ON_CRAN <- !identical(Sys.getenv("R_NOT_CRAN"), "true")
ON_GH <- identical(Sys.getenv("R_GH"), "true")
ON_CI <- isTRUE(ON_CRAN) || isTRUE(ON_GH)
ON_WINDOWS <- isTRUE(Sys.info()[['sysname']] == "Windows")
ON_OSX <- isTRUE(Sys.info()[['sysname']] == "Darwin")

minver <- function(pkg, ver = NULL) {
    ins <- try(utils::packageVersion(pkg), silent = TRUE)
    if (is.null(ver)) {
        isTRUE(inherits(ins, "try-error"))
    } else {
        isTRUE(as.character(ins) >= ver)
    }
}

testing_path <- function(x) {
    wd <- tinytest::get_call_wd()
    if (isTRUE(wd != "")) {
        out <- x
    } else {
        out <- paste0(wd, "/", x)
    }
    out <- gsub("^\\/", "", out)
    return(out)
}
