EXPENSIVE <- FALSE

# libraries
requiet <- function(package) {
    void <- capture.output(
    pkg_available <<- tryCatch(suppressPackageStartupMessages(suppressWarnings(suppressMessages(tryCatch(
        isTRUE(require(package, warn.conflicts = FALSE, character.only = TRUE)),
        error = function(e) FALSE
    ))))))
    return(pkg_available)
}

requiet("tinytest")
requiet("tinyviztest")

if (isTRUE(suppressMessages(require("tinytest"))) && packageVersion("tinytest") >= "1.4.0") {
    tinytest::register_tinytest_extension(
        "marginaleffects",
        c("expect_slopes", "expect_predictions", "expect_margins", "expect_marginalmeans"))
}

# common names of datasets, often assigned to global environment
common <- c("dat", "tmp", "d", "k", "mod", "tmp1", "tmp2", "test1", "test2")
suppressWarnings(rm(list = common, envir = .GlobalEnv))
suppressWarnings(rm(list = common))

# avoids a `timedatectl`` warning
Sys.setenv(TZ="America/New_York") 

# snapshots
options(width = 10000)

# important because otherwise testing so many packages is terrible
conflicted::conflict_prefer(name = "expect_error", winner = "tinytest", quiet = TRUE)
conflicted::conflict_prefer(name = "expect_true", winner = "tinytest", quiet = TRUE)
conflicted::conflict_prefer(name = "expect_equal", winner = "tinytest", quiet = TRUE)
conflicted::conflict_prefer(name = "expect_warning", winner = "tinytest", quiet = TRUE)
conflicted::conflict_prefer(name = "lmer", winner = "lme4", quiet = TRUE)
conflicted::conflict_prefer(name = "s", winner = "gam", quiet = TRUE)
conflicted::conflict_prefer(name = "ar", winner = "stats", quiet = TRUE)
conflicted::conflict_prefer(name = "marginal_effects", winner = "margins", quiet = TRUE)
conflicted::conflict_prefer(name = "kidney", winner = "brms", quiet = TRUE)
conflicted::conflict_prefer(name = "ngrps", winner = "brms", quiet = TRUE)
conflicted::conflict_prefer(name = "lizards", winner = "aod", quiet = TRUE)
conflicted::conflict_prefer(name = "rats", winner = "aod", quiet = TRUE)
conflicted::conflict_prefer(name = "mad", winner = "stats", quiet = TRUE)
conflicted::conflict_prefer(name = "sd", winner = "stats", quiet = TRUE)
conflicted::conflict_prefer(name = "var", winner = "stats", quiet = TRUE)

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
