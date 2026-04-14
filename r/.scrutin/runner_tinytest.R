# scrutin tinytest runner
#
# Sources the shared R runner infrastructure, defines the tinytest-specific
# run function, and starts the main loop.
# Edit this file to customize package loading or test execution.

# Locate this script's directory (works under both Rscript and source()).
.scrutin_script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m) > 0) return(dirname(sub("^--file=", "", m[1])))
  for (i in rev(seq_len(sys.nframe()))) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) return(dirname(f))
  }
  "."
})
source(file.path(.scrutin_script_dir, "runner_r.R"), local = FALSE)

# Load the package under test and set up runtime dependency tracing.
.scrutin_env$load_package()
.scrutin_env$setup_tracing()

.scrutin_env$run_test <- function(path) {
  `%||%` <- .scrutin_env$`%||%`
  file <- basename(path)
  t0 <- proc.time()["elapsed"]
  counts <- list(pass = 0L, fail = 0L, error = 0L,
                 skip = 0L, xfail = 0L, warn = 0L)
  tryCatch({
    results <- withCallingHandlers(
      tinytest::run_test_file(path, verbose = 0),
      warning = function(w) {
        counts$warn <<- counts$warn + 1L
        .scrutin_env$emit(.scrutin_env$event(
          file = file,
          outcome = "warn",
          subject_kind = "function",
          subject_name = "<warning>",
          message = conditionMessage(w)
        ))
        tryInvokeRestart("muffleWarning")
      }
    )
    elapsed <- as.integer((proc.time()["elapsed"] - t0) * 1000)

    # tinytest's exit_file() bails out and returns an empty `tinytests`
    # object. Surface as a generic file-level skip.
    if (length(results) == 0) {
      counts$skip <- counts$skip + 1L
      .scrutin_env$emit(.scrutin_env$event(
        file = file,
        outcome = "skip",
        subject_kind = "function",
        subject_name = "(file)",
        message = "tinytest exit_file() -- see file for reason"
      ))
    }

    for (i in seq_along(results)) {
      r <- results[[i]]
      # tinytest stores `info` as NA (not NULL) when unset, so %||% doesn't
      # catch it. Handle both forms explicitly. Same care for `call`,
      # which can be a language object that deparses fine but still needs
      # to round-trip as a string for the wire protocol.
      info_attr <- attr(r, "info")
      info <- if (is.null(info_attr) || (length(info_attr) == 1 && is.na(info_attr))) {
        ""
      } else {
        as.character(info_attr)
      }
      call_str <- if (!is.null(attr(r, "call"))) deparse1(attr(r, "call")) else ""
      test_name <- if (nzchar(info)) info else if (nzchar(call_str)) call_str else paste0("test ", i)
      fst <- attr(r, "fst")
      line <- if (is.null(fst)) NULL else as.integer(fst)[1]

      if (isTRUE(attr(r, "skip"))) {
        counts$skip <- counts$skip + 1L
        .scrutin_env$emit(.scrutin_env$event(
          file = file,
          outcome = "skip",
          subject_kind = "function",
          subject_name = test_name,
          message = as.character(attr(r, "message") %||% ""),
          line = line
        ))
      } else if (isTRUE(r)) {
        counts$pass <- counts$pass + 1L
        .scrutin_env$emit(.scrutin_env$event(
          file = file,
          outcome = "pass",
          subject_kind = "function",
          subject_name = test_name,
          line = line
        ))
      } else {
        counts$fail <- counts$fail + 1L
        msg <- as.character(attr(r, "diff") %||% as.character(r))
        .scrutin_env$emit(.scrutin_env$event(
          file = file,
          outcome = "fail",
          subject_kind = "function",
          subject_name = test_name,
          message = msg,
          line = line
        ))
      }
    }

    .scrutin_env$emit_summary(file, counts, elapsed)
  }, error = function(e) {
    elapsed <- as.integer((proc.time()["elapsed"] - t0) * 1000)
    counts$error <- counts$error + 1L
    .scrutin_env$emit(.scrutin_env$event(
      file = file,
      outcome = "error",
      subject_kind = "function",
      subject_name = "<file>",
      message = conditionMessage(e),
      line = .scrutin_env$error_line(e),
      duration_ms = elapsed
    ))
    .scrutin_env$emit_summary(file, counts, elapsed)
  })
}

.scrutin_env$main()
