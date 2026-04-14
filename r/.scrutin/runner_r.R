# scrutin shared R runner infrastructure
#
# Sourced by each per-plugin runner (testthat, tinytest, pointblank, jarl).
# Defines the .scrutin_env environment with NDJSON encoding, emit helpers,
# srcref lookup, emit_summary, worker startup hook, and the main stdin loop.
#
# After sourcing this file, a plugin runner must:
#   1. Optionally call pkgload::load_all() or other package loading
#   2. Define .scrutin_env$run_test (a function taking a file path)
#   3. Call .scrutin_env$main() to start the stdin loop
#
# Wire protocol: see docs/reporting-spec.md and the Rust mirror at
# crates/scrutin-core/src/engine/protocol.rs. Three message types:
#   - {"type":"event", "outcome": one of pass|fail|error|skip|xfail|warn, ...}
#   - {"type":"summary", "duration_ms":..., "counts": {...}}
#   - {"type":"done"}

# Make warnings fire as conditions immediately so withCallingHandlers can
# capture them. R's default (warn=0) defers warnings until top-level, which
# bypasses calling handlers entirely.
options(warn = 1)

.scrutin_env <- new.env(parent = baseenv())

.scrutin_env$`%||%` <- function(x, y) if (is.null(x)) y else x

# --- Minimal hand-rolled NDJSON encoder ---
# Removes the jsonlite runtime dependency. Handles only the shapes scrutin's
# protocol actually emits: NULL, scalar logical/integer/numeric/character,
# atomic vectors (length != 1), and named lists (treated as JSON objects).
# Anything else throws so future schema additions surface immediately.

.scrutin_env$json_str <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n",  s, fixed = TRUE)
  s <- gsub("\r", "\\r",  s, fixed = TRUE)
  s <- gsub("\t", "\\t",  s, fixed = TRUE)
  # Other C0 control chars (including NUL) and the U+FFFE/FFFF non-characters
  # are not legal in JSON strings; replace with '?'.
  s <- gsub("[\\x00-\\x08\\x0b\\x0c\\x0e-\\x1f]", "?", s, perl = TRUE)
  paste0("\"", s, "\"")
}

.scrutin_env$json_scalar <- function(v) {
  if (is.na(v))      return("null")
  if (is.logical(v)) return(if (v) "true" else "false")
  if (is.numeric(v)) {
    if (!is.finite(v)) return("null")
    return(format(v, scientific = FALSE, trim = TRUE))
  }
  if (is.character(v)) return(.scrutin_env$json_str(v))
  stop("scrutin to_json: unsupported scalar type ", typeof(v))
}

.scrutin_env$to_json <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (length(x) == 0) return("{}")
    nms <- names(x)
    if (is.null(nms) || any(!nzchar(nms)))
      stop("scrutin to_json: lists must be fully named")
    parts <- vapply(seq_along(x), function(i) {
      paste0(.scrutin_env$json_str(nms[i]), ":", .scrutin_env$to_json(x[[i]]))
    }, character(1))
    return(paste0("{", paste(parts, collapse = ","), "}"))
  }
  if (is.atomic(x)) {
    if (length(x) == 0) return("null")
    if (length(x) == 1) return(.scrutin_env$json_scalar(x))
    parts <- vapply(seq_along(x), function(i) .scrutin_env$json_scalar(x[i]), character(1))
    return(paste0("[", paste(parts, collapse = ","), "]"))
  }
  stop("scrutin to_json: unsupported type ", typeof(x))
}

.scrutin_env$emit <- function(obj) {
  cat(.scrutin_env$to_json(obj), "\n", sep = "")
  flush(stdout())
}

# Build a per-test event. `outcome` is one of pass/fail/error/skip/xfail/warn.
# Optional fields are dropped when NULL so the wire stays compact.
#
# `metrics`, when supplied, is a named list with optional `total`, `failed`,
# `fraction` numeric scalars (used by data-validation plugins). Unit-test
# plugins pass NULL.
.scrutin_env$event <- function(file, outcome, subject_kind, subject_name,
                               subject_parent = NULL, message = NULL,
                               line = NULL, duration_ms = 0L,
                               metrics = NULL) {
  obj <- list(
    type = "event",
    file = file,
    outcome = outcome,
    subject = if (is.null(subject_parent)) {
      list(kind = subject_kind, name = subject_name)
    } else {
      list(kind = subject_kind, name = subject_name, parent = subject_parent)
    }
  )
  if (!is.null(metrics)) obj$metrics <- metrics
  if (!is.null(message)) obj$message <- message
  if (!is.null(line)) obj$line <- line
  obj$duration_ms <- as.integer(duration_ms)
  obj
}

.scrutin_env$pkg_dir <- Sys.getenv("SCRUTIN_PKG_DIR", ".")

# Worker startup hook: sourced once before the stdin loop. Failure emits a
# sentinel event tagged file = "<worker_startup>" and exits 2 so the pool
# can mark the worker permanently poisoned.
.scrutin_env$worker_startup_path <- Sys.getenv("SCRUTIN_WORKER_STARTUP", "")
if (nzchar(.scrutin_env$worker_startup_path)) {
  tryCatch(
    sys.source(.scrutin_env$worker_startup_path, envir = globalenv()),
    error = function(e) {
      .scrutin_env$emit(.scrutin_env$event(
        file = "<worker_startup>",
        outcome = "error",
        subject_kind = "engine",
        subject_name = "<worker_startup>",
        message = conditionMessage(e)
      ))
      quit(save = "no", status = 2)
    }
  )
}

# Source-file line lookup for an expectation result.
#
# `result$srcref` is the obvious answer, but it has two failure modes:
#   1. When called from a helper sourced from another file, it points at
#      the *helper* file, not the test file.
#   2. testthat 3.x doesn't always attach `srcref` for the success path.
#
# Strategy: trust `result$srcref` only if its srcfile matches the test file;
# otherwise walk `sys.calls()` from deepest to shallowest looking for a
# call frame whose srcref *is* in the test file.
.scrutin_env$srcref_line_in_file <- function(sr, test_file) {
  if (is.null(sr)) return(NULL)
  sf <- attr(sr, "srcfile")
  fname <- if (!is.null(sf)) sf$filename else NULL
  if (is.null(fname) || basename(fname) == test_file) {
    n <- as.integer(sr)
    if (length(n) >= 1) return(n[1])
  }
  NULL
}

.scrutin_env$error_line <- function(e) {
  tryCatch({
    cl <- conditionCall(e)
    if (is.null(cl)) return(NULL)
    sr <- attr(cl, "srcref")
    if (is.null(sr)) return(NULL)
    n <- as.integer(sr)
    if (length(n) >= 1) n[1] else NULL
  }, error = function(e2) NULL)
}

.scrutin_env$expectation_line <- function(result, test_file) {
  line <- .scrutin_env$srcref_line_in_file(result$srcref, test_file)
  if (!is.null(line)) return(line)
  calls <- sys.calls()
  for (i in rev(seq_along(calls))) {
    line <- .scrutin_env$srcref_line_in_file(attr(calls[[i]], "srcref"), test_file)
    if (!is.null(line)) return(line)
  }
  NULL
}

.scrutin_env$emit_summary <- function(file, counts, duration_ms) {
  `%||%` <- .scrutin_env$`%||%`
  .scrutin_env$emit(list(
    type = "summary",
    file = file,
    duration_ms = as.integer(duration_ms),
    counts = list(
      pass  = as.integer(counts$pass  %||% 0L),
      fail  = as.integer(counts$fail  %||% 0L),
      error = as.integer(counts$error %||% 0L),
      skip  = as.integer(counts$skip  %||% 0L),
      xfail = as.integer(counts$xfail %||% 0L),
      warn  = as.integer(counts$warn  %||% 0L)
    )
  ))
}

# Helper: load the package under test via pkgload::load_all().
# Called by plugins that need it (testthat, tinytest, pointblank).
# Failure is non-fatal: surfaces as an error event, then continues.
# On success, stashes the package name in .scrutin_env$pkg_name for
# setup_tracing() to use.
#
# Pre-flight: if pkgload itself is missing, poison the worker with a
# clear, actionable message. Otherwise the user sees a generic "there
# is no package called 'pkgload'" repeated once per test file.
.scrutin_env$load_package <- function() {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    .scrutin_env$emit(.scrutin_env$event(
      file = "<worker_startup>",
      outcome = "error",
      subject_kind = "engine",
      subject_name = "<worker_startup>",
      message = paste(
        "scrutin's default R runner requires 'pkgload', which is not installed.",
        "Fix: install.packages('pkgload')",
        "Or edit .scrutin/<tool>/runner.R to use library() instead of pkgload::load_all().",
        sep = "\n"
      )
    ))
    quit(save = "no", status = 2)
  }
  tryCatch({
    pkgload::load_all(.scrutin_env$pkg_dir, quiet = TRUE)
    desc <- file.path(.scrutin_env$pkg_dir, "DESCRIPTION")
    if (file.exists(desc)) {
      lines <- readLines(desc, warn = FALSE)
      m <- grep("^Package:", lines, value = TRUE)
      if (length(m) > 0) {
        .scrutin_env$pkg_name <- trimws(sub("^Package:", "", m[1]))
      }
    }
  }, error = function(e) {
    .scrutin_env$emit(.scrutin_env$event(
      file = "",
      outcome = "error",
      subject_kind = "engine",
      subject_name = "<load_all>",
      message = paste("load_all failed:", conditionMessage(e))
    ))
  })
}

# --- Runtime dependency tracing ---
#
# Instruments package functions via trace() so that each test file's actual
# source-file dependencies are captured at runtime. setup_tracing() is called
# once after load_package(); reset_tracing() before each test file;
# get_traced_sources() after each test file to collect hit source files.

# Environment used as a set: keys are relative source file paths that were
# hit during the current test file's execution.
.scrutin_env$.traced_sources <- new.env(parent = emptyenv())
.scrutin_env$.tracing_active <- FALSE

.scrutin_env$setup_tracing <- function() {
  pkg_name <- .scrutin_env$pkg_name
  if (is.null(pkg_name)) return()
  ns <- tryCatch(asNamespace(pkg_name), error = function(e) NULL)
  if (is.null(ns)) return()
  pkg_dir <- normalizePath(.scrutin_env$pkg_dir, winslash = "/")
  fns <- ls(ns, all.names = TRUE)
  for (fn_name in fns) {
    obj <- tryCatch(get(fn_name, envir = ns), error = function(e) NULL)
    if (!is.function(obj)) next
    sr <- tryCatch(getSrcref(obj), error = function(e) NULL)
    if (is.null(sr)) next
    sf <- attr(sr, "srcfile")
    if (is.null(sf) || is.null(sf$filename)) next
    abs_path <- normalizePath(sf$filename, winslash = "/", mustWork = FALSE)
    # Compute path relative to the package root.
    if (!startsWith(abs_path, pkg_dir)) next
    rel <- substring(abs_path, nchar(pkg_dir) + 2)
    if (!nzchar(rel)) next
    # Build a tracer expression that records this source file.
    tracer_expr <- substitute(
      assign(KEY, TRUE, envir = .scrutin_env$.traced_sources),
      list(KEY = rel)
    )
    # Trace without `where=ns`: pkgload::load_all() exports functions to
    # the search path, and calls resolve there (not in the namespace).
    # Tracing in the namespace doesn't intercept search-path calls.
    tryCatch(
      suppressMessages(trace(fn_name, tracer = tracer_expr, print = FALSE)),
      error = function(e) NULL
    )
  }
  .scrutin_env$.tracing_active <- TRUE
}

.scrutin_env$reset_tracing <- function() {
  .scrutin_env$.traced_sources <- new.env(parent = emptyenv())
}

.scrutin_env$get_traced_sources <- function() {
  ls(.scrutin_env$.traced_sources, all.names = TRUE)
}

# Emit a deps message for the given test file path. Extracted as a helper
# so both the in-process and forked code paths can share it.
.scrutin_env$emit_deps <- function(path) {
  if (!.scrutin_env$.tracing_active) return(invisible())
  sources <- .scrutin_env$get_traced_sources()
  if (length(sources) == 0) return(invisible())
  # Build deps JSON manually because to_json encodes length-1
  # character vectors as scalars, but Rust expects an array.
  src_json <- paste0(
    "[",
    paste(vapply(sources, .scrutin_env$json_str, character(1)),
          collapse = ","),
    "]"
  )
  line <- paste0(
    '{"type":"deps","file":',
    .scrutin_env$json_str(basename(path)),
    ',"sources":', src_json, '}'
  )
  .scrutin_env$emit_raw(line)
}

# Raw NDJSON writer. In-process/Windows mode writes to stdout; TCP fork
# mode overrides this to write to the socket.
.scrutin_env$emit_raw <- function(line) {
  cat(line, "\n", sep = "")
  flush(stdout())
}

# Main stdin loop. Call this after defining .scrutin_env$run_test.
#
# Two modes:
#   - TCP fork mode (SCRUTIN_TCP_PORT set): parent reads file paths from
#     stdin and forks a child per file. Each child connects to Rust via
#     TCP to deliver NDJSON results. Parent doesn't wait; children run
#     concurrently (Rust controls parallelism via the semaphore).
#   - Direct mode (no TCP port): runs tests in-process, emits NDJSON to
#     stdout. Used on Windows where fork() is unavailable.
#
# Commands:
#   /path/to/test.R  : run test
#   !shutdown        : clean teardown and exit 0
.scrutin_env$main <- function() {
  con <- file("stdin", open = "r")
  tcp_port <- Sys.getenv("SCRUTIN_TCP_PORT", "")

  shutdown <- function() {
    shutdown_path <- Sys.getenv("SCRUTIN_WORKER_TEARDOWN", "")
    if (nzchar(shutdown_path)) {
      tryCatch(
        sys.source(shutdown_path, envir = globalenv()),
        error = function(e) {
          message(sprintf("[scrutin] worker_teardown failed: %s", conditionMessage(e)))
        }
      )
    }
    close(con)
    quit(save = "no", status = 0)
  }

  if (nzchar(tcp_port)) {
    # TCP fork mode: fork per file, child connects to Rust via TCP.
    port <- as.integer(tcp_port)
    repeat {
      line <- readLines(con, n = 1)
      if (length(line) == 0) break
      line <- trimws(line)
      if (!nzchar(line)) next
      if (line == "!shutdown") shutdown()

      if (.scrutin_env$.tracing_active) .scrutin_env$reset_tracing()

      parallel::mcparallel({
        sock <- NULL
        tryCatch({
          sock <- socketConnection(
            host = "127.0.0.1", port = port,
            open = "w", blocking = TRUE, server = FALSE
          )
          # Redirect emit() and emit_raw() to write to the TCP socket.
          .scrutin_env$emit <- function(obj) {
            writeLines(.scrutin_env$to_json(obj), sock)
          }
          .scrutin_env$emit_raw <- function(line) {
            writeLines(line, sock)
          }
          .scrutin_env$run_test(line)
          .scrutin_env$emit_deps(line)
        }, error = function(e) {
          # If TCP connect or test execution fails, silently exit.
          # Rust will see the TCP close and handle the missing results.
        })
        # Explicitly close before mcparallel's _exit() (on.exit does
        # not fire in forked children).
        tryCatch(if (!is.null(sock)) close(sock), error = function(e) NULL)
        NULL
      }, mc.set.seed = FALSE)

      # Reap finished children without blocking so the parent stays
      # responsive to the next file path from Rust.
      tryCatch(parallel::mccollect(wait = FALSE), error = function(e) NULL)
    }
    # Reap all remaining children before exiting.
    tryCatch(parallel::mccollect(), error = function(e) NULL)
    close(con)
  } else {
    # Direct mode (Windows fallback): run in-process, emit to stdout.
    repeat {
      line <- readLines(con, n = 1)
      if (length(line) == 0) break
      line <- trimws(line)
      if (!nzchar(line)) next
      if (line == "!shutdown") shutdown()

      if (.scrutin_env$.tracing_active) .scrutin_env$reset_tracing()
      .scrutin_env$run_test(line)
      .scrutin_env$emit_deps(line)
      .scrutin_env$emit(list(type = "done"))
    }
    close(con)
  }
}
