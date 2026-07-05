#!/usr/bin/env Rscript

# Compare the CRAN release of marginaleffects to the local checkout.
#
# The two package versions are installed in separate temporary libraries and
# benchmarked in separate R sessions, so namespaces never collide.

required <- c("bench", "callr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    "Install required packages before running this benchmark: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

script_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
script_dir <- if (is.null(script_file)) getwd() else dirname(script_file)
repo_root <- normalizePath(file.path(script_dir, "..", ".."), mustWork = FALSE)
pkg_dir <- file.path(repo_root, "r")
if (!file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
  repo_root <- normalizePath(getwd(), mustWork = TRUE)
  pkg_dir <- file.path(repo_root, "r")
}
if (!file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
  stop("Could not find r/DESCRIPTION. Run this script from the repository root.", call. = FALSE)
}

iterations <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_ITERATIONS", "10"))
n <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_N", "50000"))
profile_memory <- tolower(Sys.getenv("MARGINALEFFECTS_BENCH_MEMORY", "false")) %in%
  c("1", "true", "yes")

install_local <- function(lib, pkg_dir) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-multiarch", "-l", shQuote(lib), shQuote(pkg_dir))
  )
  if (!identical(status, 0L)) {
    stop("Local package installation failed.", call. = FALSE)
  }
}

install_cran <- function(lib) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  utils::install.packages(
    "marginaleffects",
    lib = lib,
    repos = "https://cloud.r-project.org",
    dependencies = NA,
    quiet = TRUE
  )
  if (!file.exists(file.path(lib, "marginaleffects", "DESCRIPTION"))) {
    stop("CRAN package installation failed.", call. = FALSE)
  }
}

run_benchmark <- function(lib, label, iterations, n, profile_memory) {
  callr::r(
    function(lib, label, iterations, n, profile_memory) {
      .libPaths(c(lib, .libPaths()))
      suppressPackageStartupMessages({
        library(bench)
        library(marginaleffects)
      })

      package_path <- normalizePath(find.package("marginaleffects"), mustWork = TRUE)
      expected_path <- normalizePath(file.path(lib, "marginaleffects"), mustWork = TRUE)
      if (!identical(package_path, expected_path)) {
        stop(sprintf(
          "%s benchmark loaded marginaleffects from %s instead of %s",
          label, package_path, expected_path
        ))
      }

      set.seed(1024)

      dat_lm <- data.frame(matrix(rnorm(n * 12), ncol = 12))
      names(dat_lm) <- paste0("X", seq_along(dat_lm))
      dat_lm$groupid <- sample(letters[1:10], n, replace = TRUE)
      mod_lm <- lm(X1 ~ ., data = dat_lm)

      dat_glm <- data.frame(
        y = sample(0:1, n, replace = TRUE),
        treatment = sample(0:1, n, replace = TRUE),
        x1 = rnorm(n),
        x2 = rnorm(n),
        x3 = sample(letters[1:5], n, replace = TRUE),
        groupid = sample(letters[1:10], n, replace = TRUE)
      )
      mod_glm <- glm(
        y ~ treatment * (x1 + x2 + x3),
        data = dat_glm,
        family = binomial
      )

      out <- bench::mark(
        `comparisons(mod_glm, variables = "treatment")` =
          comparisons(mod_glm, variables = "treatment"),
        `slopes(mod_lm, variables = "X3")` =
          slopes(mod_lm, variables = "X3"),
        `avg_comparisons(mod_glm, variables = "treatment", by = "groupid")` =
          avg_comparisons(mod_glm, variables = "treatment", by = "groupid"),
        check = FALSE,
        iterations = iterations,
        memory = profile_memory,
        filter_gc = FALSE
      )

      memory_mb <- if ("mem_alloc" %in% names(out)) {
        as.numeric(out$mem_alloc) / 1024^2
      } else {
        rep(NA_real_, nrow(out))
      }

      data.frame(
        package = label,
        version = as.character(utils::packageVersion("marginaleffects")),
        expression = as.character(out$expression),
        median_seconds = as.numeric(out$median),
        memory_mb = memory_mb,
        stringsAsFactors = FALSE
      )
    },
    args = list(
      lib = lib,
      label = label,
      iterations = iterations,
      n = n,
      profile_memory = profile_memory
    ),
    show = TRUE
  )
}

format_seconds <- function(x) sprintf("%.3f", x)
format_memory <- function(x) ifelse(is.na(x), "n/a", paste0(sprintf("%.1f", x), " MB"))
format_speedup <- function(x) sprintf("%.1fx", x)

tmp <- tempfile("marginaleffects-cran-main-benchmark-")
cran_lib <- file.path(tmp, "cran")
main_lib <- file.path(tmp, "main")

cat("Installing CRAN marginaleffects...\n")
install_cran(cran_lib)

cat("Installing local marginaleffects from ", pkg_dir, "...\n", sep = "")
install_local(main_lib, pkg_dir)

cat("Running benchmarks with n = ", n, " and iterations = ", iterations, "...\n", sep = "")
cran <- run_benchmark(cran_lib, "CRAN", iterations, n, profile_memory)
main <- run_benchmark(main_lib, "main", iterations, n, profile_memory)

wide <- merge(
  cran,
  main,
  by = "expression",
  suffixes = c("_cran", "_main"),
  sort = FALSE
)
wide$speedup <- wide$median_seconds_cran / wide$median_seconds_main
table <- data.frame(
  Command = wide$expression,
  `CRAN version` = wide$version_cran,
  `main version` = wide$version_main,
  `CRAN median (s)` = format_seconds(wide$median_seconds_cran),
  `main median (s)` = format_seconds(wide$median_seconds_main),
  `Speedup` = format_speedup(wide$speedup),
  check.names = FALSE
)
if (profile_memory) {
  table[["CRAN memory"]] <- format_memory(wide$memory_mb_cran)
  table[["main memory"]] <- format_memory(wide$memory_mb_main)
}

cat("\n")
print(table, row.names = FALSE)

cat("\nMarkdown table:\n\n")
if (profile_memory) {
  cat("| Command | CRAN | main | Speedup | CRAN memory | main memory |\n")
  cat("|---|---:|---:|---:|---:|---:|\n")
  for (i in seq_len(nrow(wide))) {
    cat(sprintf(
      "| `%s` | %ss | %ss | %s | %s | %s |\n",
      wide$expression[i],
      format_seconds(wide$median_seconds_cran[i]),
      format_seconds(wide$median_seconds_main[i]),
      format_speedup(wide$speedup[i]),
      format_memory(wide$memory_mb_cran[i]),
      format_memory(wide$memory_mb_main[i])
    ))
  }
} else {
  cat("| Command | CRAN | main | Speedup |\n")
  cat("|---|---:|---:|---:|\n")
  for (i in seq_len(nrow(wide))) {
    cat(sprintf(
      "| `%s` | %ss | %ss | %s |\n",
      wide$expression[i],
      format_seconds(wide$median_seconds_cran[i]),
      format_seconds(wide$median_seconds_main[i]),
      format_speedup(wide$speedup[i])
    ))
  }
}
