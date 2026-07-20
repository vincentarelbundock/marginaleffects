# Profile the fastest analytic case from benchmark_jacobian_analytic_packages.R.
# Run from the repository root after installing the development package.

source("r/sandbox/benchmark_jacobian_analytic_packages.R")

profile_name <- Sys.getenv("MARGINALEFFECTS_PROFILE_MODEL", "")
if (!nzchar(profile_name)) {
    profile_name <- rownames(benchmark_results)[which.min(benchmark_results[, "analytic"])]
}
if (!profile_name %in% names(benchmark_cases)) {
    stop("Unknown profile model: ", profile_name)
}

iterations <- as.integer(Sys.getenv("MARGINALEFFECTS_PROFILE_ITER", "50"))
profile_output <- Sys.getenv(
    "MARGINALEFFECTS_PROFILE_OUTPUT",
    file.path(tempdir(), paste0("jacobian-package-", profile_name, ".out"))
)
profile_fun <- benchmark_cases[[profile_name]]

old_option <- options(marginaleffects_analytic_jacobian = TRUE)
on.exit(options(old_option), add = TRUE)
invisible(profile_fun())
utils::Rprof(profile_output, interval = 0.001, memory.profiling = TRUE)
for (i in seq_len(iterations)) {
    invisible(profile_fun())
}
utils::Rprof(NULL)

profile_summary <- summaryRprof(profile_output, memory = "both")
cat("Profiled model:", profile_name, "\n")
cat("Iterations:", iterations, "\n")
cat("Profile output:", profile_output, "\n\n")
cat("Top functions by total time:\n")
print(utils::head(profile_summary$by.total, 20))
cat("\nTop functions by self time:\n")
print(utils::head(profile_summary$by.self, 20))
