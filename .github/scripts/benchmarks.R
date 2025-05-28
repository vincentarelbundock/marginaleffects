library(cross)
library(tinytable)
library(collapse)

# Stored in the Github actions workflow
pr_number <- Sys.getenv("PR_NUMBER")

out <- cross::run(
  pkgs = c(
    "marginaleffects",
    "vincentarelbundock/marginaleffects",
    paste0("vincentarelbundock/marginaleffects#", pr_number)
  ),
  ~ {
    library(marginaleffects)

    bench::press(
      N = 75000,
      {
        dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
        mod <- lm(X1 ~ ., dat)

        set.seed(1234)
        dat2 <- data.frame(
          outcome = sample(0:1, N, TRUE),
          incentive = runif(N),
          agecat = sample(c("18-35", "35-60", "60+"), N, TRUE),
          distance = sample(0:10000, N, TRUE)
        )

        mod2 <- glm(outcome ~ incentive * (agecat + distance),
          data = dat2, family = binomial
        )
        grid <- data.frame(distance = 2, agecat = "18-35", incentive = 1)

        bench::mark(
          check = FALSE,
          iterations = 20,

          # Slopes =========================================
          slopes(mod, vcov = FALSE, newdata = "mean"),
          slopes(mod, newdata = "mean"),
          slopes(mod, vcov = FALSE, variables = "X3"),
          slopes(mod, variables = "X3"),
          slopes(mod, vcov = FALSE),
          slopes(mod),

          # Hypothesis =========================================
          hypotheses(mod, hypothesis = "b3 - b1 = 0"),
          hypotheses(mod, hypothesis = "b2^2 * exp(b1) = 0"),
          hypotheses(mod, hypothesis = ~reference),

          # Predictions =========================================
          predictions(mod),
          predictions(mod, newdata = "mean"),
          predictions(mod, newdata = datagrid(X2 = unique)),

          # Comparisons =========================================
          comparisons(mod2),
          comparisons(mod2, comparison = "dydxavg"),
          comparisons(mod2, comparison = "eydxavg"),
          comparisons(mod2, comparison = "ratioavg")
        )
    })
  })


unnested <- do.call(rbind, lapply(seq_len(nrow(out)), \(i) {
  x <- data.frame(
    pkg = out$pkg[i],
    expression = as.character(out$result[[i]]$expression),
    median = as.numeric(out$result[[i]]$median),
    mem_alloc = as.numeric(out$result[[i]]$mem_alloc) / 1e6
  )
  dict <- c(
    "CRAN" = "marginaleffects",
    "main" = "vincentarelbundock/marginaleffects",
    "PR" = paste0("vincentarelbundock/marginaleffects#", pr_number)
  )
  x$pkg <- names(dict)[match(x$pkg, dict)]
  return(x)
}))


final <- unnested |>
  reshape(
    direction = "wide",
    idvar = "expression",
    timevar = "pkg"
  ) |>
  transform(
    # Compute change in time and memory between PR/main branch and PR/CRAN
    median_diff.main.pr = round((median.PR - median.main) / median.main * 100, 2),
    median_diff.CRAN.pr = round((median.PR - median.CRAN) / median.CRAN * 100, 2),

    # Compute change in time and memory between PR and CRAN
    mem_alloc_diff.main.pr = round((mem_alloc.PR - mem_alloc.main) / mem_alloc.main * 100, 2),
    mem_alloc_diff.CRAN.pr = round((mem_alloc.PR - mem_alloc.CRAN) / mem_alloc.CRAN * 100, 2)
  )

cols <- c("median_diff.main.pr", "median_diff.CRAN.pr", "mem_alloc_diff.main.pr", "mem_alloc_diff.CRAN.pr")
for (col in cols) {
  old <- final[[col]]
  new <- rep(NA_character_, nrow(final))
  new <- ifelse(old >= 5, paste0(":collision: ", old, "%"), new)
  new <- ifelse(old < 5 & old > -5, paste0(old, "%"), new)
  new <- ifelse(old <= -5, paste0(":zap: ", old, "%"), new)
  final[[col]] <- new
}

cols <- c(
  "Expression" = "expression",
  "PR time (median, seconds)" = "median.PR",
  '% change with "main"' = "median_diff.main.pr",
  "% change with CRAN" = "median_diff.CRAN.pr",
  "PR memory (MB)" = "mem_alloc.PR",
  'Mem. % change with "main"' = "mem_alloc_diff.main.pr",
  "Mem. % change with CRAN" = "mem_alloc_diff.CRAN.pr"
)

final <- setNames(final[, cols], names(cols))

raw_table <- tt(final) |>
  save_tt("gfm")

paste0(
  "**Benchmark results**\n\n",
  ":collision: means that PR is more than 5% slower than main or CRAN\n",
  ":zap: means that PR is more than 5% faster than main or CRAN\n",
  "<details>\n<summary>Click to see benchmark results</summary>\n\n",
  raw_table,
  "\n\n</details>"
) |>
  writeLines("report.md")
