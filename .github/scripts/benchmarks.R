library(dplyr)
library(tidyr)
library(tinytable)

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
    library(data.table)

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
      }
    )
  }
)

unnested <- out |>
  mutate(
    pkg = case_match(
      pkg,
      "marginaleffects" ~ "CRAN",
      "vincentarelbundock/marginaleffects" ~ "main",
      paste0("vincentarelbundock/marginaleffects#", pr_number) ~ "PR"
    )
  ) |>
  unnest(result) |>
  mutate(
    expression = as.character(expression),
    # Get duration in seconds
    median = round(as.numeric(median), 3),
    # Get memory in MB
    mem_alloc = round(as.numeric(mem_alloc) / 1000000, 3)
  ) |>
  select(pkg, expression, median, mem_alloc)

final <- unnested |>
  pivot_wider(
    id_cols = expression,
    names_from = pkg,
    values_from = c(median, mem_alloc)
  ) |>
  mutate(
    # Compute change in time and memory between PR/main branch and PR/CRAN
    median_diff_main_pr = round((median_PR - median_main) / median_main * 100, 2),
    median_diff_CRAN_pr = round((median_PR - median_CRAN) / median_CRAN * 100, 2),

    # Compute change in time and memory between PR and CRAN
    mem_alloc_diff_main_pr = round((mem_alloc_PR - mem_alloc_main) / mem_alloc_main * 100, 2),
    mem_alloc_diff_CRAN_pr = round((mem_alloc_PR - mem_alloc_CRAN) / mem_alloc_CRAN * 100, 2),
    across(
      .cols = c(
        median_diff_main_pr, median_diff_CRAN_pr,
        mem_alloc_diff_main_pr, mem_alloc_diff_CRAN_pr
      ),
      function(x) {
        case_when(
          x >= 5 ~ paste0(":collision: ", x, "%"),
          x < 5 & x > -5 ~ paste0(x, "%"),
          x <= -5 ~ paste0(":zap: ", x, "%"),
          .default = NA
        )
      }
    ),
  ) |>
  select(
    Expression = expression,
    `PR time (median, seconds)` = median_PR,
    "% change with `main`" = median_diff_main_pr,
    "% change with CRAN" = median_diff_CRAN_pr,
    `PR memory (MB)` = mem_alloc_PR,
    "Mem. % change with `main`" = mem_alloc_diff_main_pr,
    "Mem. % change with CRAN" = mem_alloc_diff_CRAN_pr,
  )

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
