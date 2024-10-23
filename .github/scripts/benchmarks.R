library(dplyr)
library(tidyr)
library(tinytable)

# Stored in the Github actions workflow
pr_number <- Sys.getenv("PR_NUMBER")
# pr_number <- 1244

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
      N = 50,
      # N = c(100, 500, 1000, 10000, 50000),
      {
        dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
        mod <- lm(X1 ~ ., dat)
        bench::mark(
          # marginal effects at the mean; no standard error
          slopes(mod, vcov = FALSE, newdata = "mean"),
          # marginal effects at the mean
          slopes(mod, newdata = "mean"),
          # 1 variable; no standard error
          slopes(mod, vcov = FALSE, variables = "X3"),
          # 1 variable
          slopes(mod, variables = "X3"),
          # 26 variables; no standard error
          slopes(mod, vcov = FALSE),
          # 26 variables
          slopes(mod),
          check = FALSE,
          iterations = 10
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
    median_diff_main_pr = round((median_PR - median_main) / median_main * 100, 2),
    median_PR = paste0(median_PR, " (", median_diff_main_pr, "%)"),
    mem_alloc_diff_main_pr = round((mem_alloc_PR - mem_alloc_main) / mem_alloc_main * 100, 2),
    mem_alloc_PR = paste0(mem_alloc_PR, " (", mem_alloc_diff_main_pr, "%)")
  ) |>
  select(
    Expression = expression,
    `Median time with PR (% change with main)` = median_PR,
    `Memory used with PR (% change with main)` = mem_alloc_PR
  )

tt(final) |>
  save_tt("report.md")
