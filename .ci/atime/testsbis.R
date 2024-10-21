library(dplyr)
library(ggplot2)
library(tidyr)

# Stored in the Github actions workflow
pr_number <- Sys.getenv("PR_NUMBER")

run_benchmark <- function(pr_number) {
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
        N = c(10),
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
            marginaleffects::slopes(mod),
            check = FALSE,
            iterations = 5
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
    mutate(expression = as.character(expression))

  plot_result <- ggplot(unnested, aes(N, median, color = pkg)) +
    geom_line(aes(linetype = pkg)) +
    geom_point() +
    facet_wrap(~expression) +
    theme_light() +
    theme(
      legend.position = "bottom"
    )

  ggsave(
    plot = plot_result,
    ".ci/atime/benchmark_result.png"
  )
}

run_benchmark(pr_number = pr_number)
