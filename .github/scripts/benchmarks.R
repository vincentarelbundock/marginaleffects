library(dplyr)
library(ggplot2)
library(tidyr)

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
      N = c(100, 500, 1000, 10000, 50000),
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

plot_result <- ggplot(unnested, aes(N, median, color = pkg, linetype = pkg)) +
  geom_line() +
  geom_point() +
  facet_wrap(
    ~expression,
    labeller = labeller(expression = label_wrap_gen(width = 25)),
    scales = "free_y"
  ) +
  labs(
    y = "Median time (s)",
    color = "Package version",
    linetype = "Package version"
  ) +
  theme_light() +
  theme(
    strip.text = element_text(color = "white", size = 10),
    strip.background = element_rect(
      fill = "#24478f", linetype = "solid",
      color = "black", linewidth = 1
    ),
    legend.position = "bottom"
  )

ggsave(
  plot = plot_result,
  ".github/scripts/benchmark_result.png"
)
