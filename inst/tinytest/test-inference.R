# mod <- glm(vs ~ hp * wt, data = mtcars, family = binomial)

# mod |> inferences() |> avg_predictions()

# mod |> inferences() |> slopes() |> head()

# mod |> inferences() |> predictions(vcov = ~gear) |> head()

# mod |> inferences(iter = 17) |> comparisons() |> attr("posterior_draws") |> dim()