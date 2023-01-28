# mod <- glm(vs ~ hp * wt, data = mtcars, family = binomial)

# mod |> inference() |> avg_predictions()

# mod |> inference() |> slopes() |> head()

# mod |> inference() |> predictions(vcov = ~gear) |> head()

# mod |> inference(iter = 17) |> comparisons() |> attr("posterior_draws") |> dim()