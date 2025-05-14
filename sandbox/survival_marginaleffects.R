library(ggplot2)
library(survival)
library(splines)
library(patchwork)
theme_set(theme_bw())
options(marginaleffects_numderiv = list(method = "richardson"))
# options(marginaleffects_numderiv = NULL)
options(width = 300)
model <- coxph(
    Surv(dtime, death) ~ hormon + grade + ns(age, df = 2),
    # Surv(dtime, death) ~ hormon + grade + age,
    data = rotterdam
)

Q
pkgload::load_all()

grid = datagrid(
  dtime = seq(min(rotterdam$dtime), max(rotterdam$dtime), length.out = 25),
  hormon = 0:1,
  model = model
)

p1 = predict(model, se.fit = TRUE, type = "expected", newdata = grid) |>
  data.frame() |>
  data.frame(grid) |>
  transform(
    conf.low = fit - 1.96 * se.fit,
    conf.high = fit + 1.96 * se.fit
  )
p2 = predictions(
  model,
  type = "expected",
  numderiv = "fdcenter",
  newdata = grid
)

plot1 = ggplot(p1, aes(x = dtime, y = fit, fill = factor(hormon), ymax = conf.high, ymin = conf.low)) +
  geom_line() +
  geom_ribbon(alpha = .5) + ggtitle("survival")

plot2 = ggplot(p2, aes(x = dtime, y = estimate, fill = factor(hormon), ymax = conf.high, ymin = conf.low)) +
  geom_line() +
  geom_ribbon(alpha = .5) + ggtitle("marginaleffects")

plot1 / plot2

avg_predictions(model, type = "expected")
avg_predictions(model, type = "expected") |> inferences(method = "boot")


grid = datagrid(
  dtime = seq(min(rotterdam$dtime), max(rotterdam$dtime), length.out = 25),
  hormon = 0:1,
  model = model,
  grid_type = "counterfactual"
)
p = avg_predictions(model, type = "expected", newdata = grid, by = c("dtime", "hormon")) |> 
  inferences(method = "boot")

