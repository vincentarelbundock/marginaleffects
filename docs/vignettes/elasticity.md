
# Elasticity

In some contexts, it is useful to interpret the results of a regression
model in terms of elasticity or semi-elasticity. One strategy to achieve
that is to estimate a log-log or a semilog model, where the left and/or
right-hand side variables are logged. Another approach is to note that
$\frac{\partial ln(x)}{\partial x}=\frac{1}{x}$, and to post-process the
marginal effects to transform them into elasticities or
semi-elasticities.

For example, say we estimate a linear model of this form:

*y* = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>1</sub> + *β*<sub>2</sub>*x*<sub>2</sub> + *ε*

Let *ŷ* be the adjusted prediction made by the model for some
combination of covariates *x*<sub>1</sub> and *x*<sub>2</sub>. The slope
with respect to *x*<sub>1</sub> (or “marginal effect”) is:

$$\frac{\partial \hat{y}}{\partial x_1}$$

We can estimate the “eyex”, “eydx”, and “dyex” (semi-)elasticities with
respect to *x*<sub>1</sub> as follows:

$$\eta_1=\frac{\partial \hat{y}}{\partial x_1}\cdot \frac{x_1}{\hat{y}}$$
$$\eta_2=\frac{\partial \hat{y}}{\partial x_1}\cdot \frac{1}{\hat{y}}$$
$$\eta_3=\frac{\partial \hat{y}}{\partial x_1}\cdot x_1$$

with interpretations roughly as follows:

1.  A percentage point increase in *x*<sub>1</sub> is associated to a
    *η*<sub>1</sub> percentage points increase in *y*.
2.  A unit increase in *x*<sub>1</sub> is associated to a
    *η*<sub>2</sub> percentage points increase in *y*.
3.  A percentage point increase in *x*<sub>1</sub> is associated to a
    *η*<sub>3</sub> units increase in *y*.

For further intuition, consider the ratio of change in *y* to change in
*x*: $\frac{\Delta y}{\Delta x}$. We can turn this ratio into a ratio
between *relative* changes by dividing both the numerator and the
denominator: $\frac{\frac{\Delta y}{y}}{\frac{\Delta x}{x}}$. This is of
course linked to the expression for the *η*<sub>1</sub> elasticity
above.

With the `marginaleffects` package, these quantities are easy to
compute:

``` r
library(marginaleffects)
mod <- lm(mpg ~ hp + wt, data = mtcars)

avg_slopes(mod)
#> 
#>  Term Estimate Std. Error     z Pr(>|z|)    S   2.5 %  97.5 %
#>    hp  -0.0318    0.00903 -3.52   <0.001 11.2 -0.0495 -0.0141
#>    wt  -3.8778    0.63276 -6.13   <0.001 30.1 -5.1180 -2.6377
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

avg_slopes(mod, slope = "eyex")
#> 
#>  Term Contrast Estimate Std. Error     z Pr(>|z|)    S  2.5 % 97.5 %
#>    hp    eY/eX   -0.285     0.0855 -3.34   <0.001 10.2 -0.453 -0.118
#>    wt    eY/eX   -0.746     0.1418 -5.26   <0.001 22.7 -1.024 -0.468
#> 
#> Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

avg_slopes(mod, slope = "eydx")
#> 
#>  Term Contrast Estimate Std. Error     z Pr(>|z|)    S    2.5 %    97.5 %
#>    hp    eY/dX -0.00173   0.000502 -3.46   <0.001 10.8 -0.00272 -0.000751
#>    wt    eY/dX -0.21165   0.037850 -5.59   <0.001 25.4 -0.28583 -0.137462
#> 
#> Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

avg_slopes(mod, slope = "dyex")
#> 
#>  Term Contrast Estimate Std. Error     z Pr(>|z|)    S  2.5 % 97.5 %
#>    hp    dY/eX    -4.66       1.32 -3.52   <0.001 11.2  -7.26  -2.06
#>    wt    dY/eX   -12.48       2.04 -6.13   <0.001 30.1 -16.47  -8.49
#> 
#> Columns: term, contrast, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```
