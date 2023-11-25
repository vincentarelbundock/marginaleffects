
# Extensions

This vignette shows how to add support for new models and add new
functionality for supported models.

## Support a new model type

It is very easy to add support for new models in `marginaleffects`. All
we need is to set a global option and define 4 very simple functions.

If you add support for a class of models produced by a CRAN package,
please consider submitting your code for inclusion in the package:
https://github.com/vincentarelbundock/marginaleffects

If you add support for a class of models produced by a package hosted
*elsewhere* than CRAN, you can submit it for inclusion in the
*unsupported* user-submitted library of extensions: Currently

-   [`countreg`
    package](https://github.com/vincentarelbundock/marginaleffects/blob/main/sandbox/methods_countreg.R).
    Thanks to Olivier Beaumais.
-   [`censreg`
    package](https://github.com/vincentarelbundock/marginaleffects/blob/main/sandbox/methods_censReg.R).
    Thanks to Oleg Komashko.

The rest of this section illustrates how to add support for a very
simple lm_manual model.

### Fit function

To begin, we define a function which fits a model. Normally, this
function will be supplied by a modeling package published on CRAN. Here,
we create a function called `lm_manual()`, which estimates a linear
regression model using simple linear algebra operates:

``` r
lm_manual <- function(f, data, ...) {
    # design matrix
    X <- model.matrix(f, data = data)
    # response matrix
    Y <- data[[as.character(f[2])]]
    # coefficients
    b <- solve(crossprod(X)) %*% crossprod(X, Y)
    Yhat <- X %*% b
    # variance-covariance matrix
    e <- Y - Yhat
    df <- nrow(X) - ncol(X)
    s2 <- sum(e^2) / df
    V <- s2 * solve(crossprod(X))
    # model object
    out <- list(
        d = data,
        f = f,
        X = X,
        Y = Y,
        V = V,
        b = b)
    # class name: lm_manual
    class(out) <- c("lm_manual", "list")
    return(out)
}
```

Important: The custom fit function must assign a new class name to the
object it returns. In the example above, the model is assigned to be of
class `lm_manual` (see the penultimate line of code in the function).

Our new function replicates the results of `lm()`:

``` r
model <- lm_manual(mpg ~ hp + drat, data = mtcars)
model$b
#>                    [,1]
#> (Intercept) 10.78986122
#> hp          -0.05178665
#> drat         4.69815776

model_lm <- lm(mpg ~ hp + drat, data = mtcars)
coef(model_lm)
#> (Intercept)          hp        drat 
#> 10.78986122 -0.05178665  4.69815776
```

### `marginaleffects` extension

To extend support in `marginaleffects`, the first step is to tell the
package that our new class is supported. We do this by defining a global
option:

``` r
library(marginaleffects)

options("marginaleffects_model_classes" = "lm_manual")
```

Then, we define 4 methods:

1.  `get_coef()`
    -   Mandatory arguments: `model`, `...`
    -   Returns: named vector of parameters (coefficients).
2.  `set_coef()`
    -   Mandatory arguments: `model`, `coefs` (named vector of
        coefficients), `...`
    -   Returns: A new model object in which the original coefficients
        were replaced by the new vector.
    -   [Example](https://github.com/vincentarelbundock/marginaleffects/blob/main/R/set_coef.R)
3.  `get_vcov()`
    -   Mandatory arguments: `model`, `...`.
    -   Optional arguments: `vcov`
    -   Returns: A named square variance-covariance matrix.
4.  `get_predict()`
    -   Mandatory arguments: `model`, `newdata` (data frame), `...`
    -   Option arguments: `type` and other model-specific arguments.
    -   Returns: A data frame with two columns: a unique `rowid` and a
        column of `estimate` values.

Note that each of these methods will be named with the suffix
`.lm_manual` to indicate that they should be used whenever
`marginaleffects` needs to process an object of class `lm_manual`.

``` r
get_coef.lm_manual <- function(model, ...) {
    b <- model$b
    b <- setNames(as.vector(b), row.names(b))
    return(b)
}

set_coef.lm_manual <- function(model, coefs, ...) {
    out <- model
    out$b <- coefs
    return(out)
}

get_vcov.lm_manual <- function(model, ...) {
    return(model$V)
}

get_predict.lm_manual <- function(model, newdata, ...) {
    newX <- model.matrix(model$f, data = newdata)
    Yhat <- newX %*% model$b
    out <- data.frame(
        rowid = seq_len(nrow(Yhat)),
        estimate = as.vector(Yhat))
    return(out)
}
```

The methods we just defined work as expected:

``` r
get_coef(model)
#> (Intercept)          hp        drat 
#> 10.78986122 -0.05178665  4.69815776

get_vcov(model)
#>             (Intercept)            hp         drat
#> (Intercept) 25.78356135 -3.054007e-02 -5.836030687
#> hp          -0.03054007  8.635615e-05  0.004969385
#> drat        -5.83603069  4.969385e-03  1.419990359

get_predict(model, newdata = head(mtcars))
#>   rowid estimate
#> 1     1 23.41614
#> 2     2 23.41614
#> 3     3 24.06161
#> 4     4 19.56366
#> 5     5 16.52639
#> 6     6 18.31918
```

Now we can use the `avg_slopes` function:

``` r
avg_slopes(model, newdata = mtcars, variables = c("hp", "drat"))
#> 
#>  Term Estimate Std. Error     z Pr(>|z|)    S 2.5 %  97.5 %
#>  drat   4.6982    1.19166  3.94   <0.001 13.6  2.36  7.0338
#>  hp    -0.0518    0.00929 -5.57   <0.001 25.2 -0.07 -0.0336
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

predictions(model, newdata = mtcars) |> head()
#> 
#>  Estimate Std. Error    z Pr(>|z|)     S 2.5 % 97.5 %  mpg cyl disp  hp drat   wt qsec vs am gear carb
#>      23.4      0.671 34.9   <0.001 883.6  22.1   24.7 21.0   6  160 110 3.90 2.62 16.5  0  1    4    4
#>      23.4      0.671 34.9   <0.001 883.6  22.1   24.7 21.0   6  160 110 3.90 2.88 17.0  0  1    4    4
#>      24.1      0.720 33.4   <0.001 810.2  22.6   25.5 22.8   4  108  93 3.85 2.32 18.6  1  1    4    1
#>      19.6      0.999 19.6   <0.001 281.4  17.6   21.5 21.4   6  258 110 3.08 3.21 19.4  1  0    3    1
#>      16.5      0.735 22.5   <0.001 369.1  15.1   18.0 18.7   8  360 175 3.15 3.44 17.0  0  0    3    2
#>      18.3      1.343 13.6   <0.001 138.3  15.7   21.0 18.1   6  225 105 2.76 3.46 20.2  1  0    3    1
#> 
#> Columns: rowid, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high, mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb 
#> Type:  response
```

Note that, for custom model, we typically have to supply values for the
`newdata` and `variables` arguments explicitly.

## Modify or extend supported models

Let’s say you want to estimate a model using the `mclogit::mblogit`
function. That package is already supported by `marginaleffects`, but
you want to use a `type` (scale) of predictions that is not currently
supported: a “centered link scale.”

To achieve this, we would need to override the `get_predict.mblogit()`
method. However, it can be unsafe to reassign methods supplied by a
package that we loaded with `library`. To be safe, we assign a new model
class to our object (“customclass”) which will inherit from `mblogit`.
Then, we define a `get_predict.customclass` method to make our new kinds
of predictions.

Load libraries, estimate a model:

``` r
library(mclogit)
library(data.table)

model <- mblogit(
    factor(gear) ~ am + mpg,
    data = mtcars,
    trace = FALSE)
```

Tell `marginaleffects` that we are adding support for a new class model
models, and assign a new inherited class name to a duplicate of the
model object:

``` r
options("marginaleffects_model_classes" = "customclass")

model_custom <- model

class(model_custom) <- c("customclass", class(model))
```

Define a new `get_predict.customclass` method. We use the default
`predict()` function to obtain predictions. Since this is a multinomial
model, `predict()` returns a matrix of predictions with one column per
level of the response variable.

Our new `get_predict.customclass` method takes this matrix of
predictions, modifies it, and reshapes it to return a data frame with
three columns: `rowid`, `group`, and `estimate`:

``` r
get_predict.customclass <- function(model, newdata, ...) {
    out <- predict(model, newdata = newdata, type = "link")
    out <- cbind(0, out)
    colnames(out)[1] <- dimnames(model$D)[[1]][[1]]
    out <- out - rowMeans(out)
    out <- as.data.frame(out)
    out$rowid <- seq_len(nrow(out))
    out <- data.table(out)
    out <- melt(
        out,
        id.vars = "rowid",
        value.name = "estimate",
        variable.name = "group")
}
```

Finally, we can call any `slopes` function and obtain results. Notice
that our object of class `customclass` now produces different results
than the default `mblogit` object:

``` r
avg_predictions(model)
#> 
#>  Group Estimate Std. Error     z Pr(>|z|)    S 2.5 % 97.5 %
#>      3    0.469     0.0444 10.56  < 0.001 84.2 0.382  0.556
#>      4    0.375     0.0670  5.60  < 0.001 25.5 0.244  0.506
#>      5    0.156     0.0501  3.12  0.00183  9.1 0.058  0.255
#> 
#> Columns: group, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response

avg_predictions(model_custom)
#> 
#>  Group Estimate Std. Error         z Pr(>|z|)   S 2.5 % 97.5 %
#>      3    -1.42       2525 -0.000561    1.000 0.0 -4950   4947
#>      4     6.36       1779  0.003578    0.997 0.0 -3480   3493
#>      5    -4.95       3074 -0.001609    0.999 0.0 -6030   6020
#> 
#> Columns: group, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high 
#> Type:  response
```
