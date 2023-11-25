
# FAQ

## Stack Overflow questions

-   [`plot_predictions()` over a range of unobserved
    values](https://stackoverflow.com/questions/72723687/plot-cap-response-curve-for-counterfactual-data)
-   [Plot the marginal effects from a `plm` package
    model](https://stackoverflow.com/questions/73126378/marginal-effects-plot-of-plm/73127507#73127507)
-   [Models with demeaned, polynomials, or transformed
    variables](https://stackoverflow.com/questions/73303108/marginal-effects-for-de-meaned-polynomials-in-mixed-models/73305398#73305398)
-   [`nlme::lme` problem with character
    predictors](https://stackoverflow.com/questions/77516330/marginaleffects-package-refuses-service-with-nlme/77517278#77517278)

## Calling `marginaleffects` in functions, loops, environments, or after re-assigning variables

Functions from the `marginaleffects` package can sometimes fail when
they are called inside a function, loop, or other environments. To see
why, it is important to know that `marginaleffects` often needs to
operate on the original data that was used to fit the model. To extract
this original data, we use the `get_data()` function from the `insight`
package.

In most cases, `get_data()` can extract the data which is stored inside
the model object created by the modeling package. However, some modeling
packages do *not* save the original data in the model object (in order
to save memory). In those cases, `get_data()` will parse the call to
find the name of the data object, and will search for that data object
in the global environment. When users fit models in a different
environment (e.g., function calls), `get_data()` may not be able to
retrieve the original data.

A related problem can arise if users fit a model, but then assign a new
value to the variable that used to store the dataset.

Recommendations:

1.  Supply your dataset explicitly to the `newdata` argument of `slopes`
    functions.
2.  Avoid assigning a new value to a variable that you use to store a
    dataset for model fitting.
