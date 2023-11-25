
# Python

The Python programming language offers several powerful libraries for
(bayesian) statistical analysis, such as [`NumPyro`]() and
[`PyMC`.](https://www.pymc.io/welcome.html) This vignette shows how to
use the the full power of `marginaleffects` to analyze and interpret the
results of models estimated by Markov Chain Monte Carlo using the
`NumPyro` Python library.

## Fitting a `NumPyro` model

To begin, we load the `reticulate` package which allows us to interact
with the Python interpreter from an `R` session. Then, we write a
`NumPyro` model and we load it to memory using the `source_python()`
function. The important functions to note in the Python code are:

-   `load_df()` downloads data on pulmonary fibrosis.
-   `model()` defines the `NumPyro` model.
-   `fit_mcmc_model()` fits the model using Markov Chain Monte Carlo.
-   `predict_mcmc()`: accepts a data frame and returns a matrix of draws
    from the posterior distribution of adjusted predictions (fitted
    values).

``` r
library(reticulate)
library(marginaleffects)

model <- '
## Model code adapted from the NumPyro documtation under Apache License:
## https://num.pyro.ai/en/latest/tutorials/bayesian_hierarchical_linear_regression.html

import pandas as pd
import numpy as np
import numpyro
from numpyro.infer import SVI, Predictive, MCMC,NUTS, autoguide, TraceMeanField_ELBO
import numpyro.distributions as dist
from numpyro.infer.initialization import init_to_median, init_to_uniform,init_to_sample
from jax import random
from sklearn.preprocessing import LabelEncoder
import pickle

def load_df():
    train = pd.read_csv("https://raw.githubusercontent.com/vincentarelbundock/modelarchive/main/data-raw/osic_pulmonary_fibrosis.csv")
    return train


def model(data, predict = False):
    FVC_obs = data["FVC"].values  if predict == False else None
    patient_encoder = LabelEncoder()
    Age_obs = data["Age"].values
    patient_code = patient_encoder.fit_transform(data["Patient"].values)
    μ_α = numpyro.sample("μ_α", dist.Normal(0.0, 500.0))
    σ_α = numpyro.sample("σ_α", dist.HalfNormal(100.0))

    age = numpyro.sample("age", dist.Normal(0.0, 500.0))

    n_patients = len(np.unique(patient_code))

    with numpyro.plate("plate_i", n_patients):
        α = numpyro.sample("α", dist.Normal(μ_α, σ_α))

    σ = numpyro.sample("σ", dist.HalfNormal(100.0))
    FVC_est = α[patient_code] + age * Age_obs

    with numpyro.plate("data", len(patient_code)):
        numpyro.sample("obs", dist.Normal(FVC_est, σ), obs=FVC_obs)


def fit_mcmc_model(train_df, samples = 1000):
    numpyro.set_host_device_count(4)
    rng_key = random.PRNGKey(0)
    mcmc = MCMC(
        NUTS(model),
        num_samples=samples,
        num_warmup=1000,
        progress_bar=True,
        num_chains = 4
        )
    
    mcmc.run(rng_key, train_df)

    posterior_draws = mcmc.get_samples()

    with open("mcmc_posterior_draws.pickle", "wb") as handle:
        pickle.dump(posterior_draws, handle, protocol=pickle.HIGHEST_PROTOCOL)

def predict_mcmc(data):

    with open("mcmc_posterior_draws.pickle", "rb") as handle:
        posterior_draws = pickle.load(handle)

    predictive = Predictive(model = model,posterior_samples=posterior_draws)
    samples = predictive(random.PRNGKey(1), data, predict = True)
    y_pred = samples["obs"]
    # transpose so that each column is a draw and each row is an observation
    y_pred = np.transpose(np.array(y_pred))

    return y_pred 
'

## save python script to temp file
tmp <- tempfile()
cat(model, file = tmp)

## load functions
source_python(tmp)

## download data
df <- load_df()

## fit model
fit_mcmc_model(df)
```

## Analyzing the results in `marginaleffects`

Each of the functions in the `marginaleffects` package requires that
users supply a `model` object on which the function will operate. When
estimating models outside `R`, we do not have such a model object. We
thus begin by creating a “fake” model object: an empty data frame which
we define to be of class “custom”. Then, we set a global option to tell
`marginaleffects` that this “custom” class is supported.

``` r
mod <- data.frame()
class(mod) <- "custom"

options("marginaleffects_model_classes" = "custom")
```

Next, we define a `get_predict` method for our new custom class. This
method must accept three arguments: `model`, `newdata`, and `...`. The
`get_predict` method must return a data frame with one row for each of
the rows in `newdata`, two columns (`rowid` and `estimate`), and an
attribute called `posterior_draws` which hosts a matrix of posterior
draws with the same number of rows as `newdata`.

The method below uses `reticulate` to call the `predict_mcmc()` function
that we defined in the Python script above. The `predict_mcmc()`
function accepts a data frame and returns a matrix with the same number
of rows.

``` r
get_predict.custom <- function(model, newdata, ...) {
    pred <- predict_mcmc(newdata)
    out <- data.frame(
        rowid = seq_len(nrow(newdata)),
        predicted = apply(pred, 1, stats::median)
    )
    attr(out, "posterior_draws") <- pred
    return(out)
}
```

Now we can use most of the `marginaleffects` package functions to
analyze our results. Since we use a “fake” model object,
`marginaleffects` cannot retrieve the original data from the model
object, and we always need to supply a `newdata` argument:

``` r
## predictions on the original dataset
predictions(mod, newdata = df) |> head()

## predictions for user-defined predictor values
predictions(mod, newdata = datagrid(newdata = df, Age = c(60, 70)))

predictions(mod, newdata = datagrid(newdata = df, Age = range))

## average predictions by group
predictions(mod, newdata = df, by = "Sex")

## contrasts (average)
avg_comparisons(mod, variables = "Age", newdata = df)

avg_comparisons(mod, variables = list("Age" = "sd"), newdata = df)

## slope (elasticity)
avg_slopes(mod, variables = "Age", slope = "eyex", newdata = df)
```
