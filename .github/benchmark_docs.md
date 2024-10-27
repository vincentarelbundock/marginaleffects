Every time a PR is opened and a commit is added to the PR, a Github Actions 
workflow is run with a suite of benchmarks. This document explains how this
workflow works.

# Defining the workflow

The workflow running in Github Actions is stored in 
`.github/workflows/benchmarks.yaml`. This runs on every pull request, every time
there is a new commit that modifies a file in `R`, `src`, or `.github/scripts`.
This is to avoid running benchmarks for changes in the README for instance.
After installing required dependencies to run the benchmark (see next section),
the workflow runs the benchmarks, stores a table containing the results, and
automatically adds a comment in the PR with those results. To avoid flooding the
PR with new comments every time a commit is pushed, only one comment is added
and is then updated by new commits.

One thing that makes this workflow slightly more challenging is that we need
the workflow to have the permission to write (meaning using the Github token
associated with it) on our own PRs as well as external PRs introduced via forks.
To avoid giving forks access to our Github token, we will use 
`on: pull_request_target` instead of `on: pull_request`. This will still trigger
the workflow when a new commit is pushed in the PR, but the workflow will run
against the `main` branch and not the against the PR, meaning that any changes
in the workflow made in the PR will not run in Github actions while the PR is
not merged.

This is nicely explained in this blog post: https://jacobtomlinson.dev/posts/2022/commenting-on-pull-requests-with-github-actions/

However this also means that if you open a PR to update 
`.github/workflows/benchmarks.yaml`, then you will need to merge this PR to
see the changes applied in the future workflows. You will not be able to see
the changes "live" in the PR you opened.


# Defining the benchmarks

The R code with the benchmarks is stored in `.github/scripts/benchmarks.R`. It
uses the R packages [`cross`](https://github.com/davisVaughan/cross) and 
[`bench`](https://cran.r-project.org/web/packages/bench/):

* `bench` computes the time and memory used for each expression;
* `cross` allows one to run the same code on several packages, including those
  corresponding to specific commits or branches. The following lines specify
  that we want to run the benchmarks on the CRAN version, the `main` branch of
  the development version, and the current PR in which this workflow runs:
  ```r
  pkgs = c(
    "marginaleffects",
    "vincentarelbundock/marginaleffects",
    paste0("vincentarelbundock/marginaleffects#", pr_number)
  )
  ```

Benchmarks are defined and run in `cross::run()`, which will take care of running
them on all the versions mentioned above. It creates a nested data.frame
with the timings, memory used, and more information for all expressions and
all versions.

The rest of the code cleans this data and computes comparisons between the PR 
and main and between the PR and the CRAN version. Finally, it creates the 
Markdown table and the rest of the content that will go in the comment that is 
automatically posted in the pull request.

# Adding benchmarks

Adding benchmarks can be done in the `bench::mark()` call in 
`.github/scripts/benchmarks.R`, but the more benchmarks there are, the longest
the workflow will take. Generally speaking, the time spent by the workflow 
depends on the number of benchmarks, the number of observations, and the 
number of iterations, so there is a tradeoff.
