#' Contrasts Between Adjusted Predictions
#'
#' This function calculates contrasts (or comparisons) between adjusted
#' predictions for each row of the dataset. The resulting object can processed
#' by the `tidy()` or `summary()` functions, which compute Average Contrasts
#' (see `?summary.marginaleffects`). The `newdata` argument can be used to
#' calculate a variety of contrasts, including "Contrasts at the Mean,"
#' "Contrasts at User-Specified values" (aka Contrasts at Representative
#' values), "Contrasts in Marginal Means", "Adjusted Risk Ratios", and much
#' more. For more information, see the Details and Examples sections below, and
#' in the vignettes on the `marginaleffects` website: <https://vincentarelbundock.github.io/marginaleffects/>
#' * [Getting Started](https://vincentarelbundock.github.io/marginaleffects/#getting-started)
#' * [Contrasts Vignette](https://vincentarelbundock.github.io/marginaleffects/articles/mfx03_contrasts.html)
#' * [Supported Models](https://vincentarelbundock.github.io/marginaleffects/articles/mfx06_supported_models.html)
#' * Case Studies
#'    - [Bayesian analyses with `brms`](https://vincentarelbundock.github.io/marginaleffects/articles/brms.html)
#'    - [Mixed effects models](https://vincentarelbundock.github.io/marginaleffects/articles/lme4.html)
#'    - [Generalized Additive Models](https://vincentarelbundock.github.io/marginaleffects/articles/gam.html)
#'    - [Multinomial Logit and Discrete Choice Models](https://vincentarelbundock.github.io/marginaleffects/articles/mlogit.html)
#'    - [Tables and plots](https://vincentarelbundock.github.io/marginaleffects/articles/modelsummary.html)
#'    - [Robust standard errors and more](https://vincentarelbundock.github.io/marginaleffects/articles/sandwich.html)
#'    - [Transformations and Custom Contrasts: Adjusted Risk Ratio Example](https://vincentarelbundock.github.io/marginaleffects/articles/transformation.html)
#'
#'
#' A "contrast" is the difference between two adjusted predictions, calculated
#' for meaningfully different regressor values (e.g., College graduates vs.
#' Others). Uncertainty estimates are computed using the delta method.
#'
#' Detailed vignettes on contrasts, marginal effects, predictions, and marginal
#' means, as well as a list of supported models can be found on the package
#' website:
#'
#' https://vincentarelbundock.github.io/marginaleffects/
#'
#' @inheritParams marginaleffects
#' @param variables `NULL`, character vector, or named list. The subset of variables for which to compute contrasts.
#' * `NULL`: compute contrasts for all the variables in the model object (can be slow).
#' * Character vector: subset of variables (usually faster).
#' * Named list: subset of variables with the type of contrasts to use, following the conventions in the `contrast_factor` and `contrast_numeric` arguments. Examples:
#'   + `variables = list(gear = "pairwise", hp = 10)`
#'   + `variables = list(gear = "sequential", hp = c(100, 120))`
#'   + See the Examples section below.
#' @param newdata `NULL`, data frame, string, or `datagrid()` call. Determines the predictor values for which to compute contrasts.
#' + `NULL` (default): Unit-level contrasts for each observed value in the original dataset.
#' + data frame: Unit-level contrasts for each row of the `newdata` data frame.
#' + string:
#'   - "mean": Contrasts at the Mean. Contrasts when each predictor is held at its mean or mode.
#'   - "median": Contrasts at the Median. Contrasts when each predictor is held at its median or mode.
#'   - "marginalmeans": Contrasts at Marginal Means.
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid] documentation.
#' @param transform_pre (experimental) string or function. How should pairs of adjusted predictions be contrasted?
#' * string: shortcuts to common contrast functions.
#'   - "difference" (default): `function(hi, lo) hi - lo`
#'   - "differenceavg": `function(hi, lo) mean(hi) - mean(lo)`
#'   - "ratio": `function(hi, lo) hi / lo`
#'   - "lnratio": `function(hi, lo) log(hi / lo)`
#'   - "ratioavg": `function(hi, lo) mean(hi) / mean(lo)`
#'   - "lnratioavg": `function(hi, lo) log(mean(hi) / mean(lo))`
#' * function: accept two equal-length numeric vectors of adjusted predictions (`hi` and `lo`) and returns a vector of contrasts of the same length, or a unique numeric value.
#' @param contrast_factor string. Which pairs of factors should be contrasted?
#' * "reference": Each factor level is compared to the factor reference (base) level
#' * "all": All combinations of observed levels
#' * "sequential": Each factor level is compared to the previous factor level
#' * "pairwise": Each factor level is compared to all other levels
#' @param contrast_numeric string or numeric. Which pairs of numeric values should be contrasted?
#' * Numeric of length 1: Contrast for a gap of `contrast_numeric`, computed at the observed value plus and minus `contrast_numeric / 2`
#' * Numeric vector of length 2: Contrast between the 2nd element and the 1st element of the `contrast_numeric` vector.
#' * "iqr": Contrast across the interquartile range of the regressor.
#' * "sd": Contrast across one standard deviation around the regressor mean.
#' * "2sd": Contrast across two standard deviations around the regressor mean.
#' * "minmax": Contrast between the maximum and the minimum values of the regressor.
#' @param transform_post (experimental) A function applied to the estimate and confidence interval just before returning the final results. For example, users can exponentiate their final results by setting `transform_post=exp` or transform contrasts made on the link scale for ease of interpretation.
#' @param interaction TRUE, FALSE, or NULL
#' * `FALSE`: Contrasts represent the change in adjusted predictions when one predictor changes and all other variables are held constant.
#' * `TRUE`: Contrasts represent the changes in adjusted predictions when the predictors specified in the `variables` argument are manipulated simultaneously.
#' * `NULL` (default): Behaves like `TRUE` when the `variables` argument is specified and the model formula includes interactions. Behaves like `FALSE` otherwise.
#' @template model_specific_arguments
#'
#' @examples
#'
#' library(marginaleffects)
#' library(magrittr)
#'
#' # Linear model
#' tmp <- mtcars
#' tmp$am <- as.logical(tmp$am)
#' mod <- lm(mpg ~ am + factor(cyl), tmp)
#' comparisons(mod, contrast_factor = "reference") %>% tidy()
#' comparisons(mod, contrast_factor = "sequential") %>% tidy()
#' comparisons(mod, contrast_factor = "pairwise") %>% tidy()
#'
#' # GLM with different scale types
#' mod <- glm(am ~ factor(gear), data = mtcars)
#' comparisons(mod, type = "response") %>% tidy()
#' comparisons(mod, type = "link") %>% tidy()
#'
#' # Contrasts at the mean
#' comparisons(mod, newdata = "mean")
#'
#' # Contrasts between marginal means
#' comparisons(mod, newdata = "marginalmeans")
#'
#' # Contrasts at user-specified values
#' comparisons(mod, newdata = datagrid(am = 0, cyl = tmp$cyl))
#'
#' # Numeric contrasts
#' mod <- lm(mpg ~ hp, data = mtcars)
#' comparisons(mod, contrast_numeric = 1) %>% tidy()
#' comparisons(mod, contrast_numeric = 5) %>% tidy()
#' comparisons(mod, contrast_numeric = c(90, 100)) %>% tidy()
#' comparisons(mod, contrast_numeric = "iqr") %>% tidy()
#' comparisons(mod, contrast_numeric = "sd") %>% tidy()
#' comparisons(mod, contrast_numeric = "minmax") %>% tidy()
#'
#' # Adjusted Risk Ratio (see Case Study vignette on the website)
#' mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
#' cmp <- comparisons(mod, transform_pre = "lnratioavg")
#' summary(cmp, transform_post = exp)
#'
#' # Adjusted Risk Ratio: Manual specification of the `transform_pre`
#' cmp <- comparisons(mod, transform_pre = function(hi, lo) log(mean(hi) / mean(lo)))
#' summary(cmp, transform_post = exp)
#
#' # Interactions between contrasts
#' mod <- lm(mpg ~ factor(cyl) * factor(gear) + hp, data = mtcars)
#' cmp <- comparisons(mod, variables = c("cyl", "gear"))
#' summary(cmp)
#'
#' # variable-specific contrasts
#' cmp <- comparisons(mod, variables = list(gear = "sequential", hp = 10))
#' summary(cmp)
#'
#' @export
comparisons <- function(model,
                        newdata = NULL,
                        variables = NULL,
                        type = "response",
                        vcov = TRUE,
                        conf_level = 0.95,
                        contrast_factor = "reference",
                        contrast_numeric = 1,
                        transform_pre = "difference",
                        transform_post = NULL,
                        interaction = NULL,
                        eps = 1e-4,
                        ...) {

    dots <- list(...)

    internal_call <- dots[["internal_call"]]

    if (!isTRUE(internal_call)) {
        # if `newdata` is a call to `datagrid`, `typical`, or `counterfactual`, insert `model`
        # should probably not be nested too deeply in the call stack since we eval.parent() (not sure about this)
        scall <- substitute(newdata)
        if (is.call(scall) && as.character(scall)[1] %in% c("datagrid", "typical", "counterfactual")) {
            lcall <- as.list(scall)
            if (!any(c("model", "data") %in% names(lcall))) {
                lcall <- c(lcall, list("model" = model))
                newdata <- eval.parent(as.call(lcall))
            }
        }
    }

    # `marginaleffects()` runs its own sanity checks and hardcodes valid arguments
    if (!isTRUE(internal_call)) {
        model <- sanitize_model(model = model, newdata = newdata, calling_function = "comparisons", ...)
        conf_level <- sanitize_conf_level(conf_level, ...)
        interaction <- sanitize_interaction(interaction, variables, model)
        sanity_type(model = model, type = type)
        sanity_contrast_factor(contrast_factor) # hardcoded in marginaleffects()
        sanity_contrast_numeric(contrast_numeric) # hardcoded in marginaleffects()
        checkmate::assert_function(transform_post, null.ok = TRUE)
    }

    marginalmeans <- isTRUE(checkmate::check_choice(newdata, choices = "marginalmeans")) # before sanitize_newdata
    newdata <- sanity_newdata(model = model, newdata = newdata)
    transform_pre <- sanitize_transform_pre(transform_pre)

    # get dof before transforming the vcov arg
    if (is.character(vcov) && (isTRUE(vcov == "satterthwaite") || isTRUE(vcov == "kenward-roger"))) {
        mi <- insight::model_info(model)
        V <- get_vcov(model, vcov = vcov)
        df <- insight::find_response(model)
        # predict.lmerTest requires the DV
        if (!df %in% colnames(newdata)) {
            newdata[[df]] <- mean(insight::get_response(model))
        }
        dof <- insight::get_df(model, type = vcov, data = newdata)
    } else {
        dof <- NULL
    }

    vcov_label <- get_vcov_label(vcov)
    vcov <- get_vcov(model, vcov = vcov)

    # variables vector
    variables_list <- sanitize_variables(model = model, newdata = newdata, variables = variables)
    contrast_types <- attr(variables_list, "contrast_types")
    variables <- unique(unlist(variables_list, recursive = TRUE))
    # this won't be triggered for multivariate outcomes in `brms`, which
    # produces a list of lists where top level names correspond to names of the
    # outcomes. There should be a more robust way to handle those, but it seems
    # to work for now.
    if ("conditional" %in% names(variables_list)) {
        variables <- intersect(variables, variables_list[["conditional"]])
    }

    # modelbased::visualisation_matrix attaches useful info for plotting
    attributes_newdata <- attributes(newdata)
    idx <- c("class", "row.names", "names", "data", "reference")
    idx <- !names(attributes_newdata) %in% idx
    attributes_newdata <- attributes_newdata[idx]

    # compute contrasts and standard errors
    args <- list(model = model,
                 newdata = newdata,
                 variables = variables,
                 contrast_factor = contrast_factor,
                 contrast_numeric = contrast_numeric,
                 interaction = interaction,
                 contrast_types = contrast_types,
                 marginalmeans = marginalmeans,
                 contrast_label = transform_pre[["label"]],
                 eps = eps)
    args <- c(args, dots)
    cache <- do.call("get_contrast_data", args)

    args <- list(model,
                 newdata = newdata,
                 variables = variables,
                 type = type,
                 transform_pre = transform_pre[["function"]],
                 contrast_factor = contrast_factor,
                 contrast_numeric = contrast_numeric,
                 eps = eps,
                 cache = cache,
                 marginalmeans = marginalmeans)
    args <- c(args, dots)
    mfx <- do.call("get_contrasts", args)

    # bayesian posterior
    if (!is.null(attr(mfx, "posterior_draws"))) {
        draws <- attr(mfx, "posterior_draws")
        J <- NULL

    # standard errors via delta method
    } else if (isTRUE(checkmate::check_matrix(vcov))) {
        idx <- intersect(colnames(mfx), c("type", "group", "term", "contrast"))
        idx <- mfx[, (idx), drop = FALSE]
        args <- list(model,
                     vcov = vcov,
                     type = type,
                     FUN = standard_errors_delta_contrasts,
                     newdata = newdata,
                     index = idx,
                     variables = variables,
                     cache = cache,
                     transform_pre = transform_pre[["function"]],
                     contrast_factor = contrast_factor,
                     contrast_numeric = contrast_numeric,
                     marginalmeans = marginalmeans,
                     eps = eps)
        args <- c(args, dots)
        se <- do.call("standard_errors_delta", args)
        mfx$std.error <- as.numeric(se)
        J <- attr(se, "J")
        draws <- NULL

    # no standard error
    } else {
        J <- draws <- NULL
    }

    # merge original data back in
    # HACK: relies on NO sorting at ANY point
    if (isTRUE(nrow(mfx) == nrow(cache$original))) {
        idx <- setdiff(colnames(cache$original), colnames(mfx))
        mfx <- data.table(mfx, cache$original[, ..idx])
    }

    # meta info
    mfx[["type"]] <- type

    # bayesian posterior draws
    if (!is.null(draws)) {
        if (!"conf.low" %in% colnames(mfx)) {
            flag <- getOption("marginaleffects_credible_interval", default = "eti")
            if (isTRUE(flag == "hdi")) {
                tmp <- apply(draws, 1, get_hdi, credMass = conf_level)
            } else {
                tmp <- apply(draws, 1, get_eti, credMass = conf_level)
            }
            mfx[["std.error"]] <- NULL
            mfx[["comparison"]] <- apply(draws, 1, stats::median)
            mfx[["conf.low"]] <- tmp[1, ]
            mfx[["conf.high"]] <- tmp[2, ]
        }
    }

    if (is.null(dof)) {
        critical_val <- stats::qnorm((1 - conf_level) / 2)
    } else {
        critical_val <- stats::qt((1 - conf_level) / 2, df = dof)
        if (!"df" %in% colnames(mfx)) {
            mfx[["df"]] <- dof
        }
    }

    if ("std.error" %in% colnames(mfx)) {
        mfx[["std.error"]] <- ifelse(is.na(mfx[["std.error"]]) | mfx[["std.error"]] == 0,
                                     NA,
                                     mfx[["std.error"]])

        if (!"statistic" %in% colnames(mfx)) {
            mfx$statistic <- mfx$comparison / mfx$std.error
        }
        if (!"p.value" %in% colnames(mfx) && "statistic" %in% colnames(mfx)) {
            mfx$p.value <- ifelse(is.na(mfx$statistic),
                                  NA,
                                  2 * stats::pnorm(-abs(mfx$statistic)))
        }
        if (!"conf.low" %in% colnames(mfx)) {
            mfx$conf.low <- mfx$comparison - abs(critical_val) * mfx$std.error
            mfx$conf.high <- mfx$comparison + abs(critical_val) * mfx$std.error
        }
    }

    # group id: useful for merging, only if it's an internal call and not user-initiated
    if (isTRUE(internal_call) && !"group" %in% colnames(mfx)) {
         mfx$group <- "main_marginaleffect"
    }

    if (!is.null(transform_post)) {
        mfx <- backtransform(mfx, transform_post)
    }

    # clean columns
    stubcols <- c("rowid", "rowid_counterfactual", "type", "group", "term",
                  grep("^contrast", colnames(mfx), value = TRUE),
                  "comparison", "std.error", "statistic", "p.value", "conf.low", "conf.high", "df",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(mfx))
    cols <- unique(c(cols, colnames(mfx)))
    mfx <- mfx[, ..cols, drop = FALSE]

    out <- mfx

    if (!isTRUE(internal_call)) {
        setDF(out)
    }
    class(out) <- c("comparisons", class(out))
    attr(out, "posterior_draws") <- draws
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables
    attr(out, "J") <- J
    attr(out, "vcov") <- vcov
    attr(out, "vcov.type") <- get_vcov_label(vcov)
    if (!is.null(transform_post)) {
        attr(out, "transform_post") <- transform_post
    }

    # modelbased::visualisation_matrix attaches useful info for plotting
    for (a in names(attributes_newdata)) {
        attr(out, paste0("newdata_", a)) <- attributes_newdata[[a]]
    }

    return(out)
}
