estimates_dictionary_build <- function() {
text <-
'Model,Description,Package,Function
lm,Linear Model,stats,lm
nls,Nonlinear Least Squares,stats,nls
logit,Logistic,stats,glm
probit,Probit,stats,glm
poisson,Poisson,stats,glm
quasipoisson,Quasi-Poisson,stats,glm
beta,Beta Regression,betareg,betareg
betabinomial,Beta-Binomial,aod,betabin
glm,Generalized Linear Model,stats,glm
ologit,Ordered Logistic,MASS,polr
oprobit,Ordered Probit,MASS,polr
ologlog,Ordered Log-Log,MASS,polr
ocloglog,Ordered Complementary Log-Log,MASS,polr
ocauchit,Ordered Cauchit,MASS,polr
robust_lm,Robust Linear,robustbase,lmrob
robust_glm,Robust Generalized Linear,robustbase,glmrob
multinom,Multinomial Log-Linear,nnet,multinom
neg_bin,Negative Binomial,MASS,glm.nb
felm,Fixed Effects Linear Model,fixest,feols
fepoisson,Fixed Effects Poisson,fixest,fepois
feglm,Fixed Effects GLM,fixest,feglm
2sls,Two-Stage Least Squares,ivreg,ivreg
firthlogit,Firth Logitistic,logistf,logistf
firthflic,Firth Logitistic with Intercept Correction,logistf,flac
firthflac,Firth Logitistic with Added Covariate,logistf,flac
melm,Mixed-Effects Linear,lme4,lmer
melogit,Mixed-Effects Logit,lme4,glmer
meprobit,Mixed-Effects Probit,lme4,glmer
mepoisson,Mixed-Effects Poisson,lme4,glmer
meglm,Mixed-Effects Generalized Linear,lme4,glmer
trunc,Truncated Gaussian Response,truncreg,truncreg
cox,Cox Proportional Hazards,survival,coxph
quantile,Quantile Regression,quantreg,rq
0poisson,Zero-Inflated Poisson,pscl,zeroinfl
0negbin,Zero-Inflated Negative Binomial,pscl,zeroinfl
0geometric,Zero-Inflated Geometric,pscl,zeroinfl
heckman,Heckman-Style Selection and Treatment Effect,sampleSelection,selection
heckit,Heckman-Style Selection and Treatment Effect,sampleSelection,heckit
gam,Generalized Additive Model,mgcv,gam
2sls_robust,Two-Stage Least Squares with Robust SEs,estimatr,iv_robust
'
out <- utils::read.csv(
    text = text,
    colClasses = c("character", "character", "character", "character"))
colnames(out) <- gsub("\\.$", "", colnames(out))
for (i in 1:4) {
    out[[i]] <- trimws(out[[i]])
}
out <- out[order(out$Description), ]
class(out) <- c("estimates_dictionary", "data.frame")
row.names(out) <- NULL
return(out)
}


#' estimates dictionary
#'
#' @noRd
estimates_dictionary <- estimates_dictionary_build()


get_function_args <- function(name, pkg) {
    insight::check_if_installed(pkg)
    args <- names(formals(methods::getFunction(name, where = asNamespace(pkg))))
    args <- setdiff(args, c("formula", "data", "model", "fml", "..."))
    args <- unique(args)
    return(args)
}


print.estimates_dictionary <- function(x, ...) {
    flag <- insight::check_if_installed("knitr", quietly = TRUE)
    if (isTRUE(flag)) {
        cat("\nAvailable models:")
        print(knitr::kable(x, row.names = FALSE))
    } else {
        print(x)
    }
}


check_required_argument <- function(arg, pkg, fun, ...) {
    if (!arg %in% names(list(...))) {
        insight::format_error(
            sprintf("The `%s` argument is required. Please read the documentation:", arg),
            sprintf("?%s::%s ", pkg, fun)
            )
    }
}


#' Fit statistical models to obtain parameter estimates
#'
#' This function offers a single point of entry for fitting many different statistical models. It provides a unified user interface for various model fitting functions, making it easier to switch between models and compare results. If the `model` argument is missing, the function returns a list of available models and the functions used under the hood to fit each model. If the `formula` argument is missing, a description of the model is printed with a list of extra arguments which can be passed to `estimates()` to change the model or estimation procedure.
#'
#' @param formula a formula specifying the model to fit
#' @param data a data frame containing the variables in the formula
#' @param model a character string specifying the model to fit. If missing, returns a list of available models and their corresponding functions.
#' @param ... additional arguments to be passed to the model fitting function
#'
#' @return a model object. These objects can differ from model to model, but they are all supported by `marginaleffects` functions like `predictions()`, `slopes()`, `comparisons()`, and `hypotheses()`. These objects can also be summarized in nice tables using the `modelsummary` package.
#'
#' @examples
#' estimates(gear ~ wt, data = mtcars, model = "lm")
#' 
#' estimates(gear ~ wt, data = mtcars, model = "oprobit")
#'
#' estimates(gear ~ wt + (1 | cyl), data = mtcars, model = "melm")
#' 
#' estimates(gear ~ wt, data = mtcars, model = "oprobit") |>
#'   avg_slopes()
#' 
#' @export
#'
estimates <- function(formula, data, model, ...) {
    call_save <- utils::match.call()
    # what models are available?
    if (missing(model)) {
        return(estimates_dictionary)
    } else {
        checkmate::assert_choice(model, estimates_dictionary$Model)
    }

    fun_name <- estimates_dictionary[estimates_dictionary$Model == model, , drop = FALSE]
    funargs <- get_function_args(fun_name$Function, fun_name$Package)
    funargs <- c("formula", "data", "model", funargs)

    # what arguments are available?
    if (missing(formula)) {
        insight::format_error("The `formula` argument is required.")
    }

    # fit the model and extract estimates
    estimates <- do.call(fun_name$Function, c(list(formula = formula, data = data), list(...)))
    estimates <- extract_estimates(estimates, model)

    return(estimates)
}
estimates <- function(formula, data, model, ...) {
    call_save <- match.call()
    # what models are available?
    if (missing(model)) {
        return(estimates_dictionary)
    } else {
        checkmate::assert_choice(model, estimates_dictionary$Model)
    }

    fun_name <- subset(estimates_dictionary, Model == model)
    funargs <- get_function_args(fun_name$Function, fun_name$Package)
    funargs <- c("formula", "data", "model", funargs)

    # what arguments are available?
    if (missing(formula)) {
        msg <- sprintf("
Model: %s
Package: %s
Function: %s
Documentation: ?%s::%s
Arguments: %s
",
        fun_name$Description, fun_name$Package, fun_name$Function, fun_name$Package, fun_name$Function, paste(funargs, collapse = ", "))
        message(msg)
        return(invisible(estimates_dictionary))
    }

    checkmate::assert_formula(formula)

    # missing data
    if (missing(data) || !isTRUE(checkmate::check_data_frame(data, null.ok = FALSE))) {
        insight::format_error("`data` must be a data.frame.")
    }

    # fit arguments
    args <- list(formula = formula, data = data)
    args <- c(args, list(...))

    # standardize argument names
    if (model %in% c("felm", "fepoisson", "feglm")) {
        args$fml <- args$formula
        args$formula <- NULL
    } else if (model %in% c("logit", "melogit")) {
        args$family = stats::binomial(link = "logit")
    } else if (model %in% c("probit", "meprobit")) {
        args$family = stats::binomial(link = "probit")
    } else if (model %in% c("poisson", "mepoisson")) {
        args$family = stats::poisson()
    } else if (model == "ologit") {
        args$method = "logistic"
    } else if (model == "oprobit") {
        args$method = "probit"
    } else if (model == "ologlog") {
        args$method = "loglog"
    } else if (model == "ocloglog") {
        args$method = "cloglog"
    } else if (model == "ocauchit") {
        args$method = "cauchit"
    } else if (model == "2sls") {
        check_required_argument("instruments", "ivreg", "ivreg", ...)
    } else if (model == "quantreg") {
        check_required_argument("tau", "quantreg", "rq", ...)
    } else if (model == "negbin0") {
        args$dist <- "negbin"
    } else if (model == "geometric") {
        args$dist <- "geometric"
    } else if (model == "heckman") {
        check_required_argument("outcome", "sampleSelection", "selection")
        args$outcome <- args$formula
        args$formula <- NULL
    }

    # convenience: ordinal responses must be factor
    if (model %in% c("ologit", "oprobit", "ologlog", "ocloglog", "ocauchit")) {
        dv <- as.character(as.list(formula)[[2]])
        if (!dv %in% colnames(data)) {
            insight::format_error(sprintf("The dependent variable `%s` is not in the data.", dv))
        }
        if (!is.factor(data[[dv]])) {
            data[[dv]] <- factor(data[[dv]])
            args$data <- data
        }
    }

    FUN <- methods::getFunction(fun_name$Function, where = asNamespace(fun_name$Package))
    out <- do.call(FUN, args)

    if ("call" %in% names(out)) {
        out$call <- call_save
    } else if ("call" %in% names(attributes(out))) {
        attributes(out)$call <- call_save
    }
    return(out)
}