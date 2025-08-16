#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.gamlss <- function(model, ...) {
    dots <- list(...)

    if (is.null(dots$what)) stop("Argument `what` indicating the parameter of interest is missing.")

    out <- stats::coef(model, what = dots$what)

    return(out)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.gamlss <- function(
    model,
    newdata = insight::get_data(model),
    type = "response",
    ...
) {
    # Get predictions

    dots <- list(...)

    if (is.null(dots$what)) {
        msg <- sprintf(
            "Please specifiy a `what` argument with one of these values: %s",
            toString(model$parameter)
        )
        stop(msg, call. = FALSE)
    }

    # if (!isTRUE(checkmate::check_flag(vcov, null.ok = TRUE))) {
    #   msg <- "The `vcov` argument is not supported for models of this class."
    #   stop(msg, call. = FALSE)
    # }

    # predict.gamlss() breaks when `newdata` includes unknown variables
    origindata <- insight::get_data(model)
    originvars <- colnames(origindata)
    data.table::setDF(newdata)
    index <- which(colnames(newdata) %in% originvars)
    tmp <- newdata[, index]
    hush(
        out <- predict_gamlss(
            model,
            newdata = tmp,
            type = type,
            data = origindata,
            ...
        )
    )

    if ("rowid" %in% colnames(newdata)) {
        out <- data.frame(rowid = newdata$rowid, estimate = out)
    } else {
        out <- data.frame(rowid = seq_along(out), estimate = out)
    }

    return(out)
}

#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.gamlss <- function(model, ...) {
    dots <- list(...)

    if (is.null(dots$what)) {
        msg <- sprintf(
            "Please specifiy a `what` argument with one of these values: %s",
            toString(model$parameter)
        )
        stop(msg, call. = FALSE)
    }

    p <- match(dots$what, model$parameters)

    vc <- stats::vcov(model, what = dots$what)
    index <- which(cumsum(rownames(vc) == "(Intercept)") == p)
    out <- vc[index, index, drop = FALSE]

    return(out)
}

#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.gamlss <- function(model, coefs, ...) {
    dots <- list(...)

    if (is.null(dots$what)) stop("Argument `what` indicating the parameter of interest is missing.")

    p <- paste0(dots$what, ".coefficients")
    model[[p]][names(coefs)] <- coefs
    out <- model

    return(out)
}


# Modified predict method from the R-package gamlss
# Renamed with underscore to avoid conflict with exported method from the package.
predict_gamlss <- function(
    object,
    what = c("mu", "sigma", "nu", "tau"),
    parameter = NULL,
    newdata = NULL,
    type = c("link", "response", "terms"),
    safe = TRUE,
    terms = NULL,
    se.fit = FALSE,
    data = NULL,
    ...
) {
    concat <- function(..., names = NULL) {
        tmp <- list(...)
        if (is.null(names)) {
            names <- names(tmp)
        }
        if (is.null(names)) {
            names <- sapply(as.list(match.call()), deparse)[-1]
        }
        if (any(sapply(tmp, is.matrix) | sapply(tmp, is.data.frame))) {
            len <- sapply(tmp, function(x) c(dim(x), 1)[1])
            len[is.null(len)] <- 1
            data <- rbind(...)
        } else {
            len <- lengths(tmp)
            data <- unlist(tmp)
        }
        namelist <- factor(rep(names, len), levels = names)
        return(data.frame(data, source = namelist))
    }
    if (is.null(newdata)) {
        predictor <- gamlss::lpred(
            object,
            what = what,
            type = type,
            terms = terms,
            se.fit = se.fit,
            ...
        )
        return(predictor)
    }
    if (se.fit) {
        warning(
            " se.fit = TRUE is not supported for new data values at the moment \n"
        )
    }
    if (!(inherits(newdata, "data.frame"))) {
        stop("newdata must be a data frame ")
    }
    what <- if (!is.null(parameter)) {
        match.arg(
            parameter,
            choices = c(
                "mu",
                "sigma",
                "nu",
                "tau"
            )
        )
    } else {
        match.arg(what)
    }
    type <- match.arg(type)
    Call <- object$call
    data <- data1 <- if (is.null(data)) {
        if (!is.null(Call$data)) {
            eval(Call$data)
        } else {
            stop("define the original data using the option data")
        }
    } else {
        data
    }

    data <- data[match(names(newdata), names(data))]
    data <- concat(data, newdata)
    parform <- stats::formula(object, what)
    if (length(parform) == 3) {
        parform[2] <- NULL
    }
    Terms <- stats::terms(parform)
    offsetVar <- if (!is.null(off.num <- attr(Terms, "offset"))) {
        eval(attr(Terms, "variables")[[off.num + 1]], data)
    }
    m <- stats::model.frame(
        Terms,
        data,
        xlev = object[[paste(what, "xlevels", sep = ".")]]
    )
    X <- stats::model.matrix(Terms, data, contrasts = object$contrasts)
    y <- object[[paste(what, "lp", sep = ".")]]
    w <- object[[paste(what, "wt", sep = ".")]]
    onlydata <- data$source == "data"
    smo.mat <- object[[paste(what, "s", sep = ".")]]
    if (!is.null(off.num)) {
        y <- (y - offsetVar[onlydata])
    }
    if (!is.null(smo.mat)) {
        n.smooths <- dim(smo.mat)[2]
        y <- (y - smo.mat %*% rep(1, n.smooths))
    }

    # Modified from the original prediction function.
    if (safe) {
        refit <- stats::lm.wfit(X[onlydata, , drop = FALSE], y, w)
        if (
            abs(sum(stats::resid(refit))) > 0.1 ||
                abs(sum(
                    stats::coef(object, what = what) - stats::coef(refit),
                    na.rm = TRUE
                )) >
                    1e-05
        ) {
            warning(paste(
                "There is a discrepancy  between the original and the re-fit",
                " \n used to achieve 'safe' predictions \n ",
                sep = ""
            ))
        }
        coef <- refit$coef
    } else {
        coef <- stats::coef(object, what = what)
    }

    nX <- dimnames(X)
    rownames <- nX[[1]][!onlydata]
    nrows <- sum(!onlydata)
    nac <- is.na(coef)
    assign.coef <- attr(X, "assign")
    collapse <- type != "terms"
    Xpred <- X[!onlydata, ]
    Xpred <- matrix(Xpred, nrow = nrows)
    if (!collapse) {
        aa <- attr(X, "assign")
        ll <- attr(Terms, "term.labels")
        if (attr(Terms, "intercept") > 0) {
            ll <- c("(Intercept)", ll)
        }
        aaa <- factor(aa, labels = ll)
        asgn <- split(order(aa), aaa)
        hasintercept <- attr(Terms, "intercept") > 0
        p <- refit$qr$rank
        p1 <- seq(len = p)
        piv <- refit$qr$pivot[p1]
        if (hasintercept) {
            asgn$"(Intercept)" <- NULL
            avx <- colMeans(X[onlydata, ])
            termsconst <- sum(avx[piv] * coef[piv])
        }
        nterms <- length(asgn)
        pred <- matrix(ncol = nterms, nrow = nrows)
        dimnames(pred) <- list(rownames(newdata), names(asgn))
        if (hasintercept) {
            Xpred <- sweep(Xpred, 2, avx)
        }
        unpiv <- rep.int(0, NCOL(Xpred))
        unpiv[piv] <- p1
        for (i in seq(1, nterms, length = nterms)) {
            iipiv <- asgn[[i]]
            ii <- unpiv[iipiv]
            iipiv[ii == 0] <- 0
            pred[, i] <- if (any(iipiv > 0)) {
                Xpred[, iipiv, drop = FALSE] %*% coef[iipiv]
            } else {
                0
            }
        }
        attr(pred, "constant") <- if (hasintercept) {
            termsconst
        } else {
            0
        }
        if (!is.null(terms)) {
            pred <- pred[, terms, drop = FALSE]
        }
    } else {
        pred <- drop(Xpred[, !nac, drop = FALSE] %*% coef[!nac])
        if (!is.null(off.num) && collapse) {
            pred <- pred + offsetVar[!onlydata]
        }
    }
    if (!is.null(smo.mat)) {
        cat("new prediction", "\n")
        smooth.labels <- dimnames(smo.mat)[[2]]
        pred.s <- array(
            0,
            c(nrows, n.smooths),
            list(
                names(pred),
                dimnames(smo.mat)[[2]]
            )
        )
        smooth.calls <- lapply(m[smooth.labels], attr, "call")
        data <- subset(m, onlydata, drop = FALSE)
        attr(data, "class") <- NULL
        new.m <- subset(m, !onlydata, drop = FALSE)
        attr(new.m, "class") <- NULL
        residuals <- if (!is.null(off.num)) {
            object[[paste(what, "wv", sep = ".")]] -
                object[[paste(what, "lp", sep = ".")]] +
                offsetVar[onlydata]
        } else {
            object[[paste(what, "wv", sep = ".")]] -
                object[[paste(what, "lp", sep = ".")]]
        }
        for (TT in smooth.labels) {
            if (is.matrix(m[[TT]])) {
                nm <- names(attributes(m[[TT]]))
                attributes(data[[TT]]) <- attributes(m[[TT]])[nm[
                    -c(
                        1,
                        2
                    )
                ]]
            } else {
                attributes(data[[TT]]) <- attributes(m[[TT]])
            }
            Call <- smooth.calls[[TT]]
            Call$xeval <- substitute(new.m[[TT]], list(TT = TT))
            z <- residuals + smo.mat[, TT]
            pred.s[, TT] <- eval(Call)
        }
        if (type == "terms") {
            pred[, smooth.labels] <- pred[, smooth.labels] +
                pred.s[, smooth.labels]
        } else {
            pred <- drop(pred + pred.s %*% rep(1, n.smooths))
        }
    }
    if (type == "response") {
        if (methods::is(eval(parse(text = object$family[[1]])), "gamlss.family")) {
            pred <- eval(parse(text = object$family[[1]]))[[paste(
                what,
                "linkinv",
                sep = "."
            )]](pred)
        } else {
            pred <- gamlss.dist::gamlss.family(eval(parse(
                text = paste(
                    stats::family(object)[1],
                    "(",
                    what,
                    ".link=",
                    eval(parse(text = (paste("object$", what, ".link", sep = "")))),
                    ")",
                    sep = ""
                )
            )))[[paste(what, "linkinv", sep = ".")]](pred)
        }
    }
    pred
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.gamlss <- function(model, calling_function, ...) {
    if (calling_function == "hypotheses") {
        msg <- "`marginaleffects` does not support hypothesis tests for models of this class."
        stop_sprintf(msg)
    }
    return(model)
}


