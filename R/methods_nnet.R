#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.multinom <- function(model, coefs, ...) {
    # internally, coefficients are held in the `wts` vector, with 0s
    # interspersed. When transforming that vector to a matrix, we see that the
    # first row and first column are all zeros. 
    # NOTE: must use `newdata` in predict otherwise returns stored object.
    b_original <- get_coef(model)
    model$wts[match(b_original, model$wts)] <- coefs
    return(model)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.multinom <- function(model, ...) {
    out <- insight::get_parameters(model, ...)
    out <- stats::setNames(
        out$Estimate,
        sprintf("%s:%s", out$Response, out$Parameter))
    return(out)
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.multinom <- function(model, ...) {
    resp <- insight::get_response(model)
    if (is.factor(resp)) {
        out <- levels(resp)
    } else {
        out <- unique(resp)
    }
    return(out[2:length(out)])
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.multinom <- function(model,
                                 newdata = insight::get_data(model),
                                 vcov = FALSE,
                                 conf_level = 0.95,
                                 type = "probs",
                                 ...) {

    type <- sanitize_type(model, type)

    is_latent <- is_mclogit <- FALSE
    
    if (inherits(model, "multinom")) { # && (type == "link" | type == "latent") 
      mt <- terms(model)
      rhs <- delete.response(mt)
      if (missing(newdata)) {
        m <- model$model
        na.act <- model$na.action
      } else {
        m <- model.frame(rhs, data = newdata, na.action = na.exclude)
        na.act <- attr(m, "na.action")
      }
      X <- model.matrix(rhs, m,
                        contrasts.arg = model$contrasts,
                        xlev = model$xlevels)
      betahat <- t(rbind(0, coef(model ))) # model coefficients, with expicit zero row 
                                           # added for reference category & transposed
      pred <- X %*% betahat 
      colnames(pred) <- model$lev # predictions on the link scale
                                  # with explicit zero column for reference level
      if (type == "latent") { # for type=="latent" use centered logit scale
        pred <- pred - rowMeans(pred) } else if (type == "link") {
        pred <- pred[,-1] # for type=="link" drop first zero reference level column  
                          # as in mclogit::predict.mblogit
      } else if (type == "probs") {
        # normally I would have used original predict method for type == "probs"
        # but there is a bug in predict.multinom which for some models causes it
        # to drop the reference level, so doing it by backtransforming predictions on link scale
        softMax <- function(eta) {
          exp_eta <- exp(eta)
          return(sweep(exp_eta, 1, STATS = rowSums(exp_eta), FUN = "/"))
        }
        pred <- softMax(pred)
      }} else {
        
    if (isTRUE(type == "latent") && inherits(model, c("mblogit", "mclogit"))) {
        is_latent <- TRUE
        is_mclogit <- TRUE
        type <- "link"
    } 

    pred <- stats::predict(model,
                    newdata = newdata,
                    type = type,
                    ...)
    }

    # atomic vector means there is only one row in `newdata`
    # two levels DV returns a vector 
    if (isTRUE(checkmate::check_atomic_vector(pred))) {
        y_original <- sort(unique(insight::get_response(model)))
        two_levels <- length(y_original) == 2
        if (isTRUE(two_levels)) {
            pred <- matrix(pred)
            colnames(pred) <- as.character(y_original[2])
        } else {
            pred <- matrix(pred, nrow = 1, dimnames = list(NULL, names(pred)))
        }
    }

    if (is_latent && is_mclogit) {
        missing_level <- as.character(unique(insight::get_response(model)))
        missing_level <- setdiff(missing_level, colnames(pred))
        if (length(missing_level == 1)) {
            pred <- cbind(0, pred)
            colnames(pred)[1] <- missing_level
            pred <- pred - rowMeans(pred)
        } else {
            stop("Unable to compute predictions on the latent scale.", call. = FALSE)
        }

    }


    # matrix with outcome levels as columns
    out <- data.frame(
        group = rep(colnames(pred), each = nrow(pred)),
        predicted = c(pred))

    # usually when `newdata` is supplied by `comparisons`
    if ("rowid" %in% colnames(newdata)) {
        out$rowid <- rep(newdata$rowid, times = ncol(pred))
    } else {
        out$rowid <- rep(seq_len(nrow(pred)), times = ncol(pred))
    }

    return(out)
}

