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
                                 type = ifelse(inherits(model, "multinom"), "probs", "response"),
                                 ...) {
  
    softMax <- function(eta){ # softMax transform on matrix with response levels in columns
      exp_eta <- exp(eta)
      return(sweep(exp_eta, 1, STATS=rowSums(exp_eta), FUN="/"))
    }

    type <- sanitize_type(model, type)

    if (inherits(model, "multinom")) {
      # type="link" = additive logratio, "clr" = centered logratio, "ilr" = isometric logratio
      # and "logit" = logistic
      # prediction types are not supported by predict.multinom so
      # we calculate those by hand
      if ((isTRUE(type=="link") | isTRUE(type=="clr") | isTRUE(type=="ilr") | isTRUE(type=="logit"))) {
          mt <- terms(model)
          rhs <- delete.response(mt)
          if (missing(newdata)) {
            m <- model$model
            na.act <- model$na.action
          } else {
            m <- model.frame(rhs, data = newdata, na.action = na.exclude) # na.action = na.omit, xlev = model$xlevels
            na.act <- attr(m, "na.action")
          }
          X <- model.matrix(rhs, m,
                            contrasts.arg = model$contrasts,
                            xlev = model$xlevels)
          betahat <- t(rbind(0, coef(model))) # model coefficients, with expicit zero row 
                                               # added for reference category & transposed
          pred <- X %*% betahat        # predictions for type="link" = additive logratio
          colnames(pred) <- model$lev  # with explicit zero column for reference level included
          
          # for type=="clr" use centered logit scale
          if (isTRUE(type=="clr") | isTRUE(type=="ilr")) pred <- pred - rowMeans(pred)
          
          # for type=="ilr" convert centered logratio coordinates to isometric logratio coordinates
          # using given basis ilrBase
          if (isTRUE(type=="ilr")) { 
            V <- compositions::ilrBase(D = ncol(pred))
            pred <- as.matrix(compositions::clr2ilr(pred, V=V))
            colnames(pred) <- head(model$lev, -1)
          }
          
          if (isTRUE(type=="logit")) { 
            pred <- softMax(pred) # backtransform link scale to response / probs scale
            pred[pred < 1E-5] <- 1E-5 # to avoid overflows
            pred[pred > 0.99999] <- 0.99999
            pred <- qlogis(pred) # transform probs to logit scale
          }
      } else { 
        pred <- stats::predict(model, # for type="probs" = response scale
                               newdata = newdata,
                               type = gsub("logit", "probs", type),
                               ...)
        if ("numeric" %in% class(pred)) pred = as.matrix(t(pred), ncol=1)
      }
    }
            
    if (inherits(model, c("mblogit", "mclogit"))) {
      if (isTRUE(type=="link") | isTRUE(type=="clr") | isTRUE(type=="ilr")) {
        pred <- stats::predict(model, 
                               newdata = newdata,
                               type = "link",
                               ...)
        reference_level <- dimnames(model$D)[[1]][[1]] # add explicit zero column for reference level
        pred <- cbind(0, pred)
        colnames(pred)[1] <- reference_level
        
        # for type=="clr" use centered logit scale
        if (isTRUE(type=="clr") | isTRUE(type=="ilr")) pred <- pred - rowMeans(pred) 
        
        # for type=="ilr" convert centered logratio coordinates to isometric logratio coordinates
        # using given basis ilrBase
        if (isTRUE(type=="ilr")) { 
          V <- compositions::ilrBase(D = ncol(pred))
          pred <- as.matrix(compositions::clr2ilr(pred, V=V))
          colnames(pred) <- head(model$lev, -1)        
          }
      } else { 
        pred <- stats::predict(model, # for type="response"
                               newdata = newdata,
                               type = "response",
                               ...)
        if (isTRUE(type=="logit")) { 
          pred[pred < 1E-5] <- 1E-5 # to avoid overflows
          pred[pred > 0.99999] <- 0.99999
          pred <- qlogis(pred) # transform probs to logit scale
        }
      }
    }
      
  

    # # atomic vector means there is only one row in `newdata`
    # # two levels DV returns a vector 
    # if (isTRUE(checkmate::check_atomic_vector(pred))) {
    #     y_original <- sort(unique(insight::get_response(model)))
    #     two_levels <- length(y_original) == 2
    #     if (isTRUE(two_levels)) {
    #         pred <- matrix(pred)
    #         colnames(pred) <- as.character(y_original[2])
    #     } else {
    #         pred <- matrix(pred, nrow = 1, dimnames = list(NULL, names(pred)))
    #     }
    # }

    # for mblogit & mclogit models we add an explicit zero column 
    # for predictions with type="link" for the reference level to match behaviour
    # of nnet::multinom objects
    # (this allows predictions with type="link" or "logit"
    # to be backtransformed to the response scale using the
    # same softmax transform by rowid)
    # if (inherits(model, c("mblogit", "mclogit")) & 
    #     (isTRUE(type == "link") | isTRUE(type == "latent"))) { 
    #   reference_level <- dimnames(model$D)[[1]][[1]]
    #   pred <- cbind(0, pred)
    #   colnames(pred)[1] <- reference_level
    #   if (is_latent) { # for type=="latent" use centered logit scale
    #     pred <- pred - rowMeans(pred)
    #   }
    # }  
    
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

