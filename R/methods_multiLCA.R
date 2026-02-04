#' @rdname get_coef
#' @export
get_coef.multiLCA <- function(model, ...) {
  if (is.null(model$mGamma)) stop("multiLCA object has no mGamma. Fit with Z != NULL and extout = TRUE.")
  b <- as.vector(model$mGamma)
  if (!is.null(model$mV2) && !is.null(rownames(model$mV2))) {
    names(b) <- rownames(model$mV2)
  } else stop("Row names missing from object. Fit with Z != NULL and extout = TRUE.")
  return(b)
}


#' @rdname set_coef
#' @export
set_coef.multiLCA <- function(model, coefs, ...) {
  if (is.null(model$mGamma)) stop("multiLCA object has no mGamma. Fit with Z != NULL and extout = TRUE.")
  P <- nrow(model$mGamma)
  K <- ncol(model$mGamma)
  if (!identical(length(coefs), P * K)) stop("Coefficient vector has wrong length for mGamma.")
  model$mGamma <- matrix(unname(coefs), nrow = P, ncol = K, 
                         dimnames = dimnames(model$mGamma))
  return(model)
}


#' @rdname get_vcov
#' @export
get_vcov.multiLCA <- function(model, ...) {
  if (!is.null(model$Varmat_cor) && 
      isTRUE(as.character(model$estimator) == "Two-step")) return(model$Varmat_cor)
  # One-step estimator does not require a variance correction as structural and 
  # measurement models are fit at once
  else if (!is.null(model$Varmat_unc) && 
           isTRUE(as.character(model$estimator) == "One-step")) return(model$Varmat_unc)
  else if (!is.null(model$Varmat)) return(model$Varmat)
  else stop("No variance-covariance matrix found. Fit with extout = TRUE.")
}


.multiLCA_build_X <- function(newdata, terms) {
  if(any(grepl("\\.", names(newdata)))) stop(
    paste("multiLCA implementation requires variable names without periods.",
          "Remove periods before fitting the LCA model.")
  )
  X <- matrix(NA_real_, nrow = nrow(newdata), ncol = length(terms))
  colnames(X) <- terms
  
  for (j in seq_along(terms)) {
    trm <- terms[j]
    if (identical(trm, "Intercept")) {
      X[, j] <- 1
      next
    }
    # categorical terms
    if (grepl("\\.", trm)) {
      var <- sub("\\..*$", "", trm)
      lvl <- sub("^[^.]*\\.", "", trm)
      if (!var %in% names(newdata)) stop("Missing variable in newdata: ", var)
      x <- newdata[[var]]
      if (is.logical(x)) {
        target <- if (lvl %in% c("TRUE", "FALSE")) (lvl == "TRUE") else as.logical(lvl)
        X[, j] <- as.numeric(x == target)
      } else {
        X[, j] <- as.numeric(as.character(x) == lvl)
      }
      X[is.na(x), j] <- NA_real_
      next
    }
    # numeric terms
    if (!trm %in% names(newdata)) {
      stop("Missing numeric covariate in newdata: ", trm)
    }
    x <- newdata[[trm]]
    if (!is.numeric(x)) {
      suppressWarnings(x_num <- as.numeric(as.character(x)))
      if (any(!is.na(x) & is.na(x_num))) {
        stop("Non-numeric values in covariate '", trm, "' cannot be coerced.")
      }
      x <- x_num
    }
    X[, j] <- x
  }
  return(X)
}


#' @rdname get_predict
#' @export
get_predict.multiLCA <- function(model, newdata, ...) {
  # multiLCA does not have a predict() method
  if (is.null(newdata)) stop("multiLCA prediction requires explicit newdata.")
  newdata <- as.data.frame(newdata)
  
  G <- model$mGamma
  nb_classes <- ncol(G) + 1L
  class_labels <- colnames(model$mPhi)
  
  terms <- sub("^gamma\\(", "", sub("\\|C\\)$", "", rownames(G)))
  B <- cbind(rep(0, nrow(G)), G)  
  colnames(B) <- class_labels
  rownames(B) <- terms
  
  vars <- unique(sub("\\..*$", "", terms))
  vars <- setdiff(vars, "Intercept")
  missing_vars <- setdiff(vars, names(newdata))
  if (length(missing_vars) > 0) {
    stop("Missing required covariates in newdata: ",
         paste(missing_vars, collapse = ", "))
  }
  
  # Create feature matrix from newdata and predict
  X <- .multiLCA_build_X(newdata[, vars, drop = FALSE], terms)
  eta <- X %*% B
  # Numerically stable row-wise softmax
  rw_mx <- apply(eta, 1, max)
  exp_score <- exp(sweep(eta, 1, rw_mx, "-"))
  probs <- exp_score / rowSums(exp_score)
  
  n <- nrow(probs)
  out <- data.frame(
    rowid    = rep(seq_len(n), times = nb_classes),
    group    = rep(colnames(probs), each = n),
    estimate = as.vector(probs),
    row.names = NULL
  )
  out <- add_rowid(out, newdata)
  return(out)
}


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.multiLCA <- function(model, calling_function = NULL, ...) {
  if (!is.null(calling_function) && 
      !isTRUE(as.character(model$spec) == "Single-level LC model with covariates")) {
    msg <- "Multi-level models are not yet supported for `multiLCA` objects."
    stop_sprintf(msg)
  }
  return(model)
}