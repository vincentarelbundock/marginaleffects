get_modeldata_mids <- function(model, envir = parent.frame()) {
    # If the user passed a mids directly
    if (inherits(model, "mids")) {
        imp <- model
    } else {
        # Try to recover the mids object from the model's call
        cl <- tryCatch(
            {
                if (!is.null(model$call)) model$call else getCall(model)
            },
            error = function(e) NULL)

        if (is.null(cl) || is.null(cl$data)) {
            stop("Cannot find a 'data' argument in the model call. Pass a 'mids' or 'mira' object.")
        }

        data_expr <- cl$data

        # Environments to try, in order
        envs <- list(
            # user-provided search env
            envir,
            # environment attached to the call (if any)
            attr(cl, ".Environment"),
            # formula/terms env (for lm/glm fits)
            tryCatch(environment(formula(model)), error = function(e) NULL),
            tryCatch(environment(stats::terms(model)), error = function(e) NULL),
            # current and global as fallbacks
            environment(),
            .GlobalEnv
        )

        imp <- NULL
        for (ev in envs) {
            if (is.null(ev)) next
            obj <- tryCatch(eval(data_expr, envir = ev), error = function(e) NULL)
            if (inherits(obj, "mids")) {
                imp <- obj
                break
            }
        }

        if (is.null(imp)) {
            stop("Could not locate the original 'mids' object used to fit the model.")
        }
    }

    # Return list of completed datasets
    lapply(seq_len(imp$m), function(i) mice::complete(imp, action = i))
}
