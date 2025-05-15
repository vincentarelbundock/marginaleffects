# Similar to match.call() but works better in lapply, etc.
# May be slow due to multiple calls to eval().
construct_call <- function(model, calling_function, env = parent.frame(1L)) {
    # minimal call
    out <- list(name = calling_function, model = model)

    # known arguments
    arg_names_all <- names(formals(
        get(calling_function, pos = asNamespace("marginaleffects")),
        asNamespace(utils::packageName())
    ))
    arg_names <- setdiff(arg_names_all, c("model", "..."))
    arg <- lapply(arg_names, function(i) {
        eval(substitute(substitute(arg), list(arg = as.name(i))), envir = env)
    })
    out <- c(out, stats::setNames(arg, arg_names))

    # ellipsis
    if ("..." %in% arg_names_all) {
        out <- c(out, as.list(substitute(alist(...), env))[-1L])
    }

    call_out <- do.call("call", out, quote = TRUE)

    # append marginaleffects:: to ensure function comes from package
    if (!startsWith(calling_function, paste0(utils::packageName(), "::"))) {
        call_out[[1L]] <- str2lang(paste0(
            utils::packageName(),
            "::",
            calling_function
        ))
    }

    call_out
}
