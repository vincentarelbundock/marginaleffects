# Similar to match.call() but works better in lapply, etc.
# May be slow due to multiple calls to eval().
construct_call <- function(model, calling_function, env = parent.frame(1L)) {
    call_attr <- list(
        name = calling_function,
        model = model
    )

    all_arg_names <- names(formals(get(calling_function), asNamespace(utils::packageName())))
    arg_names <- setdiff(all_arg_names, c("model", "..."))

    call_attr <- c(
        call_attr,
        stats::setNames(
            lapply(arg_names, function(i) {
                eval(substitute(substitute(arg), list(arg = as.name(i))), envir = env)
            }),
            arg_names
        )
    )

    if ("..." %in% all_arg_names) {
        call_attr <- c(call_attr, as.list(substitute(alist(...), env))[-1L])
    }

    do.call("call", call_attr, quote = TRUE)
}

