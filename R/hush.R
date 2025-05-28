# Execute code silently
# Do not export to avoid conflict with modelsummary
hush <- function(code) {
    void <- utils::capture.output({
        out <- invisible(
            suppressMessages(
                suppressWarnings(
                    tryCatch(code, error = function(e) NULL)
                )
            )
        )
    })
    return(out)
}
