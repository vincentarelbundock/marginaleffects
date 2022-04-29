#' Cross join a list of data tables
#'
#' Source: https://github.com/Rdatatable/data.table/issues/1717#issuecomment-545758165
#' @keywords internal
cjdt <- function(dtlist) {
    Reduce(function(DT1, DT2) cbind(DT1, DT2[rep(1:.N, each = nrow(DT1))]), dtlist)
}
