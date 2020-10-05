#' disable print/cat side effects: https://stackoverflow.com/a/54136863
#' @param x ...
#' @noRd
quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}
