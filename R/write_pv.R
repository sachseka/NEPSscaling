#' Function to write pv.obj to file
#'
#' @param pv.obj    return object of function \code{NEPStools::plausible_values()}
#' @param ext       format (either SPSS, Stata (version 14) or Mplus compatible)
#' @param path      path to the directory where the plausible values are going to
#' be saved to
#'
#' @examples
#' \dontrun{
#' rm(list = ls())
#' library(NEPStools)
#' # the pv.obj results was created by plausible_values()
#' path <- "chosen/directory/"
#' write_pv(results, path, "Mplus")
#' write_pv(results, path, "Stata")
#' write_pv(results, path, "SPSS")
#' }
#'
#' @importFrom utils write.table
#' @export

write_pv <- function(pv.obj, path, ext = c('SPSS', 'Stata', 'Mplus')){
    if (class(pv.obj) != "pv.obj") {
        stop("pv.obj must be of class 'pv.obj'.")
    }
    ext <- match.arg(ext)
    if (!is.character(path) || !grepl("/$",path)) {
        stop("Path must be a character string and end in '/'.")
    }
    if (ext == 'SPSS') {
        for (i in 1: length(pv.obj$pv)) {
            haven :: write_sav(pv.obj$pv[[i]], path = paste0(path, 'plausible_values_', i, '.sav'))
        }
    }
    if (ext == 'Stata') {
        for (i in 1: length(pv.obj$pv)) {
            colnames(pv.obj$pv[[i]]) <- gsub('[[:punct:]]', '_', colnames(pv.obj$pv[[i]]))
            haven :: write_dta(pv.obj$pv[[i]], path = paste0(path, 'plausible_values_', i, '.dta'),
                               version = 14)
        }
    }
    if (ext == 'Mplus') {
        for (i in 1: length(pv.obj$pv)) {
            write.table(pv.obj$pv[i], file = paste0(path, 'plausible_values_', i, '.dat'), dec = '.',
                        sep = ',', row.names = FALSE)
        }
        contents <- 'plausible_values_1.dat'
        for (i in 2:length(pv.obj$pv)) {
            contents <- paste0(contents, '\nplausible_values_', i, '.dat')
        }
        sink(file = paste0(path, 'content_file.dat'))
        cat(contents)
        sink()
    }
}
