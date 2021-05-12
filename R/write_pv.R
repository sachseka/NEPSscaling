#' Function to write pv_obj to file
#'
#' @param pv_obj return object of function \code{NEPScaling::plausible_values()}
#' @param ext format (either SPSS, Stata (version 14) or Mplus compatible)
#' @param path path to the directory where the plausible values are going to
#' be saved to
#'
#' @examples
#' \dontrun{
#' rm(list = ls())
#' library(NEPScaling)
#' # the pv_obj results was created by plausible_values()
#' path <- "chosen/directory/"
#' write_pv(results, path, "Mplus")
#' write_pv(results, path, "Stata")
#' write_pv(results, path, "SPSS")
#' }
#'
#' @importFrom utils write.table
#' @export

write_pv <- function(pv_obj, path, ext = c("SPSS", "Stata", "Mplus")) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  ext <- match.arg(ext)
  if (!is.character(path) || !grepl("/$", path)) {
    stop("Path must be a character string and end in '/'.")
  }
  if (ext == "SPSS") {
    for (i in 1:length(pv_obj[["pv"]])) {
      haven::write_sav(pv_obj[["pv"]][[i]],
                       path = paste0(path,
                                     "SC", pv_obj[["SC"]], "_",
                                     pv_obj[["domain"]], "_",
                                     "w", pv_obj[["wave"]], "_",
                                     pv_obj[["type"]], "_",
                                     "plausible_values_", i, ".sav"))
    }
  }
  if (ext == "Stata") {
    for (i in 1:length(pv_obj[["pv"]])) {
      colnames(pv_obj[["pv"]][[i]]) <- gsub("[[:punct:]]", "_", colnames(pv_obj[["pv"]][[i]]))
      haven::write_dta(pv_obj[["pv"]][[i]],
                       path = paste0(path,
                                     "SC", pv_obj[["SC"]], "_",
                                     pv_obj[["domain"]], "_",
                                     "w", pv_obj[["wave"]], "_",
                                     pv_obj[["type"]], "_",
                                     "plausible_values_", i, ".dta"),
        version = 14
      )
    }
  }
  if (ext == "Mplus") {
    write(names(pv_obj[["pv"]][[1]]),
          file = paste0(path,
                        "SC", pv_obj[["SC"]], "_",
                        pv_obj[["domain"]], "_",
                        "w", pv_obj[["wave"]], "_",
                        pv_obj[["type"]], "_",
                        "variable_names.txt"))
    for (i in 1:length(pv_obj[["pv"]])) {
      write.table(pv_obj[["pv"]][i],
        file = paste0(path,
                      "SC", pv_obj[["SC"]], "_",
                      pv_obj[["domain"]], "_",
                      "w", pv_obj[["wave"]], "_",
                      pv_obj[["type"]], "_",
                      "plausible_values_", i, ".dat"), dec = ".",
        sep = "\t", row.names = FALSE, col.names = FALSE
      )
    }
    contents <- paste0("SC", pv_obj[["SC"]], "_",
                       pv_obj[["domain"]], "_",
                       "w", pv_obj[["wave"]], "_",
                       pv_obj[["type"]], "_", "plausible_values_1.dat")
    for (i in 2:length(pv_obj[["pv"]])) {
      contents <- paste0(contents, "\n", "SC", pv_obj[["SC"]], "_",
                         pv_obj[["domain"]], "_",
                         "w", pv_obj[["wave"]], "_",
                         pv_obj[["type"]], "_", "plausible_values_", i, ".dat")
    }
    write(contents,
          file = paste0(path, "SC", pv_obj[["SC"]], "_",
                        pv_obj[["domain"]], "_",
                        "w", pv_obj[["wave"]], "_",
                        pv_obj[["type"]], "_", "content_file.dat")
          )
  }
}
