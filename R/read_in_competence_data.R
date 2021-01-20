#' read in competence data
#'
#' @param path file path leading to NEPS competence data (String)
#' @param SC starting cohort; String
#' @param domain competence domain; String
#'
#' @return data.frame of competence tests for SC/domain called data
#' @noRd

read_in_competence_data <- function(path, SC, domain) {
  filepath <- determine_file_path(path, SC, domain, school = FALSE)
  filetype <- tools::file_ext(filepath)
  error_msg <- create_error_msg(filepath, filetype, school = FALSE)
  data <- import_data(filetype, filepath, error_msg, school = FALSE)
  data <- data[order(data[["ID_t"]]), ]
  data
}
