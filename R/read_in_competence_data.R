#' read in competence data
#'
#' @param path file path leading to NEPS competence data (String)
#' @param filetype file type of competence data (Stata or SPSS; String)
#' @param SC starting cohort; String
#' @param domain competence domain; String
#'
#' @return data.frame of competence tests for SC/domain called data
#' @noRd

read_in_competence_data <- function(path, filetype, SC, domain) {
  # get competence data for SC and domain
  files <- list.files(path = path)
  if (SC == "SC5" & domain == "BA") {
    filepath <- paste0(path, files[grep("xEcoCAPI", files)])
  } else {
    filepath <- paste0(path, files[grep("xTargetCompetencies", files)])
  }
  error_msg <- paste0(
      "* Path '", filepath, "' may not lead to competence files.\n",
      "* File format: '", tools::file_ext(filepath), "' might be wrong"
  )
  if (filetype == "SPSS") {
    data <-
      tryCatch(
        haven::read_spss(file = filepath, user_na = TRUE),
        error = function(cnd) {
          stop(cat(error_msg))
        }
      )
  } else {
    data <-
      tryCatch(
        haven::read_dta(file = filepath, user_na = TRUE),
        error = function(cnd) {
          stop(cat(error_msg))
        }
      )
  }
  data <- data[order(data$ID_t), ]
  data
}
