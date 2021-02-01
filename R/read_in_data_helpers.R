
determine_file_path <- function(path, SC, domain, school = FALSE) {
  files <- list.files(path = path)
  if (school) {
    return(paste0(path, files[grep("CohortProfile", files)]))
  }
  if (SC == "SC5" & domain == "BA") {
    filepath <- paste0(path, files[grep("xEcoCAPI", files)])
  } else if (SC == "SC1" & domain == "CD") {
    filepath <- paste0(path, files[grep("xDirectMeasures", files)])
  } else {
    filepath <- paste0(path, files[grep("xTargetCompetencies", files)])
  }
  filepath
}

create_error_msg <- function(filepath, filetype, school = FALSE) {
  if (school) {
    return(paste0(
      "* Path '", filepath, "' may not contain CohortProfile.\n",
      "* File format: '", filetype, "' might be wrong"
    ))
  }
  paste0(
    "* Path '", filepath, "' may not lead to competence files.\n",
    "* File format: '", filetype, "' might be wrong"
  )
}

import_data <- function(filetype, filepath, error_msg, school = FALSE) {
  if (filetype == "sav") {
    data <-
      tryCatch(
        haven::read_spss(file = filepath, user_na = TRUE),
        error = function(cnd) {
          stop(error_msg, call. = FALSE)
        }
      )
  } else if (filetype == "dta") {
    data <-
      tryCatch(
        haven::read_dta(file = filepath),
        error = function(cnd) {
          stop(error_msg, call. = FALSE)
        }
      )
  } else {
    stop(error_msg, call. = FALSE)
  }
  # sjlabelled because of problems with labelled_spss and tibble class
  data <- sjlabelled::remove_all_labels(data)
  # user defined missings not needed for school id data
  if (school) {
    data[data < -15] <- NA
  }
  data
}
