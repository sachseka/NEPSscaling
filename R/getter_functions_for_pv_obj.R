#' Get the starting cohort for which plausible values have been estimated
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the number of the starting cohort as an integer value (see
#' https://www.neps-data.de/en-us/datacenter/dataanddocumentation.aspx for their
#' meaning)
#'
#' @export
get_starting_cohort <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["SC"]]
}

#' Get the domain for which plausible values have been estimated
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the competence domain as a String
#'
#' @export
get_domain <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["domain"]]
}

#' Get the wave for which plausible values have been estimated
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the number of the wave as an integer value
#'
#' @export
get_wave <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["wave"]]
}

#' Get the type of estimation (longitudinal or cross-sectional)
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the type of estimation as a String
#'
#' @export
get_type <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  if (pv_obj[["type"]] == "cross") {
    type <- "cross-sectional"
  } else {
    type <- "longitudinal"
  }
  type
}

#' Get whether the test rotation has been considered during estimation
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return whether the test rotation has been considered as a String
#'
#' @export
get_rotation <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["rotation"]]
}

#' Get the position (first or second) the test has been administered in
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a data.frame containing the position (first or second) the test has
#' been administered in
#'
#' @export
get_test_position <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  if (pv_obj[["rotation"]] == "Corrected For Test Position") {
    return(pv_obj[["position"]])
  } else {
    stop(pv_obj[["rotation"]])
  }
}

#' Get the number of valid responses (i.e., non-missing) required for a test
#' taker to be considered during the estimation
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the minimum number of valid responses as an integer value
#'
#' @export
get_min_valid <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["min_valid"]]
}

#' #' Get the estimation model
#' #'
#' #' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' #' @return the estimation model as a String
#' #'
#' #' @export
#' get_model <- function(pv_obj) {
#'   if (class(pv_obj) != "pv_obj") {
#'     stop("pv_obj must be of class 'pv_obj'.")
#'   }
#'   pv_obj[["model"]]
#' }

#' Get the number of valid responses (i.e., non-missing) for each test taker
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a data.frame containing the number of valid responses per person
#' as an integer value
#'
#' @export
get_valid_responses_per_person <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["valid_responses_per_person"]]
}

#' Get the number of plausible values returned by
#' \code{NEPSscaling::plausible_values()}
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the number of plausible values as an integer value
#'
#' @export
get_npv <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["npv"]]
}

#' Get the list of control variables fed to
#' \code{NEPSscaling::plausible_values()}
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the list of control variables for the estimation algorithms
#'
#' @export
get_control <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["control"]]
}

#' Get the posterior means of the EAPs, WLEs, and plausible values
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the overall mean of the plausible values as a numeric value
#'
#' @export
get_posterior_means <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["posterior_means"]]
}

#' Get the complete list of estimated plausible values and their respective
#' background data
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the list of npv plausible values and their respective background data
#'
#' @export
get_pv_list <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["pv"]]
}

#' Get the i-th plausible value and its respective background data
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @param index the index for the plausible values ranging from 1 to npv
#' (defaults to 1)
#' @return a data.frame containing the i-th plausible values and its respective
#' background data
#'
#' @export
get_pv_index <- function(pv_obj, index = 1) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["pv"]][[index]]
}

#' Get the estimated EAP values and standard errors for each person
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a data.frame containing the EAPs and their respective standard errors
#'
#' @export
get_eap <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  if (pv_obj[["control"]][["EAP"]]) {
    return(pv_obj[["eap"]])
  } else {
    stop("EAPs have not been saved.")
  }
}

#' Get the estimated WLE values and standard errors for each person
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a data.frame containing the WLEs and their respective standard errors
#'
#' @export
get_wle <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  if (pv_obj[["control"]][["WLE"]]) {
    return(pv_obj[["wle"]])
  } else {
    stop("WLEs have not been saved.")
  }
}

#' Get the EAP reliability of the plausible values
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the EAP reliability of the plausible values as a numeric value
#'
#' @export
get_eap_reliability <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["EAP_rel"]]
}

#' Get the WLE reliability of the plausible values
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the WLE reliability of the plausible values as a numeric value
#'
#' @export
get_wle_reliability <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  if (pv_obj[["control"]][["WLE"]]) {
    return(pv_obj[["WLE_rel"]])
  } else {
    stop("WLEs have not been saved.")
  }
}

#' Get the regression coefficients estimated in the latent regression on the
#' background data
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the matrix containing the regression weight for each background
#' variable, including an intercept, as numeric values
#'
#' @export
get_regression_coefficients <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["regr_coeff"]]
}

#' Get the fixed item difficulties and their standard errors
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return the matrix containing the fixed item difficulties and their estimated
#' standard errors as numeric values
#'
#' @export
get_item_difficulties <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["items"]]
}

#' Get include_nr
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a boolean indicating whether the number of not-reached items as a
#' proxy for processing speed should be included in background model
#'
#' @export
get_include_nr <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["include_nr"]]
}

#' Get the file path
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a string of the file path leading to the competence data
#'
#' @export
get_path <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["path"]]
}

#' Get the number of test takers
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a string of the file path leading to the competence data
#'
#' @export
get_n_testtakers <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["n_testtakers"]]
}


#' Get information criteria
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a list of matrices or a matrix of AIC and BIC
#'
#' @export
get_info_criteria <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  pv_obj[["information_criteria"]]
}


#' Get names of used imputations.
#'
#' Imputations and the plausible values estimated with them are chosen randomly,
#' so not all of them might be returned after the estimation. This function
#' shows which ones were actually retained.
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @return a character vector containing the names of the used imputations
#'
#' @export

get_imputation_names <- function(pv_obj) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  names(pv_obj[["treeplot"]])
}


#' Get tree representation of specific imputation.
#'
#' For each imputed variable, a CART was constructed. This function returns its
#' character representation.
#'
#' @param pv_obj return object of function \code{NEPSscaling::plausible_values()}
#' @param imputation Integer or character string value. If it is supplied as an
#' integer it signifies the index of the imputation (ranging from 1 to max.
#' control$ML$nmi) to choose the imputed variable from. As a character string it
#' supplies the name of the imputation (e.g., "imp2").
#' @param variable A character string. Name of the variable to be displayed.
#' @return a character vector containing the tree structure
#'
#' @export

get_imputation_tree <- function(pv_obj, imputation, variable) {
  if (class(pv_obj) != "pv_obj") {
    stop("pv_obj must be of class 'pv_obj'.")
  }
  tree <- pv_obj[["treeplot"]][[imputation]][[variable]]
  cat(tree)
  invisible(tree)
}
