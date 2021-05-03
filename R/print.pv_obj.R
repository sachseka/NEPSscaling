#' Print information about NEPSscaling object
#'
#' @param x return object of function \code{NEPSscaling::plausible_values()}
#' @param ... unused
#'
#' @export

print.pv_obj <- function(x, ...) {
  pv_obj <- x
  cat("Plausible Values Estimation with NEPSscaling\n")
  cat("\nStarting Cohort: ", get_starting_cohort(pv_obj = pv_obj), "\n")
  cat("Domain: ", get_domain(pv_obj = pv_obj), "\n")
  cat("Wave(s): ", get_wave(pv_obj = pv_obj), "\n")
  cat("Test takers per wave: ", get_n_testtakers(pv_obj = pv_obj), "\n")
  cat("Number of estimated plausible values: ", get_npv(pv_obj = pv_obj), "\n")
  cat("Number of sampled imputations / completed data: ",
      length(get_eap_reliability(pv_obj = pv_obj)), "\n")

  if (get_type(pv_obj = pv_obj) == "longitudinal") {
    cat("\nEAP reliability: \n")
    tmp <- get_eap_reliability(pv_obj = pv_obj)
    tmp <- matrix(unlist(tmp), nrow = length(tmp), byrow = TRUE)
    colnames(tmp) <- paste0("w", get_wave(pv_obj = pv_obj))
    rownames(tmp) <- names(get_eap_reliability(pv_obj = pv_obj))
    print(round(tmp, 3))
    if (nrow(get_regression_coefficients(pv_obj = pv_obj)[[1]]) > 1) {
      cat("\nVariables in background model: ",
          paste(get_regression_coefficients(pv_obj = pv_obj)[[1]]$Variable[-1],
                collapse = ", "),
          "\n")
    } else {
      cat("\nVariables in background model:  none\n")
    }
  } else {
    tmp <- round(get_eap_reliability(pv_obj = pv_obj), 3)
    if (length(tmp) > 1) {
      tmp <- paste0(tmp, " (", names(tmp), ")", collapse = ", ")
    }
    cat("\nEAP reliability: ", tmp, "\n")
    if (nrow(get_regression_coefficients(pv_obj = pv_obj)) > 1) {
      cat("\nVariables in background model: ",
          paste(get_regression_coefficients(pv_obj = pv_obj)$Variable[-1],
                collapse = ", "),
          "\n")
    } else {
      cat("\nVariables in background model:  none\n")
    }
  }

  cat("\nStarting time: ", paste(pv_obj[["comp_time"]]$initial_time), "\n")
  cat("Time for estimation: ",
    paste(round(pv_obj[["comp_time"]]$estimation_time, 1)),
    units(pv_obj[["comp_time"]]$estimation_time), "\n")
  cat("Total computation time: ",
    paste(round(pv_obj[["comp_time"]]$total_comp_time, 1)),
    units(pv_obj[["comp_time"]]$total_comp_time), "\n")
}

