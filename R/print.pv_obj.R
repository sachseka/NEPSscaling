#' Print information about NEPSscaling object
#'
#' @param x return object of function \code{NEPScaling::plausible_values()}
#' @param ... unused
#'
#' @export


print.pv_obj <- function(x, ...) {
  pv_obj <- x
  cat("Plausible Values Estimation with NEPSscaling\n")
  cat("\nStarting Cohort: ", get_starting_cohort(pv_obj = pv_obj), "\n")
  cat("Domain: ", get_domain(pv_obj = pv_obj), "\n")
  cat("Wave(s): ", get_wave(pv_obj = pv_obj), "\n")
  cat("Test takers per wave: ",
    colSums(!is.na(
      get_valid_responses_per_person(pv_obj = pv_obj)[, -1, drop = FALSE]
      )),
    "\n")
  cat("EAP reliability: ",
    round(unlist(get_eap_reliability(pv_obj = pv_obj)), 3), "\n")

  if (get_type(pv_obj = pv_obj) == "longitudinal") {
    if (nrow(get_regression_coefficients(pv_obj = pv_obj)[[1]]) > 1) {
      cat("\nVariables in background model: ",
        paste(rownames(get_regression_coefficients(pv_obj = pv_obj)[[1]])[-1],
            collapse = ", "),
        "\n")
    } else {
      cat("\nVariables in background model:  none\n")
    }
  } else {
    if (nrow(get_regression_coefficients(pv_obj = pv_obj)) > 1) {
      cat("\nVariables in background model: ",
        paste(rownames(get_regression_coefficients(pv_obj = pv_obj))[-1],
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

