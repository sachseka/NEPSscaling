#' link longitudinal pvs using pre-scaled linking information
#'
#' @param datalist ...
#' @param npv ...
#' @param min_valid ...
#' @param valid_responses_per_person ...
#' @param waves ...
#' @param eap ...
#' @param wle ...
#' @param data ...
#' @param SC ...
#' @param domain ...
#' @param control ...
#'
#' @noRd

link_longitudinal_plausible_values <- function(datalist, npv, min_valid,
                                               valid_responses_per_person,
                                               waves, eap, wle,
                                               data, SC, domain, control) {
  res <- set_not_enough_valid_resp_NA_long(npv, waves, eap, wle, min_valid,
                                           valid_responses_per_person,
                                           datalist)
  datalist <- res[["datalist"]]
  eap <- res[["eap"]]
  wle <- res[["wle"]]

  # longitudinal subsamples
  longitudinal_IDs <- identify_longitudinal_ids(SC, domain, data, waves, eap)

  # re-scaling for longitudinal link
  res <- scale_person_estimates(
    pv = datalist, wle = wle, eap = eap, SC = SC, domain = domain,
    wave = gsub("_", "", waves), long_IDs = longitudinal_IDs
  )

  res
}
