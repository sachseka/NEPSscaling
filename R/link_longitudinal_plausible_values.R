#' link longitudinal pvs using pre-scaled linking information
#'
#' @param datalist list of data.frames; contains pvs and completed bgdata
#' @param npv numeric; number of plausible values
#' @param min_valid numeric; minimum number of required valid responses
#' @param valid_responses_per_person data.frame; valid responses per person
#' and wave
#' @param waves character vector; assessment waves ("_wx", "_wy")
#' @param eap data.frame; expected a posteriori estimates + standard error
#' @param wle data.frame; weighted maximum likelihood estimates + std. error
#' @param data data.frame; xTargetCompetencies
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g., "MA")
#'
#' @return list of mean shifted pv, wle and eap
#' @noRd
link_longitudinal_plausible_values <- function(datalist, npv, min_valid,
                                               valid_responses_per_person,
                                               waves, eap, wle,
                                               data, SC, domain) {
  res <- set_not_enough_valid_resp_NA_long(npv, waves, eap, wle, min_valid,
                                           valid_responses_per_person,
                                           datalist)
  datalist <- res[["datalist"]]
  eap <- res[["eap"]]
  wle <- res[["wle"]]
  # longitudinal subsamples
  long_IDs <- identify_longitudinal_ids(SC, domain, data, waves)
  # re-scaling for longitudinal link
  res <- scale_person_estimates(
    pv = datalist, wle = wle, eap = eap, SC = SC, domain = domain,
    waves = waves, long_IDs = long_IDs
  )
  res
}
