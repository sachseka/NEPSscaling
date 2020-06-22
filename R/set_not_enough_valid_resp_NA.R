#' set pvs of persons with not enough valid information to NA
#' cross-sectional case
#'
#' @param pv ...
#' @param eap ...
#' @param wle ...
#' @param valid_responses_per_person ...
#' @param min_valid ...
#' @param npv ...
#'
#' @noRd

set_not_enough_valid_resp_NA <- function(pv, eap, wle,
                                         valid_responses_per_person,
                                         min_valid, npv) {
  for (p in seq(npv)) {
    pv[[p]][valid_responses_per_person[["valid"]] < min_valid, "PV"] <- NA
  }
  eap[valid_responses_per_person[["valid"]] < min_valid, "eap"] <- NA
  if (!is.null(wle)) {
    wle[valid_responses_per_person[["valid"]] < min_valid, "wle"] <- NA
  }
  res <- list(pv = pv, eap = eap, wle = wle)
  res
}
