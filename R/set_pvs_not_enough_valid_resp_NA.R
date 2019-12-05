#' set pvs of persons with not enough valid information to NA
#' cross-sectional case
#'
#' @param datalist ...
#' @param valid_responses_per_person ...
#' @param min_valid ...
#' @param npv ...
#'
#' @noRd

set_pvs_not_enough_valid_resp_NA <- function(datalist,
                                             valid_responses_per_person,
                                             min_valid, npv) {
  pv <- datalist
  for (p in seq(npv)) {
    pv[[p]][valid_responses_per_person$valid < min_valid, "PV"] <- NA
  }
  pv
}
