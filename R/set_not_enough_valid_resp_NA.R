#' set pvs etc. of persons with not enough valid information to NA
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

#' set pvs etc. of persons with not enough valid information to NA
#' longitudinal case
#'
#' @param npv ...
#' @param eap ...
#' @param wle ...
#' @param valid_responses_per_person ...
#' @param min_valid ...
#' @param waves ...
#'
#' @noRd

set_not_enough_valid_resp_NA_long <- function(npv, waves, eap, wle, min_valid,
                                              valid_responses_per_person) {
  for (p in seq(npv)) {
    for (w in waves) {
      for (i in seq(nrow(valid_responses_per_person))) {
        if (is.na(valid_responses_per_person[i, paste0("valid", w)]) ||
            valid_responses_per_person[i, paste0("valid", w)] < min_valid) {
          datalist[[p]][i, paste0("PV", w)] <- NA
        }
      }
    }
  }
  for (w in waves) {
    for (j in seq(nrow(valid_responses_per_person))) {
      if (is.na(valid_responses_per_person[j, paste0("valid", w)]) ||
          valid_responses_per_person[j, paste0("valid", w)] < min_valid) {
        eap[j, paste0(c("eap", "se"), w)] <- NA
      }
    }
  }
  if (!is.null(wle)) {
    for (w in waves) {
      for (j in seq(nrow(valid_responses_per_person))) {
        if (is.na(valid_responses_per_person[j, paste0("valid", w)]) ||
            valid_responses_per_person[j, paste0("valid", w)] < min_valid) {
          wle[j, paste0(c("wle", "se"), w)] <- NA
        }
      }
    }
  }
  list(datalist = datalist, eap = eap, wle = wle)
}
