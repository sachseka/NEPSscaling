#' set pvs etc. of persons with not enough valid information to NA
#' cross-sectional case
#'
#' @param pv list of data.frames; contains the background data and
#' estimated pv
#' @param eap data.frame; contains expected a posteriori estimates
#' @param wle data.frame; contains weighted maximum likelihood estimates
#' @param valid_responses_per_person data.frame; valid responses per person
#' @param min_valid numeric; minimum number of required valid responses
#' @param npv numeric; number of plausible values to be estimated
#'
#' @noRd

set_not_enough_valid_resp_NA <- function(pv, eap, wle,
                                         valid_responses_per_person,
                                         min_valid, npv) {
  if ("items_not_reached" %in% names(pv[[1]])) {
    vars <- c("items_not_reached", "PV")
  } else {
    vars <- "PV"
  }
  for (p in seq(npv)) {
    pv[[p]][valid_responses_per_person[["valid"]] < min_valid, vars] <- NA
  }
  eap[valid_responses_per_person[["valid"]] < min_valid, c("eap", "se")] <- NA
  if (!is.null(wle)) {
    wle[valid_responses_per_person[["valid"]] < min_valid, c("wle", "se")] <- NA
  }
  res <- list(pv = pv, eap = eap, wle = wle)
  res
}

#' set pvs etc. of persons with not enough valid information to NA
#' longitudinal case
#'
#' @param npv numeric; number of plausible values to be estimated
#' @param eap data.frame; contains expected a posteriori estimates
#' @param wle data.frame; contains weighted maximum likelihood estimates
#' @param valid_responses_per_person data.frame; valid responses per person
#' with one column per wave
#' @param min_valid numeric; minimum number of required valid responses
#' @param waves character vector; contains assment waves ("_wx", "_wy", ...)
#' @param datalist list of data.frames; contains the background data and
#' estimated pvs
#'
#' @noRd

set_not_enough_valid_resp_NA_long <- function(npv, waves, eap, wle, min_valid,
                                              valid_responses_per_person,
                                              datalist) {
  for (p in seq(npv)) {
    for (w in waves) {
      if (paste0("items_not_reached", w) %in% names(datalist[[p]])) {
        vars <- paste0(c("items_not_reached", "PV"), w)
      } else {
        vars <- paste0("PV", w)
      }
      for (i in seq(nrow(valid_responses_per_person))) {
        if (is.na(valid_responses_per_person[i, paste0("valid", w)]) ||
            valid_responses_per_person[i, paste0("valid", w)] < min_valid) {
          datalist[[p]][i, vars] <- NA
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
