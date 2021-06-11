#' set pvs etc. of persons with not enough valid information to NA
#' cross-sectional case
#'
#' @param pv list of data.frames; contains the completed background data and
#' estimated pv
#' @param eap list of data.frames; contains expected a posteriori estimates
#' @param wle data.frame; contains weighted maximum likelihood estimates
#' @param valid_responses_per_person data.frame; valid responses per person
#' @param min_valid numeric; minimum number of required valid responses
#' @param npv numeric; number of plausible values to be estimated
#'
#' @return list of pv, eap and wle; in each data.frame, persons with less than
#' min_valid responses have been set to NA
#' @noRd

set_not_enough_valid_resp_NA <- function(pv, eap, wle,
                                         valid_responses_per_person,
                                         min_valid, npv) {
  if ("items_not_reached" %in% names(pv[[1]])) {
    vars <- c("items_not_reached", "PV")
  } else {
    vars <- "PV"
  }
  sel <- valid_responses_per_person[["valid"]] < min_valid
  pv <- lapply(pv, function(x) {
    x[sel, vars] <- NA
    x
  })
  eap <- lapply(eap, function(x) {
    x[sel, c("eap", "se")] <- NA
    x
  })
  if (!is.null(wle)) {
    wle[sel, c("wle", "se")] <- NA
  }
  res <- list(pv = pv, eap = eap, wle = wle)
  res
}

#' set pvs etc. of persons with not enough valid information to NA
#' longitudinal case
#'
#' @param npv numeric; number of plausible values to be estimated
#' @param eap list of data.frames; contains expected a posteriori estimates
#' @param wle data.frame; contains weighted maximum likelihood estimates
#' @param valid_responses_per_person data.frame; valid responses per person
#' with one column per assessment wave
#' @param min_valid numeric; minimum number of required valid responses
#' @param waves character vector; contains assment waves ("_wx", "_wy", ...)
#' @param datalist list of data.frames; contains the completed background data
#' and estimated pvs
#'
#' @return list of datalist, eap and wle; in each data.frame, persons with less
#' than  min_valid responses have been set to NA. This has been performed for
#' each assessment wave independently
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
  for (i in seq(length(eap))) {
    for (w in waves) {
      for (j in seq(nrow(valid_responses_per_person))) {
        if (is.na(valid_responses_per_person[j, paste0("valid", w)]) ||
            valid_responses_per_person[j, paste0("valid", w)] < min_valid) {
          eap[[i]][j, paste0(c("eap", "se"), w)] <- NA
        }
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
