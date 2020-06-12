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

link_longitudinal_plausible_values <- function(datalist, npv,
                                               min_valid,
                                               valid_responses_per_person,
                                               waves, eap, wle,
                                               data, SC, domain, control) {
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
  # longitudinal subsamples
  longitudinal_IDs <- list()
  if (SC == "SC6" && domain == "RE") {
    longitudinal_IDs[["w3"]] <- data[["ID_t"]][
      data[["wave_w3"]] == 1 & !is.na(data[["rea9_sc1u"]])
    ]
    longitudinal_IDs[["w5"]] <- data[["ID_t"]][
      data[["wave_w5"]] == 1 & data[["wave_w3"]] == 0 &
        !is.na(data[["rea9_sc1u"]])
    ]
  } else {
    for (i in seq(2, length(waves))) {
      longitudinal_IDs[[i - 1]] <- eap[["ID_t"]][
        !is.na(eap[paste0("eap", waves[i - 1])]) &
          !is.na(eap[paste0("eap", waves[i])])
      ]
    }
  }
  # re-scaling for longitudinal link
  res <- scale_person_estimates(
    pv = datalist,
    wle = wle,
    eap = eap,
    SC = SC, domain = domain,
    wave = gsub("_", "", waves),
    longitudinal_IDs = longitudinal_IDs
  )

  res
}
