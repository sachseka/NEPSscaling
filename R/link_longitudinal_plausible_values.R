#' link longitudinal pvs using pre-scaled linking information
#'
#' @param longitudinal
#' @param datalist
#' @param npv
#' @param min_valid
#' @param valid_responses_per_person
#' @param waves
#' @param eap
#' @param data
#' @param SC
#' @param domain
#' @param control
#'
#' @noRd

link_longitudinal_plausible_values <- function(longitudinal, datalist, npv,
                                               min_valid,
                                               valid_responses_per_person,
                                               waves, eap,
                                               data, SC, domain, control) {
  wave_w3 <- wave_w5 <- rea9_sc1u <- NULL
  if (!longitudinal) {
    pv <- datalist
    for (p in seq(npv)) {
      pv[[p]][valid_responses_per_person$valid < min_valid, "PV"] <- NA
    }
    return(pv)
  }

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
  # longitudinal subsamples
  longitudinal_IDs <- list()
  if (SC == "SC6" && domain == "RE") {
    longitudinal_IDs[["w3"]] <- dplyr::filter(
      data, wave_w3 == 1,
      !is.na(rea9_sc1u)
    )$ID_t
    longitudinal_IDs[["w5"]] <- dplyr::filter(
      data, wave_w3 == 0,
      wave_w5 == 1,
      !is.na(rea9_sc1u)
    )$ID_t
  } else {
    for (i in seq(2, length(waves))) {
      longitudinal_IDs[[i - 1]] <-
        dplyr::filter(
          eap, !is.na(eap[paste0("eap", waves[i - 1])]),
          !is.na(eap[paste0("eap", waves[i])])
        )$ID_t
    }
  }
  # re-scaling for longitudinal link
  res <- scale_person_estimates(
    pv = datalist,
    wle = if (control$WLE) {
      wle
    } else {
      NULL
    },
    eap = eap,
    SC = SC, domain = domain,
    wave = gsub("_", "", waves),
    longitudinal_IDs = longitudinal_IDs
  )
  pv <- res$pv
  wle <- res$wle
  eap <- res$eap

  list(pv = pv, wle = wle, eap = eap)
}
