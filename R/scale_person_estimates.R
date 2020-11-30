#' Re-scale plausible values to fit linked distributions:
#' The mean difference of the longitudinal sample must be the linking constant
#' @param pv plausible values
#' @param wle Warm's weighted maximum likelihood estimate
#' @param eap expected a posteriori point estimate
#' @param SC starting cohort
#' @param domain competence domain
#' @param waves all waves for a specific SC/domain
#' @param long_IDs for SC6/RE contains longitudinal subsamples for
#' original and refreshment samples separately; else contains sample IDs for
#' time points 1 and 2 (index 1), 2 and 3 (index 2), 3 and 4 (index 3) asf.
#'
#' @noRd

scale_person_estimates <- function(pv, wle, eap, SC, domain, waves, long_IDs) {
  waves_ <- gsub("_", "", waves)
  if (SC == "SC6" && domain == "RE") {
    res <- rescale_sc6_reading(SC, domain, long_IDs, eap, pv, wle)
    eap <- res[["eap"]]
    wle <- res[["wle"]]
    pv <- res[["pv"]]
  } else if ((SC %in% c("SC3", "SC4") & domain == "EF") ||
             domain %in% c("ORA", "ORB", "NR", "NT")) {
    return(list(pv = pv, wle = wle, eap = eap))
  } else {
    for (w in seq(2, length(waves))) {
      MEAN <- get_mean_linking(
        SC = SC, domain = domain, long_IDs = long_IDs[[w - 1]],
        eap1 = eap[, c("ID_t", paste0("eap", waves[w - 1]))],
        eap2 = eap[, c("ID_t", paste0("eap", waves[w]))],
        wle1 = wle[, c("ID_t", paste0("wle", waves[w - 1]))],
        wle2 = wle[, c("ID_t", paste0("wle", waves[w]))],
        pv = pv, waves = waves, w = w)
      term <-
        MEAN$eap[2] - MEAN$eap[1] - link_constant[[SC]][[domain]][[waves_[w]]]
      term <- correct_linking_term(term, SC, domain, waves_, w)
      eap[, paste0("eap", waves[w])] <- eap[, paste0("eap", waves[w])] - term
      for (i in seq(length(pv))) {
        term <- MEAN$pv[[i]][2] - MEAN$pv[[i]][1] -
          link_constant[[SC]][[domain]][[waves_[w]]]
        term <- correct_linking_term(term, SC, domain, waves_, w)
        pv[[i]][, paste0("PV", waves[w])] <-
          pv[[i]][, paste0("PV", waves[w])] - term
      }
      if (!is.null(wle)) {
        term <-
          MEAN$wle[2] - MEAN$wle[1] - link_constant[[SC]][[domain]][[waves_[w]]]
        term <- correct_linking_term(term, SC, domain, waves_, w)
        wle[, paste0("wle", waves[w])] <-
          wle[, paste0("wle", waves[w])] - term
      }
    }
  }
  list(pv = pv, wle = wle, eap = eap)
}

#' Re-scale plausible values to fit linked distributions for SC6
#' (incl. refreshment sample)
#' @param pv plausible values
#' @param wle Warm's weighted maximum likelihood estimate
#' @param eap expected a posteriori point estimate
#' @param SC starting cohort
#' @param domain competence domain
#' @param wave all waves for a specific SC/domain
#' @param long_IDs for SC6/RE contains longitudinal subsamples for
#' original and refreshment samples separately; else contains sample IDs for
#' time points 1 and 2 (index 1), 2 and 3 (index 2), 3 and 4 (index 3) asf.
#'
#' @noRd

rescale_sc6_reading <- function(SC, domain, long_IDs, eap, pv, wle) {
  MEAN <- get_mean_linking(SC = SC, domain = domain, long_IDs = long_IDs,
                           eap1 = eap, wle1 = wle, pv = pv)
  term1_eap <-
    MEAN$eap[3] - MEAN$eap[1] - 
    link_constant[[SC]][[domain]][["w9"]][["B67"]] - 0.08
  term2_eap <-
    MEAN$eap[4] - MEAN$eap[2] - 
    link_constant[[SC]][[domain]][["w9"]][["B69"]] - 0.15
  eap[eap[["ID_t"]] %in% long_IDs[["w3"]], "eap_w9"] <-
    eap[eap[["ID_t"]] %in% long_IDs[["w3"]], "eap_w9"] - term1_eap
  eap[eap[["ID_t"]] %in% long_IDs[["w5"]], "eap_w9"] <-
    eap[eap[["ID_t"]] %in% long_IDs[["w5"]], "eap_w9"] - term2_eap
  for (i in seq(length(pv))) {
    term1_pv <-
      MEAN$pv[[i]][3] - MEAN$pv[[i]][1] - 
      link_constant[[SC]][[domain]][["w9"]][["B67"]] - 0.08
    term2_pv <-
      MEAN$pv[[i]][4] - MEAN$pv[[i]][2] - 
      link_constant[[SC]][[domain]][["w9"]][["B69"]] - 0.15
    pv[[i]][pv[[i]][["ID_t"]] %in% long_IDs[["w3"]], "PV_w9"] <-
      pv[[i]][pv[[i]][["ID_t"]] %in% long_IDs[["w3"]], "PV_w9"] - term1_pv
    pv[[i]][pv[[i]][["ID_t"]] %in% long_IDs[["w5"]], "PV_w9"] <-
      pv[[i]][pv[[i]][["ID_t"]] %in% long_IDs[["w5"]], "PV_w9"] - term2_pv
  }
  if (!is.null(wle)) {
    term1_wle <-
      MEAN$wle[3] - MEAN$wle[1] - link_constant[[SC]][[domain]][["w9"]][["B67"]]
    term2_wle <-
      MEAN$wle[4] - MEAN$wle[2] - link_constant[[SC]][[domain]][["w9"]][["B69"]]
    wle[wle[["ID_t"]] %in% long_IDs[["w3"]], "wle_w9"] <-
      wle[wle[["ID_t"]] %in% long_IDs[["w3"]], "wle_w9"] - term1_wle + 0.08
    wle[wle[["ID_t"]] %in% long_IDs[["w5"]], "wle_w9"] <-
      wle[wle[["ID_t"]] %in% long_IDs[["w5"]], "wle_w9"] - term2_wle + 0.15
  }
  list(eap = eap, pv = pv, wle = wle)
}
