#' Re-scale plausible values to fit linked distributions:
#' The mean difference of the longitudinal sample must be the linking constant
#' @param pv plausible values
#' @param wle Warm's weighted maximum likelihood estimate
#' @param eap expected a posteriori point estimate
#' @param SC starting cohort
#' @param domain competence domain
#' @param wave all waves for a specific SC/domain
#' @param longitudinal_IDs for SC6/RE contains longitudinal subsamples for
#' original and refreshment samples separately; else contains sample IDs for
#' time points 1 and 2 (index 1), 2 and 3 (index 2), 3 and 4 (index 3) asf.
#'
#' @noRd

scale_person_estimates <- function(pv, wle, eap,
                                   SC, domain, wave, longitudinal_IDs) {
  if (SC == "SC6" && domain == "RE") {
    MEAN <- get_mean_linking(SC, domain, longitudinal_IDs,
                             eap1 = eap, eap2 = NULL)
    # original sample
    term1 <- MEAN[3] - MEAN[1] -
      link_constant[[SC]][[domain]][["w9"]][["B67"]]
    # refreshment sample
    term2 <- MEAN[4] - MEAN[2] -
      link_constant[[SC]][[domain]][["w9"]][["B69"]]
    eap[eap[["ID_t"]] %in% longitudinal_IDs[["w3"]], "eap_w9"] <-
      eap[eap[["ID_t"]] %in% longitudinal_IDs[["w3"]], "eap_w9"] - term1
    eap[eap[["ID_t"]] %in% longitudinal_IDs[["w5"]], "eap_w9"] <-
      eap[eap[["ID_t"]] %in% longitudinal_IDs[["w5"]], "eap_w9"] - term2
    for (i in seq(length(pv))) {
      pv[[i]][pv[[i]][["ID_t"]] %in% longitudinal_IDs[["w3"]], "PV_w9"] <-
        pv[[i]][
          pv[[i]][["ID_t"]] %in% longitudinal_IDs[["w3"]],
          "PV_w9"
        ] - term1
      pv[[i]][pv[[i]][["ID_t"]] %in% longitudinal_IDs[["w5"]], "PV_w9"] <-
        pv[[i]][
          pv[[i]][["ID_t"]] %in% longitudinal_IDs[["w5"]],
          "PV_w9"
        ] - term2
    }
    if (!is.null(wle)) {
      wle[wle[["ID_t"]] %in% longitudinal_IDs[["w3"]], "wle_w9"] <-
        wle[wle[["ID_t"]] %in% longitudinal_IDs[["w3"]], "wle_w9"] - term1
      wle[wle[["ID_t"]] %in% longitudinal_IDs[["w5"]], "wle_w9"] <-
        wle[wle[["ID_t"]] %in% longitudinal_IDs[["w5"]], "wle_w9"] - term2
    }
  } else if ((SC == "SC4" & domain == "EF") || domain %in% c("ORA", "ORB")) { # already linked via item parameters!
    return(list(pv = pv, wle = wle, eap = eap))
  } else {
    for (w in seq(2, length(wave))) {
      MEAN <- get_mean_linking(SC, domain,
        longitudinal_IDs = longitudinal_IDs[[w - 1]],
        eap1 = eap[, c("ID_t", paste0("eap_", wave[w - 1]))],
        eap2 = eap[, c("ID_t", paste0("eap_", wave[w]))]
      )
      term <- MEAN[2] - MEAN[1] -
        link_constant[[SC]][[domain]][[wave[w] ]]
      eap[, paste0("eap_", wave[w])] <-
        eap[, paste0("eap_", wave[w])] - term
      for (i in seq(length(pv))) {
        pv[[i]][, paste0("PV_", wave[w])] <-
          pv[[i]][, paste0("PV_", wave[w])] - term
      }
      if (!is.null(wle)) {
        wle[, paste0("wle_", wave[w])] <-
          wle[, paste0("wle_", wave[w])] - term
      }
    }
  }
  list(pv = pv, wle = wle, eap = eap)
}
