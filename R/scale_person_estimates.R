#' Re-scale plausible values to fit linked distributions:
#' The mean difference of the longitudinal sample must be the linking constant
#' @param pv plausible values
#' @param wle Warm's weighted maximum likelihood estimate
#' @param eap list of data.frames; expected a posteriori point estimate
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
        eap = eap,
        wle1 = wle[, c("ID_t", paste0("wle", waves[w - 1]))],
        wle2 = wle[, c("ID_t", paste0("wle", waves[w]))],
        pv = pv, waves = waves, w = w)
      term <- calculate_link_terms(MEAN, SC, domain, waves_, w)
      term$eap <- lapply(term$eap, function(x) {
        correct_linking_term(x, SC, domain, waves_, w)
      })
      if (!is.null(wle)) {
        term$wle <- correct_linking_term(term$wle, SC, domain, waves_, w)
      }
      term$pv <- lapply(term$pv, function(x) {
        correct_linking_term(x, SC, domain, waves_, w)
      })
      res <- apply_linking(eap = eap, pv,
                           wle = if (is.null(wle)) {NULL} else {wle},
                           term, waves, w)
      eap <- res$eap
      if (!is.null(wle)) {6
        wle <- res$wle
      }
      pv <- res$pv
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
                           eap = eap, wle1 = wle, pv = pv)
  term <- calculate_link_terms(MEAN, SC, domain, waves_ = NULL, w = NULL)
  res <- apply_linking_resc6(eap, pv, wle, term, long_IDs)
  res
}



calculate_link_terms <- function(MEAN, SC, domain, waves_, w) {
  term <- list()
  if (SC == "SC6" & domain == "RE") {
    term[["eap"]] <- lapply(1:length(MEAN$eap), function(i) {
      c(MEAN$eap[[i]][3] - MEAN$eap[[i]][1] -
          link_constant[[SC]][[domain]][["w9"]][["B67"]] - 0.08,
        MEAN$eap[[i]][4] - MEAN$eap[[i]][2] -
          link_constant[[SC]][[domain]][["w9"]][["B69"]] - 0.15)
    })
    if (!is.null(MEAN$wle)) {
      term[["wle"]] <- c(MEAN$wle[3] - MEAN$wle[1] -
                           link_constant[[SC]][[domain]][["w9"]][["B67"]] - 0.08,
                         MEAN$wle[4] - MEAN$wle[2] -
                           link_constant[[SC]][[domain]][["w9"]][["B69"]] - 0.15)
    }
    term[["pv"]] <- lapply(1:length(MEAN$pv), function(i) {
      c(MEAN$pv[[i]][3] - MEAN$pv[[i]][1] -
          link_constant[[SC]][[domain]][["w9"]][["B67"]] - 0.08,
        MEAN$pv[[i]][4] - MEAN$pv[[i]][2] -
          link_constant[[SC]][[domain]][["w9"]][["B69"]] - 0.15)
    })
  } else {
    term[["eap"]] <- lapply(1:length(MEAN$eap), function(i) {
      MEAN$eap[[i]][2] - MEAN$eap[[i]][1] -
        link_constant[[SC]][[domain]][[waves_[w]]]
    })
    if (!is.null(MEAN$wle)) {
      term[["wle"]] <- MEAN$wle[2] - MEAN$wle[1] -
        link_constant[[SC]][[domain]][[waves_[w]]]
    }
    term[["pv"]] <- lapply(1:length(MEAN$pv), function(i) {
      MEAN$pv[[i]][2] - MEAN$pv[[i]][1] -
        link_constant[[SC]][[domain]][[waves_[w]]]
    })
  }
  term
}

apply_linking <- function(eap, pv, wle, term, waves, w) {
  for (i in seq(length(eap))) {
    eap[[i]][, paste0("eap", waves[w])] <-
      eap[[i]][, paste0("eap", waves[w])] - term$eap[[i]]
  }
  for (i in seq(length(pv))) {
    pv[[i]][, paste0("PV", waves[w])] <-
      pv[[i]][, paste0("PV", waves[w])] - term$pv[[i]]
  }
  if (!is.null(wle)) {
    wle[, paste0("wle", waves[w])] <- wle[, paste0("wle", waves[w])] - term$wle
  }
  list(eap = eap, pv = pv, wle = wle)
}

apply_linking_resc6 <- function(eap, pv, wle, term, long_IDs) {
  for (i in seq(length(eap))) {
    eap[[i]][eap[[i]][["ID_t"]] %in% long_IDs[["w3"]], "eap_w9"] <-
      eap[[i]][eap[[i]][["ID_t"]] %in% long_IDs[["w3"]], "eap_w9"] - term$eap[[i]][[1]]
    eap[[i]][eap[[i]][["ID_t"]] %in% long_IDs[["w5"]], "eap_w9"] <-
      eap[[i]][eap[[i]][["ID_t"]] %in% long_IDs[["w5"]], "eap_w9"] - term$eap[[i]][[2]]
  }
  for (i in seq(length(pv))) {
    pv[[i]][pv[[i]][["ID_t"]] %in% long_IDs[["w3"]], "PV_w9"] <-
      pv[[i]][pv[[i]][["ID_t"]] %in% long_IDs[["w3"]], "PV_w9"] - term$pv[[i]][[1]]
    pv[[i]][pv[[i]][["ID_t"]] %in% long_IDs[["w5"]], "PV_w9"] <-
      pv[[i]][pv[[i]][["ID_t"]] %in% long_IDs[["w5"]], "PV_w9"] - term$pv[[i]][[2]]
  }
  if (!is.null(wle)) {
    wle[wle[["ID_t"]] %in% long_IDs[["w3"]], "wle_w9"] <-
      wle[wle[["ID_t"]] %in% long_IDs[["w3"]], "wle_w9"] - term$wle[[1]]
    wle[wle[["ID_t"]] %in% long_IDs[["w5"]], "wle_w9"] <-
      wle[wle[["ID_t"]] %in% long_IDs[["w5"]], "wle_w9"] - term$wle[[2]]
  }
  list(eap = eap, pv = pv, wle = wle)
}
