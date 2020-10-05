#' Get mean value for linking longitudinal scales
#'
#' @param SC starting cohort
#' @param domain competence domain
#' @param long_IDs contains sample IDs for the earlier and later time
#' points
#' @param eap1 expected a posteriori point estimate at earlier time point
#' (containing ID_t)
#' @param eap2 expected a posteriori point estimate at later time point
#' (containing ID_t)
#' @param waves ...
#' @param w ...
#' @return the means of the current two waves
#'
#' @noRd

get_mean_linking <- function(eap1, eap2 = NULL, wle1, wle2 = NULL, pv, long_IDs,
                             waves = NULL, w = NULL, SC, domain) {
  MEAN_wle <- NULL
  if (SC == "SC6" && domain == "RE") {
    # eap
    MEAN_eap <-
      c(mean(eap1[eap1$ID_t %in% long_IDs[["w3"]], "eap_w3"], na.rm = TRUE),
        mean(eap1[eap1$ID_t %in% long_IDs[["w5"]], "eap_w5"], na.rm = TRUE),
        mean(eap1[eap1$ID_t %in% long_IDs[["w3"]], "eap_w9"], na.rm = TRUE),
        mean(eap1[eap1$ID_t %in% long_IDs[["w5"]], "eap_w9"], na.rm = TRUE))
    # wle
    if (!is.null(wle1)) {
      MEAN_wle <-
        c(mean(wle1[wle1$ID_t %in% long_IDs[["w3"]], "wle_w3"], na.rm = TRUE),
          mean(wle1[wle1$ID_t %in% long_IDs[["w5"]], "wle_w5"], na.rm = TRUE),
          mean(wle1[wle1$ID_t %in% long_IDs[["w3"]], "wle_w9"], na.rm = TRUE),
          mean(wle1[wle1$ID_t %in% long_IDs[["w5"]], "wle_w9"], na.rm = TRUE))
    }
    # pv
    MEAN_pv <-
      replicate(n = length(pv), expr = vector("numeric", 4), simplify = FALSE)
    for (i in seq(length(pv))) {
      MEAN_pv[[i]] <-
        c(mean(pv[[i]][pv[[i]]$ID_t %in% long_IDs[["w3"]], "PV_w3"], na.rm = TRUE),
          mean(pv[[i]][pv[[i]]$ID_t %in% long_IDs[["w5"]], "PV_w5"], na.rm = TRUE),
          mean(pv[[i]][pv[[i]]$ID_t %in% long_IDs[["w3"]], "PV_w9"], na.rm = TRUE),
          mean(pv[[i]][pv[[i]]$ID_t %in% long_IDs[["w5"]], "PV_w9"], na.rm = TRUE))
    }
  } else {
    # eap
    MEAN_eap <- vector("numeric", 2)
    MEAN_eap[1] <- mean(eap1[eap1$ID_t %in% long_IDs, 2], na.rm = TRUE)
    MEAN_eap[2] <- mean(eap2[eap2$ID_t %in% long_IDs, 2], na.rm = TRUE)
    # wle
    if (!is.null(wle1)) {
      MEAN_wle <- vector("numeric", 2)
      MEAN_wle[1] <- mean(wle1[wle1$ID_t %in% long_IDs, 2], na.rm = TRUE)
      MEAN_wle[2] <- mean(wle2[wle2$ID_t %in% long_IDs, 2], na.rm = TRUE)
    }
    # pv
    MEAN_pv <-
      replicate(n = length(pv), expr = vector("numeric", 2), simplify = FALSE)
    for (i in seq(length(pv))) {
      MEAN_pv[[i]][1] <- mean(pv[[i]][pv[[i]]$ID_t %in% long_IDs,
                                      paste0("PV", waves[w - 1])], na.rm = TRUE)
      MEAN_pv[[i]][2] <- mean(pv[[i]][pv[[i]]$ID_t %in% long_IDs,
                                      paste0("PV", waves[w])], na.rm = TRUE)
    }
  }

  list(eap = MEAN_eap, wle = MEAN_wle, pv = MEAN_pv)
}



# get_mean_linking <- function(SC, domain, long_IDs, eap1, eap2) {
#   if (SC == "SC6" && domain == "RE") {
#     MEAN <-
#       c(mean(eap1[eap1$ID_t %in% long_IDs[["w3"]], "eap_w3"],na.rm = TRUE),
#         mean(eap1[eap1$ID_t %in% long_IDs[["w5"]], "eap_w5"],na.rm = TRUE),
#         mean(eap1[eap1$ID_t %in% long_IDs[["w3"]], "eap_w9"],na.rm = TRUE),
#         mean(eap1[eap1$ID_t %in% long_IDs[["w5"]], "eap_w9"],na.rm = TRUE))
#   } else if (SC == "SC2" && domain == "SC") {
#     # longitudinal mean biases the wle estimation wrt the suf estimates
#     MEAN <- vector("numeric", 2)
#     MEAN[1] <- mean(eap1[, 2], na.rm = TRUE)
#     MEAN[2] <- mean(eap2[, 2], na.rm = TRUE)
#   } else {
#     MEAN <- vector("numeric", 2)
#     MEAN[1] <- mean(eap1[eap1$ID_t %in% long_IDs, 2], na.rm = TRUE)
#     MEAN[2] <- mean(eap2[eap2$ID_t %in% long_IDs, 2], na.rm = TRUE)
#   }
#   MEAN
# }
