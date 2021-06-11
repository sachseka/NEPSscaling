#' Get mean value for linking longitudinal scales
#'
#' @param SC String; starting cohort ("SCx")
#' @param domain String; competence domain (e.g., "MA", "IC")
#' @param long_IDs list of IDs (for SC6 reading) or numeric vector; contains
#' sample IDs for the earlier and later time points
#' @param eap list of data.frames; expected a posteriori point estimates
#' (containing ID_t)
#' @param wle1 data.frame; weighted maximum likelihood estimate at earlier time point
#' (containing ID_t)
#' @param wle2 data.frame; weighted maximum likelihood estimate at later time point
#' (containing ID_t) or NULL for SC6 reading
#' @param waves character; waves of longitudinal competence assessment (e.g.,
#' c("_w1", "_w3"))
#' @param w integer; current wave of assessment
#' @return the means of the current two waves in a list for eap, wle, pv
#'
#' @noRd

get_mean_linking <- function(eap, wle1, wle2 = NULL, pv, long_IDs,
                             waves = NULL, w = NULL, SC, domain) {
  MEAN_wle <- NULL
  if (SC == "SC6" && domain == "RE") {
    # eap
    MEAN_eap <- lapply(eap, function(x) {
      c(mean(x[x$ID_t %in% long_IDs[["w3"]], "eap_w3"], na.rm = TRUE),
        mean(x[x$ID_t %in% long_IDs[["w5"]], "eap_w5"], na.rm = TRUE),
        mean(x[x$ID_t %in% long_IDs[["w3"]], "eap_w9"], na.rm = TRUE),
        mean(x[x$ID_t %in% long_IDs[["w5"]], "eap_w9"], na.rm = TRUE))
    })
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
    MEAN_eap <- lapply(eap, function(x) {
      c(mean(x[x$ID_t %in% long_IDs, paste0("eap", waves[w - 1])], na.rm = TRUE),
        mean(x[x$ID_t %in% long_IDs, paste0("eap", waves[w])], na.rm = TRUE))
    })
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
