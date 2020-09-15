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
#' @return the means of the current two waves
#'
#' @noRd

get_mean_linking <- function(SC, domain, long_IDs, eap1, eap2) {
  if (SC == "SC6" && domain == "RE") {
    MEAN <-
      c(
        mean(eap1[eap1$ID_t %in% long_IDs[["w3"]], "eap_w3"],
          na.rm = TRUE
        ),
        mean(eap1[eap1$ID_t %in% long_IDs[["w5"]], "eap_w5"],
          na.rm = TRUE
        ),
        mean(eap1[eap1$ID_t %in% long_IDs[["w3"]], "eap_w9"],
          na.rm = TRUE
        ),
        mean(eap1[eap1$ID_t %in% long_IDs[["w5"]], "eap_w9"],
          na.rm = TRUE
        )
      )
  } else if (SC == "SC2" && domain == "SC") {
    # longitudinal mean biases the wle estimation wrt the suf estimates
    MEAN <- vector("numeric", 2)
    MEAN[1] <- mean(eap1[, 2], na.rm = TRUE)
    MEAN[2] <- mean(eap2[, 2], na.rm = TRUE)
  } else {
    MEAN <- vector("numeric", 2)
    MEAN[1] <- mean(eap1[eap1$ID_t %in% long_IDs, 2], na.rm = TRUE)
    MEAN[2] <- mean(eap2[eap2$ID_t %in% long_IDs, 2], na.rm = TRUE)
  }
  MEAN
}
