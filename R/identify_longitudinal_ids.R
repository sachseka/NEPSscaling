#' identify longitudinal ids for linking
#'
#' @param SC character; starting cohort ("SCx")
#' @param domain character; competence domain (e.g., "RE", "IC")
#' @param data data.frame; xTargetCompetencies
#' @param waves character vector; longitudinal assessment waves ("_wx", ...)
#' @param eap data.frame; expected a posteriori point estimates
#'
#' @noRd

identify_longitudinal_ids <- function (SC, domain, data, waves, eap) {
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
      longitudinal_IDs[[i - 1]] <- data[["ID_t"]][
        !is.na(data[[wle_names[[SC]][[domain]][[gsub("_", "", waves[i - 1])]]]]) &
        !is.na(data[[wle_names[[SC]][[domain]][[gsub("_", "", waves[i])]]]])
        # data[[paste0("wave", waves[i - 1])]] == 1 &
        #   data[[paste0("wave", waves[i])]] == 1
        # !is.na(eap[, paste0("eap_", gsub("_", "", waves[i-1]))]) &
        #   !is.na(eap[, paste0("eap_", gsub("_", "", waves[i]))])
      ]
    }
  }
  longitudinal_IDs
}
