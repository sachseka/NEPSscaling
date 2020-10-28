#' test data and test taker selection
#'
#' @param longitudinal logical; whether longitudinal PVs are estimated
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. for competence domain (e.g. "MA", "RE")
#' @param data data.frame; xTargetCompetencies
#' @param wave character; wave of test ("wx")
#' @param min_valid numeric; minimum number of valid test responses per person
#'
#' @noRd

select_test_responses_and_test_takers <- function(longitudinal, SC, domain,
                                                  data, wave, min_valid) {
  if (longitudinal) {
    if (SC == "SC6" && domain == "RE") {
      resp <- select_longitudinal_sc6_reading(data, SC, domain, min_valid)
    } else {
      resp <- list()
      for (i in seq(length(item_labels[[SC]][[domain]]))) {
        resp[[i]] <-
          data[, names(data) %in% c("ID_t", item_labels[[SC]][[domain]][[i]])]
        resp[[i]] <- resp[[i]][rowSums(!is.na(resp[[i]][, -1])) >= min_valid, ]
        resp[[i]] <- resp[[i]][order(resp[[i]][["ID_t"]]), ]
        if (SC %in% c("SC3", "SC4") & domain == "EF") {
          resp[[i]] <- impute_english_competence_data(resp[[i]], SC, wave = i)
        }
      }
    }
    data <-
      data[
        data[["ID_t"]] %in% unique(unlist(lapply(resp,
                                                 function(x) {x[["ID_t"]]}))), ]
    data <- data[order(data[["ID_t"]]), ]
  } else {
    # reading has been tested twice for different samples in SC6:
    # estimating simultaneously might cause problems because of different
    # information available for background model (large quantities of missing data)
    if (SC == "SC6" & domain == "RE") {
      if (wave == "w3") {
        data <- data[data[["wave_w3"]] == 1, ]
      } else if (wave == "w5") {
        data <- data[data[["wave_w3"]] == 0 & data[["wave_w5"]] == 1, ]
      }
    }
    resp <- data[, c("ID_t", item_labels[[SC]][[domain]][[wave]])]
    resp <- resp[rowSums(!is.na(resp[, -1])) >= min_valid, ]
    resp <- resp[order(resp[["ID_t"]]), ]
    data <- data[data[["ID_t"]] %in% resp[["ID_t"]], ]
    data <- data[order(data[["ID_t"]]), ]
    if (SC == "SC4" & domain == "ST") {
      items <-
        c("stg12nhs_c", "stg12egs_c", "stg12mts_c", "stg12cws_c", "stg12pds_c")
      for (i in items) {
        resp[[i]] <- rowSums(resp[, grep(substr(i, 1, 7), names(resp))],
                             na.rm = TRUE)
      }
      resp <- resp[, c("ID_t", items)]
    }
    if (SC %in% c("SC3", "SC4") & domain == "EF") {
      resp <- impute_english_competence_data(resp, SC, wave)
    }
  }
  list(resp = resp, data = data)
}
