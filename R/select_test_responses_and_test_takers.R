#' test data and test taker selection
#'
#' @param longitudinal
#' @param SC
#' @param domain
#' @param data
#' @param wave
#' @param min_valid
#'
#' @noRd

select_test_responses_and_test_takers <- function(longitudinal, SC, domain,
                                                  data, wave, min_valid) {
  if (longitudinal) {
    resp <- list()
    if (SC == "SC6" && domain == "RE") {
      resp[[1]] <-
        data[
          data$wave_w3 == 1,
          names(data) %in% c(
            "ID_t",
            item_labels[[SC]][[domain]][["w3"]]
          )
        ]
      resp[[1]] <- resp[[1]][rowSums(!is.na(resp[[1]][, -1])) >= min_valid, ]
      resp[[1]] <- resp[[1]][order(resp[[1]]$ID_t), ]
      resp[[2]] <-
        data[
          data$wave_w3 == 0 & data$wave_w5 == 1,
          names(data) %in% c(
            "ID_t",
            item_labels[[SC]][[domain]][["w5"]]
          )
        ]
      resp[[2]] <- resp[[2]][rowSums(!is.na(resp[[2]][, -1])) >= min_valid, ]
      resp[[2]] <- resp[[2]][order(resp[[2]]$ID_t), ]
      resp[[3]] <-
        data[
          !is.na(data$rea9_sc1u),
          names(data) %in% c(
            "ID_t",
            item_labels[[SC]][[domain]][["w9"]]
          )
        ]
      resp[[3]] <- resp[[3]][rowSums(!is.na(resp[[3]][, -1])) >= min_valid, ]
      resp[[3]] <- resp[[3]][order(resp[[3]]$ID_t), ]
    } else {
      for (i in seq(length(item_labels[[SC]][[domain]]))) {
        resp[[i]] <-
          data[, names(data) %in% c(
            "ID_t",
            item_labels[[SC]][[domain]][[i]]
          )]
        resp[[i]] <-
          resp[[i]][rowSums(!is.na(resp[[i]][, -1])) >= min_valid, ]
        resp[[i]] <- resp[[i]][order(resp[[i]]$ID_t), ]
      }
    }
    data <-
      data[data$ID_t %in% unique(
        unlist(lapply(resp, function(x) {
          x[["ID_t"]]
        }))
      ), ]
    data <- data[order(data$ID_t), ]
  } else {
    # reading has been tested twice for different samples in SC6:
    # estimating simultaneously might cause problems because of different
    # information available for background model (large quantities of missing data)
    if (SC == "SC6" & domain == "RE") {
      if (wave == "w3") {
        data <- data[data$wave_w3 == 1, ]
      } else if (wave == "w5") {
        data <- data[data$wave_w3 == 0 & data$wave_w5 == 1, ]
      }
    }
    resp <-
      data[, names(data) %in% c(
        "ID_t",
        item_labels[[SC]][[domain]][[wave]]
      )]
    resp <- resp[rowSums(!is.na(resp[, -1])) >= min_valid, ]
    resp <- resp[order(resp$ID_t), ]
    data <- data[data$ID_t %in% resp$ID_t, ]
    data <- data[order(data$ID_t), ]
  }
  list(resp = resp, data = data)
}
