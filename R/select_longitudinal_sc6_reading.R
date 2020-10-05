#' Because of resampling in wave 5, two distinct samples have the same items
#'
#' @param data ...
#' @param SC ..
#' @param domain ...
#' @param min_valid ...
#'
#' @noRd

select_longitudinal_sc6_reading <- function(data, SC, domain, min_valid) {
  resp <- list()
  resp[[1]] <-
    data[data[["wave_w3"]] == 1,
         names(data) %in% c("ID_t", item_labels[[SC]][[domain]][["w3"]])]
  resp[[1]] <- resp[[1]][rowSums(!is.na(resp[[1]][, -1])) >= min_valid, ]
  resp[[1]] <- resp[[1]][order(resp[[1]][["ID_t"]]), ]

  resp[[2]] <-
    data[data[["wave_w3"]] == 0 & data[["wave_w5"]] == 1,
         names(data) %in% c("ID_t", item_labels[[SC]][[domain]][["w5"]])]
  resp[[2]] <- resp[[2]][rowSums(!is.na(resp[[2]][, -1])) >= min_valid, ]
  resp[[2]] <- resp[[2]][order(resp[[2]][["ID_t"]]), ]

  resp[[3]] <-
    data[!is.na(data[["rea9_sc1u"]]),
         names(data) %in% c("ID_t", item_labels[[SC]][[domain]][["w9"]])]
  resp[[3]] <- resp[[3]][rowSums(!is.na(resp[[3]][, -1])) >= min_valid, ]
  resp[[3]] <- resp[[3]][order(resp[[3]][["ID_t"]]), ]
  resp
}
