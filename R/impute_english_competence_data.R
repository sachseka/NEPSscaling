#' because of a large amount of missing data in the subitems of PCM items,
#' subitems have been imputed according to their Rasch solution probability
#'
#' @param resp ...
#' @param SC ...
#' @param wave ...
#'
#' @noRd

impute_english_competence_data <- function(resp, SC, wave) {
  resp[is.na(resp)] <- 0
  dm <- diffMat[[SC]][[wave]][["diff"]][
      diffMat[[SC]][[wave]][["diff"]][["ID_t"]] %in% resp[["ID_t"]], ]
  setNA <- diffMat[[SC]][[wave]][["ind_NA"]][
      diffMat[[SC]][[wave]][["ind_NA"]][["ID_t"]] %in% resp[["ID_t"]], ]
  resp[, -1] <- resp[, -1] + dm[order(dm[["ID_t"]]), -1]
  for (i in 2:ncol(resp)) {
    resp[setNA[[i]], i] <- NA
  }
  resp
}
