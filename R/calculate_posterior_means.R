#' Calculate posterior means for eaps/wles/pvs
#' @param eap ...
#' @param wle ...
#' @param pv ...
#' @param waves ...
#' @param npv ...
#' @noRd

calculate_posterior_means <- function(eap, wle = NULL, pv, waves, npv) {
  MEAN <- list()
  MEAN[["eap"]] <- colMeans(
    eap[, seq(2, (1 + 2 * length(waves)), 2), drop = FALSE], na.rm = TRUE
  )
  names(MEAN[["eap"]]) <- gsub("_", "", waves)
  if (!is.null(wle)) {
    MEAN[["wle"]] <- colMeans(
      wle[, seq(2, (1 + 2 * length(waves)), 2), drop = FALSE], na.rm = TRUE
    )
    names(MEAN[["wle"]]) <- gsub("_", "", waves)
  }
  MEAN[["pv"]] <- list(total = list(), imputations = list())
  for (i in seq(npv)) {
    MEAN[["pv"]][["imputations"]][[i]] <- colMeans(
      pv[[i]][, grepl("PV", names(pv[[i]])), drop = FALSE], na.rm = TRUE
    )
  }
  MEAN[["pv"]][["total"]] <- purrr::reduce(
    MEAN[["pv"]][["imputations"]], `+`
  ) / npv
  MEAN
}