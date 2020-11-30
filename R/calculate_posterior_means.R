#' Calculate posterior means for eaps/wles/pvs
#' @param eap data.frame; expected a posteriori point estimates per person
#' @param wle data.frame; weighted maximum likelihood estimates per person
#' @param pv list of data.frames; contains completed bgdata and pvs
#' @param waves character vector; assessment waves ("_wx", "_wy")
#' @param npv numeric; number of plausible values
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