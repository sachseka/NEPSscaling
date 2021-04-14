#' Calculate posterior means for eaps/wles/pvs
#' @param eap data.frame; expected a posteriori point estimates per person
#' @param wle data.frame; weighted maximum likelihood estimates per person
#' @param pv list of data.frames; contains completed bgdata and pvs
#' @param waves character vector; assessment waves ("_wx", "_wy")
#' @param npv numeric; number of plausible values
#' @noRd

calculate_posterior_means <- function(eap, wle = NULL, pv, waves, npv) {
  MEAN <- list()
  MEAN[["eap"]] <- lapply(eap, function(x) {
    tmp <- colMeans(
      x[, seq(2, (1 + 2 * length(waves)), 2), drop = FALSE], na.rm = TRUE
    )
    names(tmp) <- gsub("_", "", waves)
    tmp
  })
  if (!is.null(wle)) {
    MEAN[["wle"]] <- colMeans(
      wle[, seq(2, (1 + 2 * length(waves)), 2), drop = FALSE], na.rm = TRUE
    )
    names(MEAN[["wle"]]) <- gsub("_", "", waves)
  }
  MEAN[["pv"]] <- list(total = list(), imputations = list())
  MEAN[["pv"]][["imputations"]] <- lapply(pv, function (x) {
    colMeans(x[, grepl("PV", names(x)), drop = FALSE], na.rm = TRUE)
  })
  MEAN[["pv"]][["total"]] <- purrr::reduce(
    MEAN[["pv"]][["imputations"]], `+`
  ) / npv
  MEAN
}

#' Calculate posterior variances for eaps/wles/pvs
#' @param eap data.frame; expected a posteriori point estimates per person
#' @param wle data.frame; weighted maximum likelihood estimates per person
#' @param pv list of data.frames; contains completed bgdata and pvs
#' @param waves character vector; assessment waves ("_wx", "_wy")
#' @param npv numeric; number of plausible values
#' @noRd

calculate_posterior_variances <- function(eap, wle = NULL, pv, waves, npv) {
  VAR <- list()
  VAR[["eap"]] <- lapply(eap, function(x) {
    tmp <- apply(x[, seq(2, (1 + 2 * length(waves)), 2), drop = FALSE],
                 2, var, na.rm = TRUE)
    names(tmp) <- gsub("_", "", waves)
    tmp
  })
  if (!is.null(wle)) {
    VAR[["wle"]] <- apply(wle[, seq(2, (1 + 2 * length(waves)), 2), drop = FALSE],
                          2, var, na.rm = TRUE)
    names(VAR[["wle"]]) <- gsub("_", "", waves)
  }
  VAR[["pv"]] <- list(total = list(), imputations = list())
  VAR[["pv"]][["imputations"]] <- lapply(pv, function (x) {
    apply(x[, grepl("PV", names(x)), drop = FALSE], 2, var, na.rm = TRUE)
  })
  VAR[["pv"]][["total"]] <- purrr::reduce(
    VAR[["pv"]][["imputations"]], `+`
  ) / npv
  VAR
}
