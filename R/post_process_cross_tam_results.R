#' model post-processing in cross-sectional estimation
#'
#' @param mod estimated TAM model
#' @param npv number of plausible values to return
#' @param control control list for TAM functions
#' @param imp imputed background data
#' @param bgdatacom completed background data set
#' @param eap list of eap values
#' @param i current iteration over background data imputations
#' @param EAP.rel list of eap reliability values
#' @param regr.coeff list of regression coefficients of latent regression
#' @param pvs list of estimated plausible values
#' @param bgdata background data (can be NULL)
#'
#' @noRd

post_process_cross_tam_results <- function(mod, npv, control, imp,
                                           bgdatacom = NULL, eap, i,
                                           EAP.rel, regr.coeff, pvs, bgdata) {
  # impute plausible values
  tmp_pvs <- impute_pvs(mod, npv, control, bgdata, imp, bgdatacom, "", 1)
  eap[[i]] <- suppressWarnings(
    dplyr::left_join(eap[[i]], mod$person[, grep("pid|EAP", names(mod$person))],
                     by = c("ID_t" = "pid"))) %>%
    dplyr::arrange(.data$ID_t)
  EAP.rel <- c(EAP.rel, mod$EAP.rel)
  # se estimation gives warning "In sqrt(-1/info_pp) : NaNs produced" because
  # item difficulty parameters are fixed --> suppress warnings!
  if (i == 1) {
    regr.coeff <- suppressWarnings(quiet(TAM::tam.se(mod)$beta))
    colnames(regr.coeff) <- paste0("imp", i, "_", c("coeff", "se"))
    rownames(regr.coeff) <-
      c("Intercept",
        names(bgdata[, -which(names(bgdata) == "ID_t"), drop = FALSE]))
  } else if (i > 1) {
    tmp <- suppressWarnings(quiet(TAM::tam.se(mod)$beta))
    colnames(tmp) <- paste0("imp", i, "_", c("coeff", "se"))
    regr.coeff <- cbind(regr.coeff, tmp)
  }
  pvs[[i]] <- lapply(tmp_pvs, function(x) {
    x[, -which(colnames(x) == "pweights")]
  })
  if (is.null(bgdata)) {
    for (n in 1:npv) {
      names(pvs[[i]][[n]])[which(names(pvs[[i]][[n]]) == "pid")] <- "ID_t"
    }
  }
  colnames(eap[[i]]) <- c("ID_t", "eap", "se")

  list(
    eap = eap, regr.coeff = regr.coeff, pvs = pvs, mod = mod, EAP.rel = EAP.rel
  )
}
