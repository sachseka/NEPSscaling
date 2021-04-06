#' model post-processing in cross-sectional estimation:
#' impute pvs, add eaps, eap reliability, regression coefficients to
#' previous estimation results, discard unnecessary data provided by TAM
#' and rename
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
#' @param info_crit list; AIC, BIC
#' @param frmY latent regression formula
#'
#' @noRd

post_process_cross_tam_results <- function(mod, npv, control, imp,
                                           bgdatacom, eap, i,
                                           EAP.rel, regr.coeff, pvs,
                                           info_crit, frmY) {
  # impute plausible values
  tmp_pvs <- impute_pvs(mod, npv, control, bgdata = bgdatacom, imp, "", 1)

  res <- gather_additional_parameters_cross(eap, mod, EAP.rel, regr.coeff,
                                            info_crit, i, bgdata = bgdatacom,
                                            frmY)
  eap <- res$eap
  EAP.rel <- res$EAP.rel
  regr.coeff <- res$regr.coeff
  info_crit <- res$info_crit

  pvs <- reformat_cross_tmp_pvs(pvs, tmp_pvs, bgdata = bgdatacom, npv, i)

  list(eap = eap, regr.coeff = regr.coeff, pvs = pvs, EAP.rel = EAP.rel,
       info_crit = info_crit)
}

reformat_cross_tmp_pvs <- function(pvs, tmp_pvs, bgdata, npv, i) {
  pvs[[i]] <- lapply(tmp_pvs, function(x) {
    x[, -which(colnames(x) == "pweights")]
  })
  if (is.null(bgdata)) {
    for (n in 1:npv) {
      names(pvs[[i]][[n]])[which(names(pvs[[i]][[n]]) == "pid")] <- "ID_t"
    }
  }
  pvs
}

gather_additional_parameters_cross <- function(eap, mod, EAP.rel, regr.coeff,
                                               info_crit, i, bgdata, frmY) {
  eap[[i]] <- suppressWarnings(
    dplyr::left_join(eap[[i]], mod$person[, grep("pid|EAP", names(mod$person))],
                     by = c("ID_t" = "pid"))) %>%
    dplyr::arrange(.data$ID_t)
  colnames(eap[[i]]) <- c("ID_t", "eap", "se")
  EAP.rel <- c(EAP.rel, mod$EAP.rel)
  # se estimation gives warning "In sqrt(-1/info_pp) : NaNs produced" because
  # item difficulty parameters are fixed --> suppress warnings!
  if (i == 1) {
    regr.coeff <- suppressWarnings(quiet(TAM::tam.se(mod)$beta))
    colnames(regr.coeff) <- paste0("imp", i, "_", c("coeff", "se"))
    if (!is.null(bgdata)) {
      rownames(regr.coeff) <- 
        c("Intercept", colnames(model.matrix(frmY, bgdata))[-1])
    } else {
      rownames(regr.coeff) <- "Intercept"
    }
    info_crit <- matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 1)
    rownames(info_crit) <- c("AIC", "BIC")
    colnames(info_crit) <- paste0("imp", i)
  } else if (i > 1) {
    tmp <- suppressWarnings(quiet(TAM::tam.se(mod)$beta))
    colnames(tmp) <- paste0("imp", i, "_", c("coeff", "se"))
    regr.coeff <- cbind(regr.coeff, tmp)
    tmp <- matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 1)
    colnames(tmp) <- paste0("imp", i)
    info_crit <- cbind(info_crit, tmp)
  }
  list(eap = eap, EAP.rel = EAP.rel, regr.coeff = regr.coeff,
       info_crit = info_crit)
}


#' model post-processing in longitudinal estimation:
#' impute pvs, add eaps, eap reliability, regression coefficients to
#' previous estimation results, discard unnecessary data provided by TAM
#' and rename
#'
#' @param mod estimated TAM model
#' @param npv number of plausible values to return
#' @param control control list for TAM functions
#' @param imp imputed background data
#' @param bgdatacom completed background data set
#' @param eap list of eap values
#' @param i current iteration over background data imputations
#' @param j current iteration over assessment waves
#' @param EAP.rel list of eap reliability values
#' @param regr.coeff list of regression coefficients of latent regression
#' @param tmp_bgdata background data (can have fewer columns than bgdata)
#' @param waves character vector; assessment waves ("_wx", "_wy")
#' @param info_crit list; AIC, BIC
#' @param frmY latent regression formula
#'
#' @noRd

post_process_long_tam_results <- function(mod, npv, control, imp,
                                          bgdatacom, eap, i, j,
                                          EAP.rel, regr.coeff, tmp_bgdata,
                                          waves, info_crit, frmY) {
  # impute plausible values
  tmp_pvs <- impute_pvs(mod, npv, control, bgdata = bgdatacom, imp, waves, j)

  res <- gather_additional_parameters_long(eap, mod, EAP.rel, regr.coeff,
                                           info_crit, i, j, waves,
                                           bgdata = tmp_bgdata, frmY)

  list(eap = res$eap, regr.coeff = res$regr.coeff, tmp_pvs = tmp_pvs,
       EAP.rel = res$EAP.rel, info_crit = res$info_crit)
}

gather_additional_parameters_long <- function(eap, mod, EAP.rel, regr.coeff,
                                              info_crit, i, j, waves, bgdata,
                                              frmY) {
  eap[[i]] <- suppressWarnings(
    dplyr::left_join(
      eap[[i]], mod$person[, grep("pid|EAP", names(mod$person))],
      by = c("ID_t" = "pid")
    )
  ) %>% dplyr::arrange(.data$ID_t)
  colnames(eap[[i]])[c(j*2, j*2 + 1)] <- paste0(c("eap", "se"), waves[j])

  tmp <- as.data.frame(suppressWarnings(quiet(TAM::tam.se(mod)$beta))) %>%
    tibble::rownames_to_column()
  names(tmp) <- c("Variable", paste0(c("coeff", "se"), waves[j]))
  # add dummy coded variables for factors with more than 2 levels
  if (!is.null(bgdata)) {
    tmp$Variable <- c("Intercept", colnames(model.matrix(frmY, bgdata))[-1])
  } else {
    tmp$Variable <- "Intercept"
  }
  regr.coeff[[i]] <- dplyr::left_join(regr.coeff[[i]], tmp, by = "Variable")

  if (j == 1) {
    EAP.rel[[i]] <- mod$EAP.rel
    info_crit[[i]] <- matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 1)
    rownames(info_crit[[i]]) <- c("AIC", "BIC")
    colnames(info_crit[[i]]) <- gsub("_", "", waves[j])
  } else {
    EAP.rel[[i]] <- c(EAP.rel[[i]], mod$EAP.rel)
    tmp <- matrix(c(AIC(mod), BIC(mod)), nrow = 2, ncol = 1)
    colnames(tmp) <- gsub("_", "", waves[j])
    info_crit[[i]] <- cbind(info_crit[[i]], tmp)
  }
  list(eap = eap, regr.coeff = regr.coeff, EAP.rel = EAP.rel,
       info_crit = info_crit)
}
