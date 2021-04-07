#' extract correct number of plausible values (npv) from pvs object
#'
#' @param datalist list of data.frames; contains npv PVs and completed bgdata
#' @param regr.coeff list of matrices or matrix; contains latent regression
#' coefficients
#' @param EAP.rel list of vectors or vector; contains EAP reliabilities
#' @param longitudinal logical; whether estimation is longitudinal in design
#' @param info_crit list of matrices or matrix; AIC, BIC
#' @param treeplot list of tree structure plots per imputation
#' @param variable_importance list of matrices with importance statistics for
#' predictor variables per imputation
#'
#' @noRd

discard_not_used_imputations <- function(datalist, regr.coeff, EAP.rel,
                                         longitudinal, info_crit, treeplot,
                                         variable_importance) {
  # names of pvs:
  keep <- determine_used_imputations(datalist)
  # if only one imputation was sampled or no bgdata supplied, exit
  if (length(keep[[1]]) == 1) {
    return(list(regr.coeff = regr.coeff, EAP.rel = EAP.rel,
                info_crit = info_crit, treeplot = treeplot,
                variable_importance = variable_importance))
  }
  # keep only those EAP reliability / regression coefficients with imputations
  if (longitudinal) {
    res <- select_used_imputations_long(EAP.rel, regr.coeff, info_crit,
                                        treeplot, variable_importance, keep[[1]])
  } else {
    res <- select_used_imputations_cross(EAP.rel, regr.coeff, info_crit,
                                         treeplot, variable_importance, keep)
  }
  res
}

determine_used_imputations <- function(datalist) {
  # for list data structures
  keep <- names(datalist)
  keep <- as.numeric(stringr::str_match(keep, "imp\\s*(.*?)\\s*pv")[, 2])
  keep <- unique(sort(keep))

  # for matrix data structures (= regr.coeff)
  keep2 <- names(datalist)
  keep2 <- stringr::str_match(keep2, pattern = "imp[0-9]+")
  keep2 <- unique(sort(keep2))

  list(keep, keep2)
}

select_used_imputations_long <- function(EAP.rel, regr.coeff, info_crit,
                                         treeplot, variable_importance, keep) {
  # list of length nmi of EAP rels for each wave
  EAP.rel <- EAP.rel[keep]
  regr.coeff <- regr.coeff[keep]
  info_crit <- info_crit[keep]
  treeplot <- treeplot[keep]
  variable_importance <- variable_importance[keep]
  names(EAP.rel) <- names(regr.coeff) <- names(info_crit) <- paste0("imp", keep)

  if (!is.null(treeplot)) {
    names(treeplot) <- names(variable_importance) <- paste0("imp", keep)
  }

  list(regr.coeff = regr.coeff, EAP.rel = EAP.rel, info_crit = info_crit,
       treeplot = treeplot, variable_importance = variable_importance)
}

select_used_imputations_cross <- function(EAP.rel, regr.coeff, info_crit,
                                          treeplot, variable_importance, keep) {
  # vector of length nmi
  EAP.rel <- EAP.rel[keep[[1]]]
  treeplot <- treeplot[keep[[1]]]
  variable_importance <- variable_importance[keep[[1]]]
  info_crit <- info_crit[, keep[[1]]]
  names(EAP.rel) <- names(info_crit) <- paste0("imp", keep[[1]])
  regr.coeff <- regr.coeff[, grepl(paste0("Variable|",
                                          paste(keep[[2]], collapse = "|")),
                                   names(regr.coeff))]

  if (!is.null(treeplot)) {
    names(treeplot) <- names(variable_importance) <- paste0("imp", keep[[1]])
  }

  list(regr.coeff = regr.coeff, EAP.rel = EAP.rel, info_crit = info_crit,
       treeplot = treeplot, variable_importance = variable_importance)
}
