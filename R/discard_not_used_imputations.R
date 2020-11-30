#' extract correct number of plausible values (npv) from pvs object
#'
#' @param datalist list of data.frames; contains npv PVs and completed bgdata
#' @param regr.coeff list of matrices or matrix; contains latent regression 
#' coefficients
#' @param EAP.rel list of vectors or vector; contains EAP reliabilities
#' @param longitudinal logical; whether estimation is longitudinal in design
#'
#' @noRd

discard_not_used_imputations <- function(datalist, regr.coeff, EAP.rel,
                                         longitudinal) {
  # names of pvs:
  keep <- names(datalist)
  keep <- as.numeric(stringr::str_match(keep, "imp\\s*(.*?)\\s*pv")[, 2])
  keep <- unique(sort(keep))
  # if only one imputation was sampled or no bgdata supplied, exit
  if (length(keep) == 1) {
    return(list(regr.coeff = regr.coeff, EAP.rel = EAP.rel))
  }
  # keep only those EAP reliability / regression coefficients with imputations
  if (longitudinal) {
    # list of length nmi of EAP rels for each wave
    EAP.rel <- EAP.rel[keep]
    regr.coeff <- regr.coeff[keep]
    names(EAP.rel) <- names(regr.coeff) <- paste0("imp", keep)
  } else {
    # vector of length nmi
    EAP.rel <- EAP.rel[keep]
    names(EAP.rel) <- paste0("imp", keep)
    # matrix of coefficient + se per imputation: 11 22 33 44 ...
    keep <- 2 * keep
    keep <- sort(c(keep, keep - 1))
    regr.coeff <- regr.coeff[, keep]
  }
  list(regr.coeff = regr.coeff, EAP.rel = EAP.rel)
}
